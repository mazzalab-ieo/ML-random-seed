# what happens when we change the random generator method for a specific number of samples
library(glmnet)
library(caret)
library(reshape2)
library(tidyverse)
library(RColorBrewer)
library(viridis)
library(pROC)
library(ggpubr)

#samples_cv<-300
#data_cv<-data_list[[11]]
methods<-c("Mersenne-Twister","Wichmann-Hill","Super-Duper","Marsaglia-Multicarry",
           "Knuth-TAOCP-2002","Knuth-TAOCP","L'Ecuyer-CMRG")

res_method<-list()

total_res$Method="Mersenne-Twister"

res_method[[1]]<-total_res

for (m in c(2:length(methods)))
{
  print(methods[m])
  
  lambda_met<-numeric()
  auc_train_met<-numeric()
  auc_test_met<-numeric()
  ncoef_met<-numeric()
  
  res_method[[m]]<-data.frame("Seed"=seeds_1000,"N.samples"=rep(samples,each=1000),"Method"=rep(methods[m],each=length(seeds_1000)*length(samples)))
  
  pb <- txtProgressBar(0, length(samples), style = 3)
  for (i in c(1:length(samples)))
  {
    #print(i)
    setTxtProgressBar(pb, i)
    
    for (j in seeds_1000)
    {
      # for each dataset, separate the train and test set
      set.seed(j,kind =methods[m])
      
      default_idx <- createDataPartition(data_list[[i]]$Label, p = 0.75, list = FALSE)
      train <- data_list[[i]][default_idx,]
      test<- data_list[[i]][-default_idx,]
      
      # scale the two datasets
      train[,-ncol(train)]<-apply(train[,-ncol(train)],2,scale)
      
      test[,-ncol(train)]<-apply(test[,-ncol(test)],2,scale)
      
      #apply method
      cv_lasso<-cv.glmnet(as.matrix(train[,-ncol(train)]), train$Label, family = "binomial", 
                          alpha = 1,type.measure="auc",nfolds =5)
      #compute lambda selected
      lambda_met<-c(lambda_met,cv_lasso$lambda.min)
      
      #compute complete model
      model <- glmnet(as.matrix(train[,-ncol(train)]), train$Label, alpha = 1, 
                      family = "binomial",type.measure="auc",
                      lambda = cv_lasso$lambda.min)
      #apply on test set
      prob <- model %>% predict(newx = as.matrix(test[,-ncol(test)]))
      pred<- ifelse(as.numeric(prob) > 0.5, 1, 0)
      
      #coefs
      coef_met<-as.vector(coef(model))
      #number of coefs different from 0
      ncoef_met<-c(ncoef_met,length(which(!(coef_met==0))))
      
      #auc on train set
      auc_train_met<-c(auc_train_met,max(cv_lasso$cvm))
      #auc on test set
      auc_t<-auc(roc(test$Label, pred, quiet=TRUE))
      auc_test_met<-c(auc_test_met,auc_t)
    }
  }
  close(pb)
  res_method[[m]]<-cbind(res_method[[m]],lambda_met,ncoef_met,auc_train_met,auc_test_met)
}

colnames(res_method[[1]])<-c("Seed","N.samples","lambda_met","ncoef_met",
                             "auc_train_met","auc_test_met","Method")
res_method[[1]]<-res_method[[1]][,colnames(res_method[[2]])]

total_res_met<-rbind(res_method[[1]],res_method[[2]],
                     res_method[[3]],res_method[[4]],
                     res_method[[5]],res_method[[6]],res_method[[7]])

total_res_met_summary <-total_res_met %>%
  group_by(Method,N.samples) %>%
  summarise(mean_auc_test=mean(auc_test_met),sd=sd(auc_test_met))
  
total_res_met %>%
  ggboxplot(x="N.samples",y="auc_test_met",fill="Method",
            facet="Method",
            notch=TRUE,
            ggtheme = theme_pubr(base_size=20))

total_res_met_summary %>% ggplot()+
  geom_point(aes(x=N.samples,y=sd,color=Method))+
  geom_line(aes(x=N.samples,y=sd,color=Method))+
  theme_bw(base_size=20)

#####compare Python and R
res_python_300 <- read.csv("~/Desktop/Instability_ML/res_python_300.csv")
res_python_300$Who<-"Python"
res_python_300$lambda=1/res_python_300$inv_lambda

res_r_300<-total_res %>% filter(N.samples==300) %>%
  select(-N.samples) %>% mutate(Who="R")

res_python_300<-res_python_300[,colnames(res_r_300)]

res_300_all<-rbind(res_r_300,res_python_300)

res_300_all %>%
  #filter(lambda)  %>%
  ggviolin(x="Who", y="auc_test",fill="Who",
           palette = c("#00AFBB", "#E7B800"),add="boxplot",add.params = list(fill = "white"),
          ggtheme = theme_pubr(base_size=20), title="Test AUC in R and Python")+
  stat_compare_means(method = "wilcox.test",paired=TRUE)

res_300_all %>%
  ggpaired(x="Who",y = "ncoef",fill="Who",
            palette = c("#00AFBB", "#E7B800"),
           ggtheme = theme_pubr(base_size=20))+
  stat_compare_means(method = "wilcox.test",paired=TRUE)

res_300_all %>% group_by(Who) %>%
  summarize(mean_=mean(auc_test))
