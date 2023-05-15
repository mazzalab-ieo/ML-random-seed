# what happens when we change the number of folds for a specific number of samples
library(glmnet)
library(caret)
library(reshape2)
library(tidyverse)
library(RColorBrewer)
library(viridis)
library(pROC)
library(ggpubr)

samples_cv<-300
data_cv<-data_list[[11]]
folds<-c(3,5,7,10)

total_res_cv<-data.frame("Seed"=seeds_1000,"Folds"=rep(folds,each=1000))
#total_coef_cv<-list()

lambda_cv<-numeric()
auc_train_cv<-numeric()
auc_test_cv<-numeric()
ncoef_cv<-numeric()

#listcoef_cv<-data.frame("Feature"=colnames(data_list[[i]]))
for (k in folds)
{
  print(k)
  for (j in seeds_1000)
  {
    # for each dataset, separate the train and test set
    set.seed(j)
    
    default_idx = createDataPartition(data_cv$Label, p = 0.75, list = FALSE)
    train = data_cv[default_idx, ]
    test<- data_cv[-default_idx, ]
    
    # scale the two datasets
    train[,-ncol(train)]<-apply(train[,-ncol(train)],2,scale)
    
    test[,-ncol(train)]<-apply(test[,-ncol(test)],2,scale)
    
    #apply method
    cv_lasso<-cv.glmnet(as.matrix(train[,-ncol(train)]), train$Label, family = "binomial", 
                        alpha = 1,type.measure="auc",nfolds =k)
    #compute lambda selected
    lambda_cv<-c(lambda_cv,cv_lasso$lambda.min)
    
    #compute complete model
    model <- glmnet(as.matrix(train[,-ncol(train)]), train$Label, alpha = 1, 
                    family = "binomial",type.measure="auc",
                    lambda = cv_lasso$lambda.min)
    #apply on test set
    prob <- model %>% predict(newx = as.matrix(test[,-ncol(test)]))
    pred<- ifelse(as.numeric(prob) > 0.5, 1, 0)
    
    #coefs
    coef_cv<-as.vector(coef(model))
    #listcoef_cv<-cbind(listcoef_cv,coef_cv)
    #number of coefs different from 0
    ncoef_cv<-c(ncoef_cv,length(which(!(coef_cv==0))))
    
    #auc on train set
    auc_train_cv<-c(auc_train_cv,max(cv_lasso$cvm))
    #auc on test set
    auc_t<-auc(roc(test$Label, pred))
    auc_test_cv<-c(auc_test_cv,auc_t)
  }
}
total_res_cv<-cbind(total_res_cv,lambda_cv,ncoef_cv,auc_train_cv,auc_test_cv)
total_res_cv$Folds<-as.factor(total_res_cv$Folds)

total_res_cv %>%
  ggboxplot(x="Folds",y="lambda_cv",fill="Folds",
  notch=TRUE,
  ggtheme = theme_pubr(base_size=20))+
  stat_compare_means(method = "kruskal.test")

total_res_cv %>%
  ggboxplot(x="Folds",y="auc_test_cv",fill="Folds",
            notch=TRUE,
            ggtheme = theme_pubr(base_size=20))+
  stat_compare_means(method = "kruskal.test")

total_res_cv %>%
  ggboxplot(x="Folds",y="ncoef_cv",fill="Folds",
            notch=TRUE,
            ggtheme = theme_pubr(base_size=20))+
  stat_compare_means(method = "kruskal.test")

