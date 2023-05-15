### try with radiomics data, both simulated and real ones
library(tidyverse)
library(infotheo)
library(glmnet)
library(caret)
library(pROC)
library(EDMeasure)
library(RColorBrewer)

source("~/Desktop/Instability_ML/scripts/svm_func.R")
source("~/Desktop/Instability_ML/scripts/rf_func.R")

# read data
data_0_fbp_unbalanced <- read.csv("~/Desktop/Instability_ML/datasets/res_fbp_0_280_noise_unbalanced_3feat_20201118.txt", sep="")
data_1_fbp_unbalanced <- read.csv("~/Desktop/Instability_ML/datasets/res_fbp_1_120_noise_unbalanced_3feat_20201118.txt", sep="")
data_0_ir_unbalanced <- read.csv("~/Desktop/Instability_ML/datasets/res_ir_0_140_noise_unbalanced_3feat_20201118.txt", sep="")
data_1_ir_unbalanced <- read.csv("~/Desktop/Instability_ML/datasets/res_ir_1_60_noise_unbalanced_3feat_20201118.txt", sep="")

#add label
data_0_fbp_unbalanced$Label<-0
data_1_fbp_unbalanced$Label<-1
data_0_ir_unbalanced$Label<-0
data_1_ir_unbalanced$Label<-1

#large case: all the data
data_sim_large<-rbind(data_0_fbp_unbalanced,data_0_ir_unbalanced,data_1_fbp_unbalanced,data_1_ir_unbalanced)
data_sim_large$Sample<-seq(1,nrow(data_sim_large))

#small sample
set.seed(5)
case_0_fbp<-sample(seq(1, nrow(data_0_fbp_unbalanced)),47)
case_1_fbp<-sample(seq(1, nrow(data_1_fbp_unbalanced)),20)
case_0_ir<-sample(seq(1, nrow(data_0_ir_unbalanced)),23)
case_1_ir<-sample(seq(1, nrow(data_1_ir_unbalanced)),10)

data_sim_small<-rbind(data_0_fbp_unbalanced[case_0_fbp,],data_1_fbp_unbalanced[case_1_fbp,],
                      data_0_ir_unbalanced[case_0_ir,],data_1_ir_unbalanced[case_1_ir,])
data_sim_small$Sample<-c(case_0_fbp,case_1_fbp,case_0_ir,case_1_ir)

#medium sample
case_0_fbp<-sample(seq(1, nrow(data_0_fbp_unbalanced)),140)
case_1_fbp<-sample(seq(1, nrow(data_1_fbp_unbalanced)),60)
case_0_ir<-sample(seq(1, nrow(data_0_ir_unbalanced)),70)
case_1_ir<-sample(seq(1, nrow(data_1_ir_unbalanced)),30)

data_sim_med<-rbind(data_0_fbp_unbalanced[case_0_fbp,],data_1_fbp_unbalanced[case_1_fbp,],
                    data_0_ir_unbalanced[case_0_ir,],data_1_ir_unbalanced[case_1_ir,])
data_sim_med$Sample<-c(case_0_fbp,case_1_fbp,case_0_ir,case_1_ir)

# put all together
data_sim_list<-list(data_sim_small,data_sim_med,data_sim_large)

# compute mdm for the datasets: R crashes
#mdm_rad<-numeric()
#for (i in c(1:length(samples)))
#  mdm_rad<-c(mdm_rad,mdm(data_sim_list[[i]][,-c(169,170)])[[1]])
  
mean_corr_rad<-as.numeric(lapply(data_sim_list,function(x) mean(abs(cor(x[,-c(169,170)])))))

#### compute the score based on correlation
score_rad<-as.numeric(lapply(data_sim_list, function(x) Score_correlation(x,c(169,170),0.4)[[1]]))
score_rad_matrix<-lapply(data_sim_list, function(x) Score_correlation(x,c(169,170),0.4)[[2]])

# check distribution of score_correlation for the 3 cases
score_dist<- score_rad_matrix[[1]] %>%
  select(Feature_1,Feature_2,points) %>%
  mutate(points_300=score_rad_matrix[[2]]$points, points_600=score_rad_matrix[[3]]$points)
  
colnames(score_dist)[3]<-"points_100"

score_dist<-score_dist %>% gather(sample, points,points_100:points_600)

score_dist %>% #filter(sample=="points_100") %>% 
  ggplot()+
  geom_bar(aes(x=points,fill=sample),position="dodge")+
  theme_bw(base_size = 20)+
  scale_fill_manual(values=colors[c(1,2,3)],labels=c("100","300","600"))+
  labs(x="Score",y="Feature couples",title="Distribution of feature score",fill="Case")

#seeds
seeds_1000<-seq(1,1000)

## apply log reg
lambda<-numeric()
auc_train<-numeric()
auc_test<-numeric()
ncoef<-numeric()

total_res<-data.frame("Seed"=seeds_1000,"N.samples"=rep(c(100,300,600),each=1000))
total_coef<-list()

for (i in c(1:length(data_sim_list)))
{
  print(i)
  listcoef<-data.frame("Feature"=colnames(data_sim_list[[i]][,-c(169,170)]))
  
  pb <- txtProgressBar(0, 1000, style = 3)
  
  for (j in seeds_1000)
  {
    setTxtProgressBar(pb, j)
    # for each dataset, separate the train and test set
    set.seed(j)
    
    default_idx = createDataPartition(as.factor(data_sim_list[[i]]$Label), p = 0.75, list = FALSE)
    train <-  data_sim_list[[i]][default_idx, ]
    test<- data_sim_list[[i]][-default_idx, ]
    
    # scale the two datasets
    train[,-c(169,170)]<-apply(train[,-c(169,170)],2,scale)
    
    test[,-c(169,170)]<-apply(test[,-c(169,170)],2,scale)
    
    #apply method
    cv_lasso<-cv.glmnet(as.matrix(train[,-c(169,170)]), train$Label, family = "binomial", 
                        alpha = 1,type.measure="auc",nfolds =5) #10
    #compute lambda selected
    lambda<-c(lambda,cv_lasso$lambda.min)
    
    #compute complete model
    model <- glmnet(as.matrix(train[,-c(169,170)]), train$Label, alpha = 1, 
                    family = "binomial",type.measure="auc",
                    lambda = cv_lasso$lambda.min)
    #apply on test set
    prob <- model %>% predict(newx = as.matrix(test[,-c(169,170)]))
    pred<- ifelse(as.numeric(prob) > 0.5, 1, 0)
    
    #coefs
    coef<-as.vector(coef(model))
    listcoef<-cbind(listcoef,coef[-1])
    #number of coefs different from 0
    ncoef<-c(ncoef,length(which(!(coef[-1]==0))))
    
    #auc on train set
    auc_train<-c(auc_train,max(cv_lasso$cvm))
    #auc on test set
    auc_t<-auc(roc(test$Label, pred,quiet=TRUE))
    auc_test<-c(auc_test,auc_t)
    
  }
  total_coef[[i]]<-listcoef
  #}
}
close(pb)

total_res<-cbind(total_res,lambda,ncoef,auc_train,auc_test)

total_res %>% ggplot()+
  geom_boxplot(aes(x=as.factor(N.samples),y=auc_test, group=as.factor(N.samples)))+
  geom_point(data=total_res %>% filter(Seed<11),aes(x=as.factor(N.samples),y=auc_test,colour=as.factor(Seed)), size=2, alpha=0.5) +
  geom_line(data=total_res %>% filter(Seed<11),aes(x=as.factor(N.samples),y=auc_test,group=as.factor(Seed),color=as.factor(Seed)), linetype="11") +
  labs(x="N.samples",y="AUC",title="Simulated radiomics datasets, test set AUC",color="Seed")+
  theme_bw(base_size=20)

summary_auc_tot_res <- total_res %>%
  group_by(N.samples) %>%
  summarize(mean=mean(auc_test),sd=sd(auc_test),cv=sd/mean,
            span_feat=max(ncoef)-min(ncoef),min_feat=min(ncoef),max_feat=max(ncoef))

colnames(total_coef[[1]])<-c("Feature",paste("Seed_",seq(1,1000),sep=""))
colnames(total_coef[[2]])<-c("Feature",paste("Seed_",seq(1,1000),sep=""))
colnames(total_coef[[3]])<-c("Feature",paste("Seed_",seq(1,1000),sep=""))

total_coef1<-total_coef[[1]] %>% gather("Seed","coef",-Feature)
total_coef1<-total_coef1 %>% mutate(Case="Small")

total_coef2<-total_coef[[2]] %>% gather("Seed","coef",-Feature)
total_coef2<-total_coef2 %>% mutate(Case="Medium")

total_coef3<-total_coef[[3]] %>% gather("Seed","coef",-Feature)
total_coef3<-total_coef3 %>% mutate(Case="Large")

feature<-rbind(total_coef1,total_coef2,total_coef3)

selection<-feature %>% group_by(Case,Feature) %>% 
  summarize(selected=length(which(!(coef==0))),percent=(selected/1000)*100)

selection %>%  
  filter(percent>80) %>%
  arrange(percent) %>%
  mutate(Feature=factor(Feature, Feature)) %>%
  ggplot()+
  geom_segment(aes(x=Feature, xend=Feature,y=0, yend= percent, color=Case)) +
  geom_point(aes(x=Feature, y=percent,color=Case),size=4)+
  coord_flip()+
  facet_wrap(~Case, ncol=1, scale="free_y")+
  theme_minimal(base_size=20)+
  theme(legend.position = "none",
           panel.border = element_blank(),
           panel.spacing = unit(0.1, "lines"))
  
##### svm linear
samples<-as.numeric(lapply(data_sim_list,nrow))
torem<-which(colnames(data_sim_small)=="Sample")
res_svm<-svm_func(seeds_1000,samples,data_sim_list,torem)

tot_res_svm<-res_svm[[1]]
#tot_res_svm$parameter<-as.numeric(res_svm[[1]][1,-c(1,2,3003,3004,3005)])

tot_res_svm %>% ggplot()+
  geom_boxplot(aes(x=as.factor(N.samples),y=auc_test, group=as.factor(N.samples)))+
  labs(x="N.samples",y="AUC",title="Simulated radiomics datasets, SVM test set AUC",color="Seed")+
  theme_bw(base_size=20)

res_svm[[2]]

### rf
#res_rf<-rf_func(seeds_1000,data_sim_list,torem)

##### rf in parallel
library(plyr)
library(doParallel)
library(foreach)

numCores <- detectCores()
numCores
cl<-makePSOCKcluster(numCores)
registerDoParallel(cl)

rf_res<-foreach (i=seq_along(samples), .combine = "c", #.export=c("svm_func"), 
               .packages =c("caret","tidyverse","pROC","randomForest")) %dopar%
  {
    #cat(paste("Starting iteration ",i,"\n"))
    list(rf_func(seeds_1000,data_sim_list[i],torem))
  }
stopCluster(cl)

# fix data
res_rf<-list()
res_rf[[1]]<-rf_res[[1]][[1]]
res_rf[[2]]<-list()
res_rf[[2]][[1]]<-rf_res[[1]][[2]][[1]]

for (i in seq(2,length(rf_res)))
{
  print(i)
  res_rf[[1]]<-rbind(res_rf[[1]],rf_res[[i]][[1]])
  res_rf[[2]][[i]]<-rf_res[[i]][[2]][[1]]
}


res_rf[[1]] %>% ggplot()+
  geom_boxplot(aes(x=as.factor(N.samples),y=auc_test, group=as.factor(N.samples)))+
  labs(x="N.samples",y="AUC",title="Simulated radiomics datasets, RF test set AUC",color="Seed")+
  theme_bw(base_size=20)

### out results altogether
res_all_radiomics<-rbind(total_res[,-3],res_svm[[1]][,-3],res_rf[[1]][,-3])
res_all_radiomics$Model<-rep(c("Lasso","SVM","RF"),each=3000)

res_all_rad_summary<- res_all_radiomics %>%
  group_by(Model,N.samples) %>%
  summarize(mean=mean(auc_test),sd=sd(auc_test),cv=sd/mean)

res_all_rad_summary$MeanCorr<-rep(mean_corr_rad,3)

res_all_rad_summary$Score_corr<-rep(score_rad,3)
#lets see if our model works!
#interval prediction: 95% of our samples has value in that interval
pred_rad<-as.data.frame(predict.lm(model_sc,res_all_rad_summary,interval="prediction")) #se.fit=TRUE, 
combo<-cbind(res_all_rad_summary,pred_rad)

colors<-brewer.pal(5,"Set1")
combo%>% ggplot() +
  geom_point( aes(x=as.factor(N.samples), y=sd, color="True"), size=3 ) +
  geom_point( aes(x=as.factor(N.samples), y=fit,color="Fit"), size=3 ) +
  scale_color_manual(values=colors[c(2,3)])+
  geom_errorbar( aes(x=as.factor(N.samples), ymin=lwr, ymax=upr), color =colors[2], linetype = "dashed",width=0.4)+
  facet_wrap(~Model)+
  labs(x="N.samples",y="St.Dev",color="Case",title="Fit for radiomics")+
  theme_bw(base_size=20)



