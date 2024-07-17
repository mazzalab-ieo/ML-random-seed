##### main of models. I will use 100 seeds instead of 1000 to reduce time
library(tidyverse)
library(RColorBrewer)
library(ggpubr)

library(doParallel)
library(foreach)

load("~/ML-random-seed/data/datasets_140224.RData")
source("~/ML-random-seed/scripts/lasso_func.R")
source("~/ML-random-seed/scripts/svm_func.R")
source("~/ML-random-seed/scripts/rf_func.R")

seeds_100<-seq(1,100)
ndata<-seq(1,length(dataSim))

#lasso
numCores <- detectCores()
numCores
cl<-makePSOCKcluster(numCores)
registerDoParallel(cl)

par_lasso<-foreach (i=seq_along(ndata), .combine = "c", #seq_along(ndata)
                 .packages =c("caret","tidyverse","pROC")) %dopar%
  {
    cat(paste("Starting iteration ",i,"\n"))
    cn<-which(colnames(dataSim[[i]])=="Dataset")
    list(lasso_func(seeds_100,dataSim[i],torem=cn,case=i))
  }
stopCluster(cl)

# fix data
res_lasso<-list()
res_lasso[[1]]<-par_lasso[[1]][[1]]
res_lasso[[2]]<-list()
res_lasso[[2]][[1]]<-par_lasso[[1]][[2]][[1]]

for (i in seq(2,length(par_lasso)))
{
  print(i)
  res_lasso[[1]]<-rbind(res_lasso[[1]],par_lasso[[i]][[1]])
  res_lasso[[2]][[i]]<-par_lasso[[i]][[2]][[1]]
}

res_lasso[[1]]<-left_join(res_lasso[[1]],info_data[,-c(1,3)],by="Dataset")
########### svm
library(plyr)

numCores <- detectCores()
numCores
cl<-makePSOCKcluster(numCores)
registerDoParallel(cl)

par_svm<-foreach (i=seq_along(ndata), .combine = "c", #.export=c("svm_func"), 
               .packages =c("caret","tidyverse","pROC")) %dopar%
  {
    #cat(paste("Starting iteration ",i,"\n"))
    cn<-which(colnames(dataSim[[i]])=="Dataset")
    list(svm_func(seeds_100,dataSim[i],torem=cn,case=i))
  }
stopCluster(cl)

# fix data
res_svm<-list()
res_svm[[1]]<-par_svm[[1]][[1]]
res_svm[[2]]<-list()
res_svm[[2]][[1]]<-par_svm[[1]][[2]][[1]]

for (i in seq(2,length(par_svm)))
{
  print(i)
  res_svm[[1]]<-rbind(res_svm[[1]],par_svm[[i]][[1]])
  res_svm[[2]][[i]]<-par_svm[[i]][[2]][[1]]
}

res_svm[[1]]<-left_join(res_svm[[1]],info_data[,-c(1,3)],by="Dataset")

# #rf
cl<-makePSOCKcluster(numCores)
registerDoParallel(cl)

par_rf<-foreach (i=seq_along(ndata), .combine = "c", #.export=c("svm_func"),
                 .packages =c("caret","tidyverse","pROC","randomForest")) %dopar%
  {
    #cn<-which(colnames(dataSim[[i]])=="Dataset") #to use as torem in case we have the Dataset column
    list(rf_func(seeds_100,dataSim[i],torem=0,case=i))
  }

stopCluster(cl)
# 
# # fix data
res_rf<-list()
res_rf[[1]]<-par_rf[[1]][[1]]
res_rf[[2]]<-list()
res_rf[[2]][[1]]<-par_rf[[1]][[2]][[1]]

for (i in seq(2,length(par_rf)))
{
  print(i)
  res_rf[[1]]<-rbind(res_rf[[1]],par_rf[[i]][[1]])
  res_rf[[2]][[i]]<-par_rf[[i]][[2]][[1]]
}

res_rf[[1]]<-left_join(res_rf[[1]],info_data[,-c(1,3)],by="Dataset")
#res_rf_c[[1]]$Model<-"RF"

#####analysis: probably I will consider the IQR
### LASSO
summary_lasso <-res_lasso[[1]] %>%
  group_by(Dataset,`N.samples`,nFeatures,Perc0) %>%
  summarise(mean=mean(auc_test),sd=sd(auc_test),cv=sd/mean,iqr=IQR(auc_test)) 

summary_lasso <-left_join(summary_lasso,ds_complexity_all[,c("Dataset",
"overlapping_my.F4","neighborhood_my.N1","neighborhood_my.N3")],by="Dataset")
colnames(summary_lasso)[c(9,10,11)]<-c("F4","N1","N3")
summary_lasso$MeanCorr<-mean_corr
  
cor(summary_lasso %>% ungroup() %>% select(N.samples,nFeatures,Perc0,F4,N1,MeanCorr))

###### SVM
summary_svm <-res_svm[[1]] %>%
  group_by(Dataset,N.samples,nFeatures,Perc0) %>%
  summarise(mean=mean(auc_test),sd=sd(auc_test),cv=sd/mean,iqr=IQR(auc_test)) 

summary_svm <-left_join(summary_svm,
                        ds_complexity_all[,c("Dataset","overlapping_my.F4",
                                             "neighborhood_my.N1","neighborhood_my.N3")],by="Dataset")
colnames(summary_svm)[c(9,10,11)]<-c("F4","N1","N3")
summary_svm$MeanCorr<-mean_corr

##### RF
summary_rf <-res_rf[[1]] %>%
  group_by(Dataset,N.samples,nFeatures,Perc0) %>%
  summarise(mean=mean(auc_test),sd=sd(auc_test),cv=sd/mean,iqr=IQR(auc_test)) 

summary_rf <-left_join(summary_rf,
                        ds_complexity_all[,c("Dataset","overlapping_my.F4",
                                             "neighborhood_my.N1","neighborhood_my.N3")],by="Dataset")
colnames(summary_rf)[c(9,10,11)]<-c("F4","N1","N3")

summary_rf$MeanCorr<-mean_corr

cor(summary_rf %>% ungroup() %>% select(N.samples,nFeatures,Perc0,F4,N1,MeanCorr))

#### plots
res_lasso[[1]] %>% 
  ggplot() +
  geom_boxplot(aes(x = N.samples, y = auc_test,group=as.factor(N.samples))) +
  facet_wrap(~Overlap,scales = "free_x")+
  labs(x="N.samples",y="AUC test set",title="Lasso")+
  theme_bw(base_size=20)

res_svm[[1]] %>% 
  ggplot() +
  geom_boxplot(aes(x = N.samples, y = auc_test,group=as.factor(N.samples))) +
  facet_wrap(~Overlap,scales = "free_x")+
  labs(x="N.samples",y="AUC test set",title="SVM")+
  theme_bw(base_size=20)

res_rf[[1]] %>% 
  ggplot() +
  geom_boxplot(aes(x = N.samples, y = auc_test,group=as.factor(N.samples))) +
  facet_wrap(~Overlap,scales = "free_x")+
  labs(x="N.samples",y="AUC test set",title="Random Forest")+
  theme_bw(base_size=20)

summary_lasso %>%
  ungroup() %>%
  dplyr::select(sd,N.samples,nFeatures,Perc0,MeanCorr,F4,N1,N3) %>%
  gather(-sd, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = sd)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ var, scales = "free") +
  labs(x="",y="st.dev(AUC test set)",title="Lasso")+
  theme_bw(base_size=20)

summary_svm %>%
  ungroup() %>%
  dplyr::select(sd,N.samples,nFeatures,Perc0,MeanCorr,F4,N1,N3) %>%
  gather(-sd, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = sd)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ var, scales = "free") +
  labs(x="",y="st.dev(AUC test set)",title="SVM")+
  theme_bw(base_size=20)

summary_rf %>%
  ungroup() %>%
  dplyr::select(sd,N.samples,nFeatures,Perc0,MeanCorr,F4,N1,N3) %>%
  gather(-sd, key = "var", value = "value") %>%
  ggplot(aes(x = value, y = sd)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ var, scales = "free") +
  labs(x="",y="st.dev(AUC test set)",title="Random Forest")+
  theme_bw(base_size=20)

all_summary<-left_join(summary_lasso,summary_svm[,c("Dataset","sd")],by="Dataset")
colnames(all_summary)[c(6,13)]<-c("sd_lasso","sd_svm")
all_summary<-left_join(all_summary,summary_rf[,c("Dataset","sd")],by="Dataset")
colnames(all_summary)[14]<-"sd_rf"

all_summary %>% 
  ungroup() %>%
  dplyr::select(Dataset,sd_lasso,sd_rf,sd_svm) %>%
  gather(-Dataset, key = "Model", value = "sd") %>%
  ggplot()+
  geom_violin(aes(x=Model, fill=Model,y=sd))+
  scale_x_discrete(labels=c("Lasso","RF","SVM"))+
  scale_fill_brewer(palette = "Set1")+
  guides(fill=FALSE)+
  theme_bw(base_size=20)



