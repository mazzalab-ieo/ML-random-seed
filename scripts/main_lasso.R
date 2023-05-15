
library(MixSim)
library(caret)
library(reshape2)
library(tidyverse)
library(RColorBrewer)
library(viridis)
library(pROC)
library(ggpmisc)

source("~/Desktop/Instability_ML/scripts/create_dataset.R")
source('~/Desktop/Instability_ML/scripts/train_test_split.R')

### built the datasets, from 50 to 500
set.seed(394)
#n<-200
mix_start<-MixSim(BarOmega = 0.1,K = 2, p = 20)

samples<-seq(100,500, by=20)
n_datasets<-length(samples)

data_list<-create_dataset(mix_start,samples)

#seeds
seeds_1000<-seq(1,1000)

#folds
folds<-c(3,5,7,10)

#
lambda<-numeric()
auc_train<-numeric()
auc_test<-numeric()
ncoef<-numeric()

total_res<-data.frame("Seed"=seeds_1000,"N.samples"=rep(samples,each=1000))
total_coef<-list()
for (i in c(1:length(samples)))
{
  print(i)
  #data_list[[i]]$Label<-as.factor(data_list[[i]]$Label)
 # for (k in folds)
  #{
  listcoef<-data.frame("Feature"=colnames(data_list[[i]]))
  
  for (j in seeds_1000)
  {
  # for each dataset, separate the train and test set
  set.seed(j)
  
  default_idx = createDataPartition(data_list[[i]]$Label, p = 0.75, list = FALSE)
  train = data_list[[i]][default_idx, ]
  test<- data_list[[i]][-default_idx, ]
  
  # scale the two datasets
  train[,-ncol(train)]<-apply(train[,-ncol(train)],2,scale)

  test[,-ncol(train)]<-apply(test[,-ncol(test)],2,scale)
  
  #apply method
  cv_lasso<-cv.glmnet(as.matrix(train[,-ncol(train)]), train$Label, family = "binomial", 
                        alpha = 1,type.measure="auc",nfolds =5) #10
  #compute lambda selected
  lambda<-c(lambda,cv_lasso$lambda.min)
  
  #compute complete model
  model <- glmnet(as.matrix(train[,-ncol(train)]), train$Label, alpha = 1, 
                  family = "binomial",type.measure="auc",
                  lambda = cv_lasso$lambda.min)
  #apply on test set
  prob <- model %>% predict(newx = as.matrix(test[,-ncol(test)]))
  pred<- ifelse(as.numeric(prob) > 0.5, 1, 0)
  
  #coefs
  coef<-as.vector(coef(model))
  listcoef<-cbind(listcoef,coef)
  #number of coefs different from 0
  ncoef<-c(ncoef,length(which(!(coef==0))))
  
  #auc on train set
  auc_train<-c(auc_train,max(cv_lasso$cvm))
  #auc on test set
  auc_t<-auc(roc(test$Label, pred))
  auc_test<-c(auc_test,auc_t)
  
  }
  total_coef[[i]]<-listcoef
#}
}
total_res<-cbind(total_res,lambda,ncoef,auc_train,auc_test)

#save data for 300 samples
#write.csv(data_list[[11]],"/Users/ieo4991/Desktop/Instability_ML/datasets/sim_data_300.csv",
#          row.names = FALSE)

#save also seeds
#write.csv(seeds_1000,"/Users/ieo4991/Desktop/Instability_ML/datasets/seeds_1000.csv",
#          row.names = FALSE)

### some analysis
total_res %>% ggplot()+
  geom_boxplot(aes(x=N.samples,y=lambda, group=N.samples))

total_res %>% ggplot()+
  geom_boxplot(aes(x=N.samples,y=auc_test, group=N.samples))+
  labs(y="AUC",title="Distribution of test set AUC")+
  theme_bw(base_size=20)

summary_auc_tot_res <- total_res %>%
  group_by(N.samples) %>%
  summarize(mean=mean(auc_test),sd=sd(auc_test),cv=sd/mean)

model<-lm(summary_auc_tot_res$sd ~ summary_auc_tot_res$N.samples)
fit <- nls(sd ~ SSasymp(N.samples, yf, y0, log_alpha), data = summary_auc_tot_res)
#yf+(y0-yf)e-(exp(log_alpha)*t)

#Nonlinear regression model
# Formula: sd ~ SSasymp(N.samples, yf, y0, log_alpha)
# 
# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
# yf         0.023935   0.002860   8.368 1.28e-07 ***
#   y0         0.113441   0.009691  11.706 7.51e-10 ***
#   log_alpha -5.084330   0.161337 -31.514  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.002769 on 18 degrees of freedom
# 
# Number of iterations to convergence: 0 
# Achieved convergence tolerance: 3.009e-07

#compute prediction for defining se limits
xnew <- data.frame(N.samples=seq(100, 500, by = 10))
p <- predict(fit,newdata=xnew, se = TRUE,interval="confidence")

# plot the estimated mean values of y (fit) at given x values
# over the finer grid of x values;
# superimpose approximate 95% confidence band for the true 
# mean values of y at given x values in the finer grid

# we obtain the standard error of the fit of a nls model using sigma()
g <- data.frame(N.samples= xnew$N.samples, 
                fit = p,
                lwr = p - 1.96*sigma(fit), 
                upr = p + 1.96*sigma(fit))


alpha<-exp(-5.08433)

summary_auc_tot_res %>% ggplot()+
  geom_point(aes(x=N.samples,y=sd),size=3)+
  #geom_line(aes(x=N.samples,y=sd), linewidth=1.1)+
  #geom_smooth(aes(x=N.samples,y=sd),method="lm",formula=(y~exp(-x)),color="blue")+
  geom_line(data=g,aes(x=N.samples,y=0.02394+(0.11344-0.02394)*exp(-(alpha*N.samples))),color="blue", linewidth=1.1)+
  geom_ribbon(data=g,aes(x=N.samples,ymin = lwr, ymax = upr), fill = "grey",alpha=0.4) + 
  labs(y="St.Dev(AUC)",title="Standard deviation of AUC over different sample size",color="Fit")+ #c("y=exp(x^-1)","y=a=b*exp(-cx)")
  theme_bw(base_size=20)

total_res %>% ggplot()+
  geom_violin(aes(x=N.samples,y=ncoef, group=N.samples))

colors<-c(brewer.pal(9, "Set1"),brewer.pal(12, "Set3"))

total_res %>% ggplot()+
  geom_bar(aes(x=N.samples,y=ncoef,fill=as.factor(ncoef)),stat="identity")+
  scale_fill_manual(values=colors)+
  theme_bw(base_size=20)

coef_diff<-total_res %>% 
  group_by(N.samples) %>%
  summarize(diff=max(ncoef)-min(ncoef), sd=sd(ncoef),mean=mean(ncoef),cv=sd/mean,min=min(ncoef)) 

#standard deviation
coef_diff%>%
  ggplot()+
  geom_point(aes(x=N.samples,y=sd, size=diff, fill=diff),shape=21,color="black")+
  #geom_line(aes(x=N.samples,y=sd))+
  geom_smooth(method="lm",aes(x=N.samples,y=sd),color="red", linetype=2,se=TRUE) + #method="loess'
  scale_size(range = c(1, 21), name="Difference")+
  scale_fill_viridis(option="A",name="Difference") +
  theme_bw(base_size=20)+
  labs(y="St.dev(n. features selected)", title="Difference in n. of selected features over 1000 seeds")

#coeffcient of variation
coef_diff%>%
  ggplot()+
  geom_point(aes(x=N.samples,y=cv, size=diff, fill=diff),shape=21,color="black")+
  geom_line(aes(x=N.samples,y=cv))+
  #geom_smooth(method="lm",aes(x=N.samples,y=cv),color="red", linetype=2,se=TRUE) + #method="loess'
  scale_size(range = c(1, 21), name="Difference")+
  scale_fill_viridis(option="A",name="Difference") +
  theme_bw(base_size=20)+
  labs(y="CV(n. features selected)", title="Difference in n. of selected features over 1000 seeds")

# minimum and difference
coef_diff %>%
  ggplot()+
  geom_line(aes(x=N.samples,y=diff),linewidth=1)+
  geom_point(aes(x=N.samples,y=diff, fill=min),shape=21,color="black",size=10)+
  scale_fill_viridis(option="A",name="Minimum") +
  #geom_smooth(aes(x=N.samples,y=diff),method="lm",formula=(y~exp(x^(-1))))+
  geom_text(aes(N.samples,y=diff, label = min), colour = ifelse(coef_diff$min<13,"white","black"), size = 5 )+
  labs(y="Span",title="Span of number of feature selected")+
  theme_bw(base_size=20)
  
#cambia il nome delle righe per coef: sono intercept...v20

#check the heatmap per i coefficienti  n=300
colnames(total_coef[[11]])<-c("Feature",paste("Seed",seeds_1000,sep="_"))
rownames(total_coef[[11]])<-c("Intercept",paste("V",seq(1,20),sep=""))

heatmap(as.matrix(t(total_coef[[11]][,-1])),Rowv = NA, Colv = NA,
        col=rev(colorRampPalette(brewer.pal(11, "RdBu"))(25)))
  
