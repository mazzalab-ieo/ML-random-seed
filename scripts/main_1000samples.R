### main for data with large number of samples and coming from same big dataset
library(tidyverse)
library(RColorBrewer)
library(ggpubr)

#load data with 1000 samples and 20 features
load("~/Desktop/Instability_ML/datasets_150223_20feat.RData")
source("~/Desktop/Instability_ML/scripts/lasso_func.R")
source("~/Desktop/Instability_ML/scripts/svm_func.R")
source("~/Desktop/Instability_ML/scripts/rf_func.R")

seeds_1000<-seq(1,1000)
samples<-seq(60,1000, by=20)

res_2_lasso<-lasso_func(seeds_1000,samples,data_list_2)
res_2_lasso[[1]]$Model<-"Lasso"


##### svm in parallel
library(plyr)
library(doParallel)
library(foreach)

numCores <- detectCores()
numCores
cl<-makePSOCKcluster(numCores)
registerDoParallel(cl)

r_p1<-foreach (i=seq_along(samples), .combine = "c", #.export=c("svm_func"), 
               .packages =c("caret","tidyverse","pROC")) %dopar%
  {
    #cat(paste("Starting iteration ",i,"\n"))
    list(svm_func(seeds_1000,data_list_2[i],torem=0))
  }
stopCluster(cl)

# fix data
res_2_svm<-list()
res_2_svm[[1]]<-r_p1[[1]][[1]]
res_2_svm[[2]]<-list()
res_2_svm[[2]][[1]]<-r_p1[[1]][[2]][[1]]

for (i in seq(2,length(r_p1)))
{
  print(i)
  res_2_svm[[1]]<-rbind(res_2_svm[[1]],r_p1[[i]][[1]])
  res_2_svm[[2]][[i]]<-r_p1[[i]][[2]][[1]]
}

detach(package:plyr)

res_2_svm[[1]]$Model<-"SVM"

##### rf in parallel
rf_1<-foreach (i=seq_along(samples), .combine = "c", #.export=c("svm_func"), 
               .packages =c("caret","tidyverse","pROC")) %dopar%
  {
    list(rf_func(seeds_1000,data_list_2[i],torem=0))
  }

stopCluster(cl)

# fix data
res_2_rf<-list()
res_2_rf[[1]]<-rf_1[[1]][[1]]
res_2_rf[[2]]<-list()
res_2_rf[[2]][[1]]<-rf_1[[1]][[2]][[1]]

for (i in seq(2,length(rf_1)))
{
  print(i)
  res_2_rf[[1]]<-rbind(res_2_rf[[1]],rf_1[[i]][[1]])
  res_2_rf[[2]][[i]]<-rf_1[[i]][[2]][[1]]
}

detach(package:plyr)

res_2_rf[[1]]$Model<-"RF"

### some analysis: lasso
res_2_lasso[[1]] %>% ggplot()+
  geom_boxplot(aes(x=N.samples,y=lambda, group=N.samples))

res_2_lasso[[1]]%>% ggplot()+
  geom_boxplot(aes(x=N.samples,y=auc_test, group=N.samples))+
  labs(y="AUC",title="Distribution of test set AUC")+
  theme_bw(base_size=20)

summary_auc_lasso <- res_2_lasso[[1]] %>%
  group_by(N.samples) %>%
  summarize(mean=mean(auc_test),sd=sd(auc_test),cv=sd/mean)

summary_auc_lasso %>% ggplot()+
  geom_point(aes(x=N.samples,y=sd),size=3)+
  geom_line(aes(x=N.samples,y=sd), linewidth=1.1)+
  #geom_smooth(aes(x=N.samples,y=sd),method="lm",formula=(y~exp(-x)),color="blue")+
  #geom_line(data=g,aes(x=N.samples,y=0.02394+(0.11344-0.02394)*exp(-(alpha*N.samples))),color="blue", linewidth=1.1)+
  #geom_ribbon(data=g,aes(x=N.samples,ymin = lwr, ymax = upr), fill = "grey",alpha=0.4) + 
  labs(y="St.Dev(AUC)",title="Standard deviation of AUC over different sample size",color="Fit")+ #c("y=exp(x^-1)","y=a=b*exp(-cx)")
  theme_bw(base_size=20)

## some analysis: svm
res_2_svm[[1]]%>% ggplot()+
  geom_boxplot(aes(x=N.samples,y=auc_test, group=N.samples))+
  labs(y="AUC",title="Distribution of SVM test set AUC")+
  theme_bw(base_size=20)

summary_auc_svm <- res_2_svm[[1]] %>%
  group_by(N.samples) %>%
  summarize(mean=mean(auc_test),sd=sd(auc_test),cv=sd/mean)

summary_auc_svm %>% ggplot()+
  geom_point(aes(x=N.samples,y=sd),size=3)+
  geom_line(aes(x=N.samples,y=sd), linewidth=1.1)+
  #geom_smooth(aes(x=N.samples,y=sd),method="lm",formula=(y~exp(-x)),color="blue")+
  #geom_line(data=g,aes(x=N.samples,y=0.02394+(0.11344-0.02394)*exp(-(alpha*N.samples))),color="blue", linewidth=1.1)+
  #geom_ribbon(data=g,aes(x=N.samples,ymin = lwr, ymax = upr), fill = "grey",alpha=0.4) + 
  labs(y="St.Dev(AUC)",title="StDev of AUC over different sample size, SVM",color="Fit")+ #c("y=exp(x^-1)","y=a=b*exp(-cx)")
  theme_bw(base_size=20)

## some analysis: rf
res_2_rf[[1]]%>% ggplot()+
  geom_boxplot(aes(x=N.samples,y=auc_test, group=N.samples))+
  labs(y="AUC",title="Distribution of RF test set AUC")+
  theme_bw(base_size=20)

summary_auc_rf <- res_2_rf[[1]] %>%
  group_by(N.samples) %>%
  summarize(mean=mean(auc_test),sd=sd(auc_test),cv=sd/mean)

summary_auc_rf %>% ggplot()+
  geom_point(aes(x=N.samples,y=sd),size=3)+
  geom_line(aes(x=N.samples,y=sd), linewidth=1.1)+
  labs(y="St.Dev(AUC)",title="StDev of AUC over different sample size, RF",color="Fit")+ #c("y=exp(x^-1)","y=a=b*exp(-cx)")
  theme_bw(base_size=20)

#### all models
res_all<-rbind(res_2_lasso[[1]][,-3],res_2_svm[[1]][,-3],res_2_rf[[1]][,-3])

summary_res<- res_all %>%
  group_by(Model,N.samples) %>%
  summarize(mean=mean(auc_test),sd=sd(auc_test),cv=sd/mean)

## add average correlation, interaction information and mdm as variables
summary_res$MeanCorr<-rep(mean_corr_2,3)
#interaction information is quite long to compute and it crashes for big data
#summary_res$IntInf<-rep(int_inf_2,3)
summary_res$mdm<-rep(mdm_2,3)

summary_res$Score_corr<-rep(sc_2,3)


####plots
summary_res %>% ggplot()+
  geom_point(aes(x=N.samples,y=sd,color=Model),size=3)+
  #geom_line(aes(x=N.samples,y=sd,color=Model), linewidth=1.1)+
  scale_color_brewer(palette="Set1")+
  geom_smooth(aes(x=N.samples,y=sd,group=Model,color=Model))+
  #geom_line(data=g,aes(x=N.samples,y=0.02394+(0.11344-0.02394)*exp(-(alpha*N.samples))),color="blue", linewidth=1.1)+
  #geom_ribbon(data=g,aes(x=N.samples,ymin = lwr, ymax = upr), fill = "grey",alpha=0.4) + 
  labs(y="St.Dev(AUC)",title="StDev of test AUC, increasing sample size")+ #c("y=exp(x^-1)","y=a=b*exp(-cx)")
  theme_bw(base_size=20)

summary_res %>% ggplot()+
  geom_point(aes(x=MeanCorr,y=sd,color=Model),size=3)+
  #geom_line(aes(x=IntInf,y=sd,color=Model), linewidth=1.1)+
  scale_color_brewer(palette="Set1")+
  geom_smooth(aes(x=MeanCorr,y=sd,group=Model,color=Model))+
  labs(y="St.Dev(AUC)",x="|mean(corr)|",title="StDev of test AUC and Average absolute correlation")+ #c("y=exp(x^-1)","y=a=b*exp(-cx)")
  theme_bw(base_size=20)

summary_res %>% ggplot()+
  geom_point(aes(x=Score_corr,y=sd,color=Model),size=3)+
  #geom_line(aes(x=IntInf,y=sd,color=Model), linewidth=1.1)+
  scale_color_brewer(palette="Set1")+
  geom_smooth(aes(x=Score_corr,y=sd,group=Model,color=Model))+
  labs(y="St.Dev(AUC)",x="Score",title="StDev of test AUC and score")+ #c("y=exp(x^-1)","y=a=b*exp(-cx)")
  theme_bw(base_size=20)

summary_res %>% ggscatter(x="MeanCorr",y="sd",group="Model",color="Model",
                         shape="Model",palette="Set1",alpha=0.8)+ #size="N.samples",
  stat_cor(aes(y=sd,x=MeanCorr,color=Model),method = "pearson")

#summary_res %>% ggscatter(x="N.samples",y="IntInf",palette="Set1",alpha=0.8)+
#  stat_cor(aes(y=IntInf,x=N.samples),method = "pearson",label.x.npc="middle")

#summary_res %>% ggscatter(x="MeanCorr",y="IntInf",palette="Set1",alpha=0.8)+
#  stat_cor(aes(x=MeanCorr,y=IntInf),method = "pearson",label.x.npc="middle")

summary_res %>% ggscatter(x="MeanCorr",y="mdm",palette="Set1",alpha=0.8)+
  stat_cor(aes(x=MeanCorr,y=mdm),method = "pearson",label.x.npc="middle")

summary_res %>% ggscatter(x="N.samples",y="mdm",palette="Set1",alpha=0.8)+
  stat_cor(aes(y=mdm,x=N.samples),method = "pearson",label.x.npc="middle")

### model: lets build a model with sample size, model and MeanCorr as variables
model_1000<-lm(data=summary_res, sd ~ N.samples+MeanCorr+Model)
summary(model_1000)
# model with MDM
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -0.0172182 -0.0009214 -0.0001663  0.0008788  0.0150332 
# 
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.347e-02  1.091e-03  21.512  < 2e-16 ***
#   N.samples   -1.298e-05  1.204e-06 -10.782  < 2e-16 ***
#   mdm          7.526e-01  2.063e-02  36.477  < 2e-16 ***
#   ModelRF     -2.542e-03  5.381e-04  -4.725 5.58e-06 ***
#   ModelSVM    -2.608e-03  5.381e-04  -4.847 3.31e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.002636 on 139 degrees of freedom
# Multiple R-squared:  0.9714,	Adjusted R-squared:  0.9706 
# F-statistic:  1182 on 4 and 139 DF,  p-value: < 2.2e-16

## model with MeanCorr
# Call:
#   lm(formula = sd ~ N.samples + MeanCorr + Model, data = summary_res)
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -0.0172847 -0.0010571 -0.0000884  0.0008309  0.0163532 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.180e-01  5.529e-03 -21.333  < 2e-16 ***
#   N.samples   -1.025e-05  1.432e-06  -7.159 4.29e-11 ***
#   MeanCorr     1.166e+00  3.650e-02  31.940  < 2e-16 ***
#   ModelRF     -2.542e-03  6.059e-04  -4.196 4.82e-05 ***
#   ModelSVM    -2.608e-03  6.059e-04  -4.305 3.14e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.002968 on 139 degrees of freedom
# Multiple R-squared:  0.9638,	Adjusted R-squared:  0.9627 
# F-statistic: 924.9 on 4 and 139 DF,  p-value: < 2.2e-16

#percentage error
sigma(model_1000)*100/mean(summary_res$sd)
#8.404058 %

#check with anova if the categorical variable is significant
mod_2<-lm(data=summary_res, sd ~ N.samples+MeanCorr)
anova(mod_2, model_1000)

#to make diagnosis of results,
#influence.measures(model_1000)

#### models with correlation score
model_sc<-lm(data=summary_res, sd ~ N.samples+sqrt(Score_corr)+Model)

#### plot of density of sd for each different model
summary_res %>% ggplot() +
  geom_density(aes(x=sd, fill=Model),alpha=0.5)+
  theme_bw(base_size=20)

summary_res$Dataset<-rep(seq(1,length(samples)),3)

summary_res %>% ggplot()+
  geom_boxplot(aes(y=sd,x=Model,group=Model))


