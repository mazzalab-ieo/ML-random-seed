library(MixSim)
library(caret)
library(reshape2)
library(tidyverse)
library(RColorBrewer)

source('~/Desktop/Instability_ML/scripts/train_test_split.R')
source("~/Desktop/Instability_ML/scripts/create_dataset.R")
source("~/Desktop/Instability_ML/scripts/logreg_func.R")

set.seed(234)
# mu_k mean of mixture are obtained from uniform p-dim hypercube
# sigma_k covariance matrices from Wishtart distrib with param p and p+1 dof

# pi_k mixing prop def from a lower bound from the user and sum to 1
# omega= average e max of misclassification allowed

#K=n. of clusters
#p: n. of dimensions---> n. of features
# homogeneus: same parameters for all the components

#two classes
#BarOmega = 0.05 --> small overlap
#BarOmega = 0.1 --> medium overlap 
#BarOmega = 0.2 --> high overlap

mix_2_small<-MixSim(BarOmega = 0.05,K = 2, p = 10) # MaxOmega = 0.05, 
mix_2_med<-MixSim(BarOmega = 0.1,K = 2, p = 10)
mix_2_high<-MixSim(BarOmega = 0.2,K = 2, p = 10)

#### check average overlap of mixtures
overlap_small<-overlap(mix_2_small$Pi, mix_2_small$Mu, mix_2_small$S)
overlap_med<-overlap(mix_2_med$Pi, mix_2_med$Mu, mix_2_med$S)
overlap_high<-overlap(mix_2_high$Pi, mix_2_high$Mu, mix_2_high$S)

### let's generate multiple datasets from the same distribution, with increasing number of patients
# cosi sono abbastanza equilibrati i dataset
data_2_small<-list()
data_2_med<-list()
data_2_high<-list()

samples<-seq(50,500, by=20)
n_datasets<-length(samples)

data_2_small<-create_dataset(mix_2_small,samples)
data_2_med<-create_dataset(mix_2_med,samples)
data_2_high<-create_dataset(mix_2_high,samples)

# visualization 
colors<-brewer.pal(9,"Set1")
plot(data_2_small[[21]][,-ncol(data_2_small[[21]])], col = colors[data_2_small[[21]]$Label+1], pch = 19, cex = 0.8, main="Overview: 450 samples and small overlap")
plot(data_2_med[[21]][,-ncol(data_2_med[[21]])], col = colors[data_2_med[[21]]$Label+1], pch = 19, cex = 0.8, main="Overview: 450 samples and medium overlap")
plot(data_2_high[[21]][,-ncol(data_2_high[[21]])], col = colors[data_2_high[[21]]$Label+1], pch = 19, cex = 0.8, main="Overview: 450 samples and high overlap")

##### generate 100 random seed (integer numbers)
set.seed(1)
seeds<-sample.int(50000,1000)

#### apply a logistic regression
res_2_small<-logreg_func(seeds,data_2_small,samples,"Small")
res_2_med<-logreg_func(seeds,data_2_med,samples,"Medium")
res_2_high<-logreg_func(seeds,data_2_high,samples,"High")

total_2_classes<-rbind(res_2_small,res_2_med,res_2_high)

summary_2_classes<-total_2_classes %>% group_by(Overlap,N.samples) %>%
  summarize(AveAcc=mean(acc_test),StdAcc=sd(acc_test), CvAcc=
            AveSens=mean(sens_test),StdSens=sd(sens_test),
            AveSpec=mean(spec_test),StdSpec=sd(spec_test),
            AvePrec=mean(prec_test),StdPrec=sd(prec_test),
            AveF1=mean(f1_test),StdF1=sd(f1_test)) #%>%
  ` #mutate(CvAcc=StdAcc/AveAcc,CvSens=StdSens/AveSens,CvSpec=StdSpec/AveSpec,CvPrec=StdPrec/AvePrec,CvF1=StdF1/AveF1)

# results
p1<-summary_2_classes %>%
  ggplot()+
  geom_ribbon(aes(x=N.samples,ymin = AvePrec - StdPrec, ymax = AvePrec + StdPrec, fill=Overlap),alpha=0.3) +
  geom_line(aes(x=N.samples,y=AvePrec, color=Overlap),size=1.2)+
  scale_color_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
  #guides(fill="none",color="none")+
  labs(title="Precision LogReg, 2 classes balanced",x="N.samples",y="Mean Precision")+
  theme_bw(base_size=20)

#you can change the metric you want to plot
p2<-summary_2_classes %>%
  ggplot()+
  geom_line(aes(x=N.samples,y=StdPrec, color=Overlap),size=1.2)+
  geom_point(aes(x=N.samples,y=StdPrec, color=Overlap),size=1.5)+
  scale_color_brewer(palette = "Set1")+
  #guides(color="none")+
  labs(title="St.Dev. on LogReg Precision, 2 classes balanced",x="N.samples",y="St.Dev.")+
  theme_bw(base_size=20)

res_2_small %>% group_by(N.samples) %>%
  summarize(mean=mean(acc_test),std=sd(acc_test)) %>%
  ggplot()+
  geom_ribbon(aes(x=N.samples,ymin = mean - std, ymax = mean + std), fill = "grey70") +
  geom_line(aes(x=N.samples,y=mean))+
  labs(title="Accuracy LogReg, 2 classes, Small overlap",x="N.samples",y="Mean Accuracy")


##### 3 classes
# three classes: here you can define the average and the maximum overlap
mix_3_small<-MixSim(BarOmega = 0.05,K = 3, p = 10)
mix_3_med<-MixSim(BarOmega = 0.1,K = 3, p = 10)
mix_3_high<-MixSim(BarOmega = 0.2,K = 3, p = 10)

data_3_small<-create_dataset(mix_3_small,samples)
data_3_med<-create_dataset(mix_3_med,samples)
data_3_high<-create_dataset(mix_3_high,samples)

### let's use Linear Discriminant Analysis for more than 2 classes
source("~/Desktop/Instability_ML/scripts/lda_func.R")

lda_3_small<-lda_func(seeds,data_3_small,samples,"Small")
lda_3_med<-lda_func(seeds,data_3_med,samples,"Medium")
lda_3_high<-lda_func(seeds,data_3_high,samples,"High")

total_3_lda<-rbind(lda_3_small,lda_3_med,lda_3_high)

summary_3_lda<-total_3_lda %>% group_by(Overlap,N.samples) %>%
  summarize(AveAcc=mean(acc_test),StdAcc=sd(acc_test),
            AveSens=mean(sens_test),StdSens=sd(sens_test),
            AveSpec=mean(spec_test),StdSpec=sd(spec_test),
            AvePrec=mean(prec_test),StdPrec=sd(prec_test),
            AveF1=mean(f1_test),StdF1=sd(f1_test)) 

#you can change the metric you want to plot
summary_3_lda %>%
  ggplot()+
  geom_ribbon(aes(x=N.samples,ymin = AvePrec - StdPrec, ymax = AvePrec + StdPrec, fill=Overlap),alpha=0.3) +
  geom_line(aes(x=N.samples,y=AvePrec, color=Overlap),size=1.2)+
  scale_color_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
  #guides(fill="none",color="none")+
  labs(title="Precision LDA, 3 classes balanced",x="N.samples",y="Mean Precision")+
  theme_bw(base_size=20)

summary_3_lda %>%
  ggplot()+
  geom_line(aes(x=N.samples,y=StdPrec, color=Overlap),size=1.2)+
  geom_point(aes(x=N.samples,y=StdPrec, color=Overlap),size=1.5)+
  scale_color_brewer(palette = "Set1")+
  #guides(color="none")+
  labs(title="St.Dev on LDA Precision, 3 classes balanced",x="N.samples",y="St.Dev.")+
  theme_bw(base_size=20)


