# generation of simulated datasets
library(MixSim)

source('~/Desktop/Instability_ML/scripts/train_test_split.R')
library(caret)
library(reshape2)
library(tidyverse)

# mu_k mean of mixture are obtained from uniform p-dim hypercube
# sigma_k covariance matrices from Wishtart distrib with param p and p+1 dof

# pi_k mixing prop def from a lower bound from the user and sum to 1
# omega= average e max of misclassification allowed

#K=n. of clusters
#p: n. of dimensions---> n. of features
# homogeneus: same parameters for all the components

set.seed(234)
#two classes with small overlap
#BarOmega = 0.05 --> small overlap
#BarOmega = 0.1 --> medium overlap 
#BarOmega = 0.2 --> high overlap

mix_2_small<-MixSim(BarOmega = 0.05,K = 2, p = 10) # MaxOmega = 0.05, 
mix_2_med<-MixSim(BarOmega = 0.1,K = 2, p = 10)
mix_2_high<-MixSim(BarOmega = 0.2,K = 2, p = 10)

# define n of samples
#n<-100

# you get also the ID
#data_try<-simdataset(n, Pi=try$Pi, Mu=try$Mu, S=try$S)
#data<-as.data.frame(data_try$X)
#data<-cbind(data,data_try$id)

#colors<-c("red","blue","grey","orange")
#plot(data_try$X, col = colors[data_try$id], 
#     pch = 19, cex = 0.8,xlab = "", ylab = "", axes = FALSE)
#box()

### let's generate multiple datasets from the same distribution, with increasing number of patients
# cosi sono abbastanza equilibrati i dataset
data_2_small<-list()
data_2_med<-list()
data_2_high<-list()

samples<-seq(50,500, by=20)
n_datasets<-length(samples)

source("~/Desktop/Instability_ML/scripts/create_dataset.R")
data_2_small<-create_dataset(mix_2_small,samples)
data_2_med<-create_dataset(mix_2_med,samples)
data_2_high<-create_dataset(mix_2_high,samples)

# j<-0
# for (i in samples)
# {
#   j<-j+1
#   print(i)
#   d_s<-simdataset(i, Pi=mix_2_small$Pi, Mu=mix_2_small$Mu, S=mix_2_small$S)#, n.noise=10)
#   d_m<-simdataset(i, Pi=mix_2_med$Pi, Mu=mix_2_med$Mu, S=mix_2_med$S)
#   d_h<-simdataset(i, Pi=mix_2_high$Pi, Mu=mix_2_high$Mu, S=mix_2_high$S)
#   
#   # small over
#   data<-as.data.frame(d_s$X)
#   data<-cbind(data,Label=d_s$id)
#   data$Label<-ifelse(data$Label==1,0,1)
#   
#   data_2_small[[j]]<-data
#   
#   print(table(data$Label)/i)
# }

##### generate 100 random seed (integer numbers)
set.seed(1)
seeds<-sample.int(50000,1000)

#### apply a logistic regression

source("~/Desktop/Instability_ML/scripts/logreg_func.R")

res_2_small<-logreg_func(seeds,data_2_small,samples,"Small")
res_2_med<-logreg_func(seeds,data_2_med,samples,"Medium")
res_2_high<-logreg_func(seeds,data_2_high,samples,"High")

total_2_classes<-rbind(res_2_small,res_2_med,res_2_high)

res_2_small %>% group_by(N.samples) %>%
  summarize(mean=mean(acc_test),std=sd(acc_test)) %>%
  ggplot()+
  geom_ribbon(aes(x=N.samples,ymin = mean - std, ymax = mean + std), fill = "grey70") +
  geom_line(aes(x=N.samples,y=mean))+
  labs(title="Accuracy LogReg, 2 classes, Small overlap",x="N.samples",y="Mean Accuracy")

res_2_small %>% group_by(N.samples) %>%
  summarize(mean=mean(sens_test),std=sd(sens_test)) %>%
  ggplot()+
  geom_ribbon(aes(x=N.samples,ymin = mean - std, ymax = mean + std), fill = "grey70") +
  geom_line(aes(x=N.samples,y=mean))+
  labs(title="Sensitivity LogReg, 2 classes, Small overlap",x="N.samples",y="Mean Accuracy")

res_2_med %>% group_by(N.samples) %>%
  summarize(mean=mean(acc_test),std=sd(acc_test)) %>%
  ggplot()+
  geom_ribbon(aes(x=N.samples,ymin = mean - std, ymax = mean + std), fill = "grey70") +
  geom_line(aes(x=N.samples,y=mean))+
  labs(title="Accuracy LogReg, 2 classes, Medium overlap",x="N.samples",y="Mean Accuracy")

res_2_high %>% group_by(N.samples) %>%
  summarize(mean=mean(acc_test),std=sd(acc_test)) %>%
  ggplot()+
  geom_ribbon(aes(x=N.samples,ymin = mean - std, ymax = mean + std), fill = "grey70") +
  geom_line(aes(x=N.samples,y=mean))+
  labs(title="Accuracy LogReg, 2 classes, High overlap",x="N.samples",y="Mean Accuracy")


acc_2_small<-data.frame("Seed"=seeds)
for (i in 1:n_datasets)
{
  print(i)
  acc_test<-numeric()
  sens_test<-numeric()
  spec_test<-numeric()
  prec_test<-numeric()
  f1_test<-numeric()
  
  for(j in 1:length(seeds))
  {
  split1<-train_test_split(data_2_small[[i]],seeds[j])
  train<-split1$train
  train$Label<-as.factor(train$Label)
  test<-split1$test
  test$Label<-as.factor(test$Label)
  
  # logistic regression
  #logreg <- glm( Label ~., data = train, family = binomial,maxit=100)
  
  # Make predictions
  #probabilities <- logreg %>% predict(test, type = "response")
  #predicted.classes <- ifelse(probabilities > 0.5, "1", "0")
  
 # if (logreg$converged==FALSE)
   # print(paste("Dataset:",i," Seed:",j,sep=""))

  logreg<-train(Label~ ., data = train, method = "glm")
  probabilities<-predict(logreg, newdata=test, type = "prob")[2]
  predicted.classes <- as.factor(ifelse(probabilities > 0.5, "1", "0"))
  
  cm<-confusionMatrix(predicted.classes, test$Label)
  # Model accuracy
  acc_test<-c(acc_test,cm$overall[1])
  spec_test<-c(spec_test,cm$byClass[2])
  sens_test<-c(sens_test,cm$byClass[1])
  prec_test<-c(prec_test,cm$byClass[5])
  f1_test<-c(f1_test,cm$byClass[7])
  
  #acc_test<-c(acc_test,mean(predicted.classes == test$Label))
  }
  acc_2_small<-cbind(acc_2_small,acc_test)
}

colnames(acc_patients)<-c("Seed",paste("N.patients_",samples,sep=""))

acc_patients2<-melt(acc_patients,id.vars="Seed")
acc_patients2$variable<-rep(samples,each=length(seeds))

acc_patients2 %>% group_by(variable) %>%
  summarize(mean=mean(value),std=sd(value)) %>%
  ggplot()+
  geom_ribbon(aes(x=variable,ymin = mean - std, ymax = mean + std), fill = "grey70") +
  geom_line(aes(x=variable,y=mean))+
  labs(title="LogReg, 2 classes, Test set",x="N.samples",y="Mean Accuracy")


### plot gaussian mixture of one dataset

# use two dimensione and 3 classes
mix_example<-MixSim(BarOmega = 0.1,K = 3, p = 2)
data_example<-create_dataset(mix_example,100)

#plot(data_example[[1]][,-3], col = colors[data_example[[1]]$Label+1], 
#         pch = 19, cex = 0.8,xlab = "Feature 1", ylab = "Feature 2")
#legend("bottomright",legend=c("Class 1","Class 2","Class 3"),fill=colors[c(1,2,3)])

ggplot(data_example[[1]])+
  geom_point(aes(x=V1,y=V2, color=as.factor(Label)),size=2)+
  scale_color_manual(values=colors[c(2,4,3)],labels=c("Class 1","Class 2","Class 3"))+
  theme_bw(base_size=20)+
  labs(x="Feature 1",y="Feature 2",color="Class")

# density plot
#plot(density(data_example[[1]]$V1))
#plot(density(subset(data_example[[1]],Label=="0")$V1),col="red")

mix_example_1<-MixSim(BarOmega = 0.1,K = 3, p = 1)
data_example_1<-create_dataset(mix_example,100)

ggplot(data_example_1[[1]]) +
stat_density(aes(x = V1,  group = as.factor(Label)), position = "stack",color="black", geom="line") +
  stat_density(aes(x = V1,  color = as.factor(Label)), position = "identity",geom="line")+
  scale_alpha_manual(values = c(1,0,0,0))

library(mixtools)
my_mix <- normalmixEM(data_example_1[[1]]$V1, k = 3)

ggplot(data_example_1[[1]]) +
  geom_histogram(aes(V1, ..density..),binwidth = 0.05, fill="lightgrey") +
  geom_density(aes(x=V1),color="black")+
  geom_density(aes(x = V1,  color = as.factor(Label)), position = "identity")
  mapply(
    function(mean, sd, lambda, n, binwidth) {
      stat_function(
        fun = function(x) {
          (dnorm(x, mean = mean, sd = sd)) * n * binwidth * lambda
        }
      )
    },
    mean = my_mix[["mu"]], #mean
    sd = my_mix[["sigma"]], #standard deviation
    lambda = my_mix[["lambda"]], #amplitude
    n = length(data_example_1[[1]]$V1), #sample size
    binwidth = 0.05 #binwidth used for histogram
    )+
  theme_bw()
  
  #### to plot
  ex_2_small<-MixSim(BarOmega = 0.05,K = 2, p = 2)
  ex_2_small<-create_dataset(ex_2_small,100)
  ex_2_med<-MixSim(BarOmega = 0.1,K = 2, p = 2)
  ex_2_med<-create_dataset(ex_2_med,100)
  ex_2_high<-MixSim(BarOmega = 0.2,K = 2, p = 2)
  ex_2_high<-create_dataset(ex_2_high,100)
  
  overlap(ex_2_small$Pi,ex_2_small$Mu,ex_2_small$S)
  
  ggplot(ex_2_small[[1]])+
    geom_point(aes(x=V1,y=V2, color=as.factor(Label)),size=2)+
    scale_color_manual(values=colors[c(1,2)],labels=c("Class 0","Class 1"))+ #,"Class 2"
    theme_bw(base_size=20)+
    labs(x="Feature 1",y="Feature 2",color="Class", title="Small overlap")
  