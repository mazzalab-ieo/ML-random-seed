# generazione di un dataset su cui applicare CV + LASSO
library(MixSim)
library(tidyverse)
library(GGally)
library(caret)
library(glmnet)
library(ggpubr)
library(reshape2)

set.seed(394)
# solo un tipo di samples: 200 pz
# definizione n. features NO RULE OF THUMB: 
n<-200

mix_start<-MixSim(BarOmega = 0.1,K = 2, p = 30)
sim1<-simdataset(n, Pi=mix_start$Pi, Mu=mix_start$Mu, S=mix_start$S)

data<-as.data.frame(sim1$X)

data<-cbind(data,Label=sim1$id)

data$Label<-data$Label-1

#save data for other use
#write.csv(data,"/Users/ieo4991/Desktop/Instability_ML/datasets/sim_data.csv",
#          row.names = FALSE)

# check the correlation between the features
ggcorr(data[,-31], method = c("everything", "pearson")) 

# fai anche considerazioni sui risultati, ex: n di features selected e quali

#seeds<-c(sample(c(1:1000),100),sample(c(1000:10000),100),sample(c(10000:100000),100),sample(c(10^5:10^6),100))
seeds<-sample(c(1:10^6),1000)

#save also seeds
#write.csv(seeds,"/Users/ieo4991/Desktop/Instability_ML/datasets/seeds.csv",
#          row.names = FALSE)

######## perform a 5fold-cv with Lasso (we do not leave a test set this time, just perform CV)
# alpha==1 da il lasso
lambda_5<-numeric()
coef_nonzero_5<-numeric()

lambda_10<-numeric()
coef_nonzero_10<-numeric()

for (i in 1:length(seeds))
{
  print(i)
  set.seed(seeds[i])
  cv_logreg5<-cv.glmnet(as.matrix(data[,-31]), data$Label, family = "binomial", 
                       alpha = 1,type.measure="auc",nfolds =5) #10
  lambda_5<-c(lambda_5,cv_logreg5$lambda.min)
  coef<-as.vector(coef(cv_logreg5, cv_logreg5$lambda.min))
  coef_nonzero_5<-c(coef_nonzero_5,length(which(coef[-1]>0)))
  #plot(cv_logreg)
  #model <- glmnet(as.matrix(data[,-31]), data$Label, alpha = 1, family = "binomial",
  #              lambda = cv_logreg$lambda.min)
  
  #10 fold
  set.seed(seeds[i])
  cv_logreg10<-cv.glmnet(as.matrix(data[,-31]), data$Label, family = "binomial", 
                       alpha = 1,type.measure="auc",nfolds=10)
  lambda_10<-c(lambda_10,cv_logreg10$lambda.min)
  coef<-as.vector(coef(cv_logreg10, cv_logreg10$lambda.min))
  coef_nonzero_10<-c(coef_nonzero_10,length(which(coef[-1]>0)))
}

res<-data.frame("Seed"=rep(seeds,2),"Fold"=rep(c(5,10),each=1000),
                "Lambda"=c(lambda_5,lambda_10),
                "Features"=c(coef_nonzero_5,coef_nonzero_10),"Method"=rep("Mersenne-Twister",1000))

res %>% ggplot()+
  geom_point(aes(Lambda,Features,color=as.factor(Fold)))+
  geom_line(aes(Lambda,Features,color=as.factor(Fold)))+
  theme_bw(base_size=20)

res %>% ggboxplot("Fold","Lambda",fill="Fold",palette = c("#00AFBB", "#E7B800"),
                  notch=TRUE,
                  ggtheme = theme_pubr(base_size=20))+
  stat_compare_means(method = "wilcox.test",hjust=1.2,size=5)

res %>% ggplot()+
  geom_histogram(aes(Lambda, group=as.factor(Fold),fill=as.factor(Fold)))

res %>% ggplot()+
  geom_point(aes(Seed,Lambda,color=as.factor(Fold)))+
  #geom_line(aes(Seed,Lambda,color=Fold))+
  theme_bw(base_size=20)

res %>% ggplot()+
  geom_bar(aes(x=Features, fill=as.factor(Fold)))


# proviamo con un altro metodo per selezionare i seed
lambda_5<-numeric()
coef_nonzero_5<-numeric()

lambda_10<-numeric()
coef_nonzero_10<-numeric()

for (i in 1:length(seeds))
{
  print(i)
  set.seed(seeds[i],kind ="Wichmann-Hill")
  cv_logreg5<-cv.glmnet(as.matrix(data[,-31]), data$Label, family = "binomial", 
                        alpha = 1,type.measure="auc",nfolds=5) #10
  lambda_5<-c(lambda_5,cv_logreg5$lambda.min)
  coef<-as.vector(coef(cv_logreg5, cv_logreg5$lambda.min))
  coef_nonzero_5<-c(coef_nonzero_5,length(which(coef[-1]>0)))
  #plot(cv_logreg)
  #model <- glmnet(as.matrix(data[,-31]), data$Label, alpha = 1, family = "binomial",
  #              lambda = cv_logreg$lambda.min)
  
  #10 fold
  set.seed(seeds[i], kind ="Wichmann-Hill")
  cv_logreg10<-cv.glmnet(as.matrix(data[,-31]), data$Label, family = "binomial", 
                         alpha = 1,type.measure="auc",nfolds=10)
  lambda_10<-c(lambda_10,cv_logreg10$lambda.min)
  coef<-as.vector(coef(cv_logreg10, cv_logreg10$lambda.min))
  coef_nonzero_10<-c(coef_nonzero_10,length(which(coef[-1]>0)))
}

res_WH<-data.frame("Seed"=rep(seeds,2),"Fold"=rep(c(5,10),each=1000),
                "Lambda"=c(lambda_5,lambda_10),
                "Features"=c(coef_nonzero_5,coef_nonzero_10),"Method"=rep("Wichmann-Hill",1000))


#SuperDuper
lambda_5<-numeric()
coef_nonzero_5<-numeric()

lambda_10<-numeric()
coef_nonzero_10<-numeric()

for (i in 1:length(seeds))
{
  print(i)
  set.seed(seeds[i],kind ="Super-Duper")
  cv_logreg5<-cv.glmnet(as.matrix(data[,-31]), data$Label, family = "binomial", 
                        alpha = 1,type.measure="auc",nfolds=5) #10
  lambda_5<-c(lambda_5,cv_logreg5$lambda.min)
  coef<-as.vector(coef(cv_logreg5, cv_logreg5$lambda.min))
  coef_nonzero_5<-c(coef_nonzero_5,length(which(coef[-1]>0)))
  #plot(cv_logreg)
  #model <- glmnet(as.matrix(data[,-31]), data$Label, alpha = 1, family = "binomial",
  #              lambda = cv_logreg$lambda.min)
  
  #10 fold
  set.seed(seeds[i], kind ="Super-Duper")
  cv_logreg10<-cv.glmnet(as.matrix(data[,-31]), data$Label, family = "binomial", 
                         alpha = 1,type.measure="auc",nfolds=10)
  lambda_10<-c(lambda_10,cv_logreg10$lambda.min)
  coef<-as.vector(coef(cv_logreg10, cv_logreg10$lambda.min))
  coef_nonzero_10<-c(coef_nonzero_10,length(which(coef[-1]>0)))
}

res_SD<-data.frame("Seed"=rep(seeds,2),"Fold"=rep(c(5,10),each=1000),
                   "Lambda"=c(lambda_5,lambda_10),
                   "Features"=c(coef_nonzero_5,coef_nonzero_10),"Method"=rep("Super-Duper",1000))

res_all<-rbind(res,res_WH,res_SD)
res_all$Fold<-as.factor(res_all$Fold)

res_all %>% ggboxplot(x = "Method", y = "Lambda",group="Fold",
                      color = "Fold", palette = "jco")

res_all %>% ggboxplot(x = "Fold", y = "Lambda",group="Method",
                      color = "Method", palette = "jco")

res_all %>% filter(Fold==5) %>%
  ggplot()+
  geom_bar(aes(x=Features, fill=Method))

res_all %>% group_by(Method, Fold, Features) %>%
  summarize(tot_features=n()) %>%
  print(n=69)

lambda_all<-res_all %>% group_by(Method, Fold, Lambda) %>%
  summarize(tot=n()) 

lambda_all %>% 
  ggplot()+
  geom_point(aes(x=Lambda,y=tot,color=Method))+
  geom_line(aes(x=Lambda,y=tot,color=Method))+
  facet_wrap(~Fold)+
  theme_bw(base_size=20)

