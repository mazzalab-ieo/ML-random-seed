#### validation on real data
library(readxl)
#library(plyr)
library(doParallel)
library(foreach)
library(tidyverse)
library(purrr)
#library(ImbCoL)

source("~/ML-random-seed/scripts/lasso_func.R")
source("~/ML-random-seed/scripts/svm_func.R")
source("~/ML-random-seed/scripts/rf_func.R")

source("~/ML-random-seed/scripts/internal.R")
source("~/ML-random-seed/scripts/overlapping_my.R")
source("~/ML-random-seed/scripts/neighborhood_my.R")
source("~/ML-random-seed/scripts/linearity.class_my.R")
source("~/ML-random-seed/scripts/complexity_my.R")

# Radiomics lung dataset
#pN is the label variable
radiomics_lung <- read_excel("data/radiomics_lung.xlsx")
radiomics_lung_fix<-radiomics_lung %>% 
  mutate(Label=pN) %>%
  separate(DIMENSIONE_NODULO, c("DIMENSIONE_NODULO", "not"),sep=";") %>%
  mutate(DIMENSIONE_NODULO=as.numeric(DIMENSIONE_NODULO),
         SESSO=as.numeric(as.factor(SESSO)),TERAPIE_PREOP=as.numeric(as.factor(TERAPIE_PREOP))) %>%
  select(-c(ID,not,pN,D_NASCITA, D_CHIRURGIA, D_TC, D__ULTIMO_CONT_, D__DECESSO, 
            LATO, SEDE, ISTOLOGIA, pT, M, GRADING,status,valid))
 
# diabetes data
# class is the label variable
diabetes_data <- read.csv("data/diabetes_data.xls", sep=";")
colnames(diabetes_data)[which(colnames(diabetes_data)=="class")]<-"Label"
diabetes_data_fix<-diabetes_data %>% mutate(gender=as.numeric(as.factor(gender)))
diabetes_data_fix<-data.frame(apply(diabetes_data_fix,2,as.numeric))

#cancer patients
# Level is the label variable and there are 3 classes
#for now I retain only the High and Low and remove the Medium
cancer_patient <- read.csv("data/cancer patient data sets.csv")
cancer_patient_fix<- cancer_patient %>%
  filter(Level %in% c("High","Medium")) %>%
  mutate(Label=ifelse(Level=="High",1,0)) %>%
  select(-c(index,Patient.Id,Level))

### breast cancer
breast_cancer<-read.csv("data/breast-cancer.csv")
breast_cancer_fix <- breast_cancer %>%
  mutate(Label=ifelse(diagnosis=="B",0,1)) %>%
  select(-c(id,diagnosis))

# put data together to run the parallel pipeline
valid_data<-list(radiomics_lung_fix[,-c(1,2,3,172)],diabetes_data_fix,cancer_patient_fix,breast_cancer_fix)
valid_size<-as.numeric(lapply(valid_data,nrow))

#compute complexity
valid_comp<-lapply(valid_data, function(x) complexity_my(Label~.,x))
valid_comp_df<-map_dfr(valid_comp, ~as.data.frame(t(.x)))
valid_comp_df$Case<-c("Lung","Diabetes","Cancer","BreastCancer")

##pca to explore the data

torem<-0
##### SVM #####
#torem<-which(colnames(radiomics_lung_fix)=="ID")
##### in parallel
numCores <- detectCores()
numCores
cl<-makePSOCKcluster(numCores)
registerDoParallel(cl)

valid_svm<-foreach (i=seq_along(valid_size), .combine = "c",
                 .packages =c("caret","tidyverse","pROC")) %dopar%
  {
    #cat(paste("Starting iteration ",i,"\n"))
    list(svm_func(seeds_1000,valid_data[i],torem))
  }
stopCluster(cl)

valid_svm[[4]]<-svm_func(seeds_1000,valid_data[4],torem)
#valid_svm<-svm_func(seq(1:2),valid_data,torem) #seeds_1000

### RF
cl<-makePSOCKcluster(numCores)
registerDoParallel(cl)

valid_rf<-foreach (i=seq_along(valid_size), .combine = "c",
                 .packages =c("caret","tidyverse","pROC","randomForest")) %dopar%
  {
    #cat(paste("Starting iteration ",i,"\n"))
    list(rf_func(seeds_1000,valid_data[i],torem))
  }
stopCluster(cl)

valid_rf[[4]]<-rf_func(seeds_1000,valid_data[4],torem)

### Lasso
cl<-makePSOCKcluster(numCores)
registerDoParallel(cl)

valid_lasso<-foreach (i=seq_along(valid_size), .combine = "c",
                      .packages =c("caret","tidyverse","pROC","glmnet")) %dopar%
{
  list(lasso_func(seeds_1000,valid_data[i],torem))
}
stopCluster(cl)

valid_lasso[[4]]<-lasso_func(seeds_1000,valid_data[4],torem)

#lung
res_lung<-rbind(valid_lasso[[1]][[1]][,-3],valid_svm[[1]][[1]][,-3],valid_rf[[1]][[1]][,-3])
res_lung$Model<-rep(c("Lasso","SVM","RF"),each=1000)
res_lung$Case<-"Lung"
res_lung$MeanCorr<-mean(abs(cor(radiomics_lung_fix[,-which(colnames(radiomics_lung_fix)=="Label")])))
res_lung<-res_lung %>% mutate("F4"=valid_comp_df$overlapping_my.F4[1],
                              "N1"=valid_comp_df$neighborhood_my.N1[1],
                              "N3"=valid_comp_df$neighborhood_my.N3[1])

#diabetes
res_diabetes<-rbind(valid_lasso[[2]][[1]][,-3],valid_svm[[2]][[1]][,-3],valid_rf[[2]][[1]][,-3])
res_diabetes$Model<-rep(c("Lasso","SVM","RF"),each=1000)
res_diabetes$Case<-"Diabetes"
res_diabetes$MeanCorr<-mean(abs(cor(diabetes_data_fix[,-which(colnames(diabetes_data_fix)=="Label")])))
res_diabetes<-res_diabetes %>% mutate("F4"=valid_comp_df$overlapping_my.F4[2],
                                      "N1"=valid_comp_df$neighborhood_my.N1[2],
                                      "N3"=valid_comp_df$neighborhood_my.N3[2])

#cancer
res_cancer<-rbind(valid_lasso[[3]][[1]][,-3],valid_svm[[3]][[1]][,-3],valid_rf[[3]][[1]][,-3])
res_cancer$Model<-rep(c("Lasso","SVM","RF"),each=1000)
res_cancer$Case<-"Cancer"
res_cancer$MeanCorr<-mean(abs(cor(cancer_patient_fix[,-which(colnames(cancer_patient_fix)=="Label")])))
res_cancer<-res_cancer %>% mutate("F4"=valid_comp_df$overlapping_my.F4[3],
                                  "N1"=valid_comp_df$neighborhood_my.N1[3],
                                  "N3"=valid_comp_df$neighborhood_my.N3[3])

# breast cancer
res_bc<-rbind(valid_lasso[[4]][[1]][,-3],valid_svm[[4]][[1]][,-3],valid_rf[[4]][[1]][,-3])
res_bc$Model<-rep(c("Lasso","SVM","RF"),each=1000)
res_bc$Case<-"BreastCancer"
res_bc$MeanCorr<-mean(abs(cor(breast_cancer_fix[,-which(colnames(breast_cancer_fix)=="Label")])))
res_bc<-res_bc %>% mutate("F4"=valid_comp_df$overlapping_my.F4[4],
                          "N1"=valid_comp_df$neighborhood_my.N1[4],
                          "N3"=valid_comp_df$neighborhood_my.N3[4])

res_validation<-rbind(res_lung,res_diabetes,res_cancer,res_bc)

summary_val<- res_validation %>%
  group_by(Case,Model,N.samples,MeanCorr,F4,N1,N3) %>%
  summarize(mean=mean(auc_test),sd=sd(auc_test),cv=sd/mean)

summary_val_100seed<- res_validation %>% filter(Seed<=100) %>%
  group_by(Case,Model,N.samples,MeanCorr,F4,N1,N3) %>%
  summarize(mean=mean(auc_test),sd=sd(auc_test),cv=sd/mean)

#write.csv(summary_val_100seed,"~/ML-random-seed/datasets/summary_validation.csv",row.names = F)

##model the sd
pred_val<-as.data.frame(predict(model_glm_invgauss,summary_val,type="response"))
library(ciTools)
val_prediction<-add_ci(summary_val, model_glm_invgauss)
#write.csv(summary_val,"~/ML-random-seed/datasets/summary_validation.csv",row.names = F)

val_prediction%>% 
  ggplot() +
  geom_point( aes(x=Model, y=pred,color="Estimate"), size= 4, shape= 4) +
  geom_point( aes(x=Model, y=sd, color="True"), size=3) +
  scale_color_manual(values=c('black','#E69F00'))+#colors[c(2,3)])+
  geom_errorbar( aes(x=Model, ymin=LCB0.025, ymax=UCB0.975), color ='black', linetype = "dashed",width=0.5)+
  facet_wrap(~Case,scales="free_y")+
  labs(x="Model",y="St.Dev",color="StDev",title="External Validation cases")+
  theme_bw(base_size=20)

#combo_val<-cbind(summary_val,1/pred_val)
combo_val%>% 
  ggplot() +
  geom_point( aes(x=Model, y=fit,color="Estimate"), size= 4, shape= 4) +
  geom_point( aes(x=Model, y=sd, color="True"), size=3) +
  scale_color_manual(values=c('black','#E69F00'))+#colors[c(2,3)])+
  geom_errorbar( aes(x=Model, ymin=lwr, ymax=upr), color ='black', linetype = "dashed",width=0.5)+
  facet_wrap(~Case,scales="free_y")+
  labs(x="Model",y="St.Dev",color="StDev",title="Fit for validation datasets, 95% Prediction Interval")+
  theme_bw(base_size=20)


#### read results from rf
rf_rad <- read.csv("~/ML-random-seed/Results/results_1000/rf_rad.csv")

rf_rad%>% 
  ggplot() +
  geom_point( aes(x=Model, y=Pred,color="Estimate"), size= 4, shape= 4) +
  geom_point( aes(x=Model, y=sd, color="True"), size=3) +
  scale_color_manual(values=c('black','#E69F00'))+#colors[c(2,3)])+
  geom_errorbar( aes(x=Model, ymin=Lower_1, ymax=Upper_99), color ='black', linetype = "dashed",width=0.5)+
  facet_wrap(~N.samples)+#,scales="free_y")+
  labs(x="Model",y="St.Dev",color="StDev",title="RF for radiomics datasets, 99% c.i.")+
  theme_bw(base_size=20)


rf_val <- read.csv("~/ML-random-seed/Results/results_1000/rf_val.csv")

rf_val%>% 
  ggplot() +
  geom_point( aes(x=Model, y=Pred,color="Estimate"), size= 4, shape= 4) +
  geom_point( aes(x=Model, y=sd, color="True"), size=3) +
  scale_color_manual(values=c('black','#E69F00'))+#colors[c(2,3)])+
  geom_errorbar( aes(x=Model, ymin=Lower_1, ymax=Upper_99), color ='black', linetype = "dashed",width=0.5)+
  facet_wrap(~Case)+#,scales="free_y")+
  labs(x="Model",y="St.Dev",color="StDev",title="RF for validation datasets, 99% c.i.")+
  theme_bw(base_size=20)

