#### validation on real data
library(readxl)
#library(plyr)
library(doParallel)
library(foreach)
library(tidyverse)
library(purrr)
#library(ImbCoL)

source("~/Desktop/Instability_ML/scripts/lasso_func.R")
source("~/Desktop/Instability_ML/scripts/svm_func.R")
source("~/Desktop/Instability_ML/scripts/rf_func.R")

source("~/Desktop/Instability_ML/scripts/internal.R")
source("~/Desktop/Instability_ML/scripts/overlapping_my.R")
source("~/Desktop/Instability_ML/scripts/neighborhood_my.R")
source("~/Desktop/Instability_ML/scripts/linearity.class_my.R")
source("~/Desktop/Instability_ML/scripts/complexity_my.R")

# Radiomics lung dataset
#pN is the label variable
radiomics_lung <- read_excel("datasets/radiomics_lung.xlsx")
radiomics_lung_fix<-radiomics_lung %>% 
  mutate(Label=pN) %>%
  separate(DIMENSIONE_NODULO, c("DIMENSIONE_NODULO", "not"),sep=";") %>%
  mutate(DIMENSIONE_NODULO=as.numeric(DIMENSIONE_NODULO),
         SESSO=as.numeric(as.factor(SESSO)),TERAPIE_PREOP=as.numeric(as.factor(TERAPIE_PREOP))) %>%
  select(-c(ID,not,pN,D_NASCITA, D_CHIRURGIA, D_TC, D__ULTIMO_CONT_, D__DECESSO, 
            LATO, SEDE, ISTOLOGIA, pT, M, GRADING,status,valid))
 
# diabetes data
# class is the label variable
diabetes_data <- read.csv("~/Desktop/Instability_ML/datasets/diabetes_data.xls", sep=";")
colnames(diabetes_data)[which(colnames(diabetes_data)=="class")]<-"Label"
diabetes_data_fix<-diabetes_data %>% mutate(gender=as.numeric(as.factor(gender)))
diabetes_data_fix<-data.frame(apply(diabetes_data_fix,2,as.numeric))

#cancer patients
# Level is the label variable and there are 3 classes
#for now I retain only the High and Low and remove the Medium
cancer_patient <- read.csv("~/Desktop/Instability_ML/datasets/cancer patient data sets.csv")
cancer_patient_fix<- cancer_patient %>%
  filter(Level %in% c("High","Medium")) %>%
  mutate(Label=ifelse(Level=="High",1,0)) %>%
  select(-c(index,Patient.Id,Level))

# put data together to run the parallel pipeline
valid_data<-list(radiomics_lung_fix,diabetes_data_fix,cancer_patient_fix)
valid_size<-as.numeric(lapply(valid_data,nrow))

#compute complexity
valid_comp<-lapply(valid_data, function(x) complexity_my(Label~.,x))
valid_comp_df<-map_dfr(valid_comp, ~as.data.frame(t(.x)))
valid_comp_df$Case<-c("Lung","Diabetes","Cancer")

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

### Lasso
cl<-makePSOCKcluster(numCores)
registerDoParallel(cl)

valid_lasso<-foreach (i=seq_along(valid_size), .combine = "c",
                      .packages =c("caret","tidyverse","pROC","glmnet")) %dopar%
{
  list(lasso_func(seeds_1000,valid_data[i],torem))
}
stopCluster(cl)

#lung
res_lung<-rbind(valid_lasso[[1]][[1]][,-3],valid_svm[[1]][[1]][,-3],valid_rf[[1]][[1]][,-3])
res_lung$Model<-rep(c("Lasso","SVM","RF"),each=1000)
res_lung$Case<-"Lung"
res_lung$MeanCorr<-mean(abs(cor(radiomics_lung_fix[,-which(colnames(radiomics_lung_fix)=="Label")])))
res_lung<-res_lung %>% mutate("F4"=valid_comp_df$overlapping_my.F4[1],"N1"=valid_comp_df$neighborhood_my.N1[1])

#diabetes
res_diabetes<-rbind(valid_lasso[[2]][[1]][,-3],valid_svm[[2]][[1]][,-3],valid_rf[[2]][[1]][,-3])
res_diabetes$Model<-rep(c("Lasso","SVM","RF"),each=1000)
res_diabetes$Case<-"Diabetes"
res_diabetes$MeanCorr<-mean(abs(cor(diabetes_data_fix[,-which(colnames(diabetes_data_fix)=="Label")])))
res_diabetes<-res_diabetes %>% mutate("F4"=valid_comp_df$overlapping_my.F4[2],"N1"=valid_comp_df$neighborhood_my.N1[2])

#cancer
res_cancer<-rbind(valid_lasso[[3]][[1]][,-3],valid_svm[[3]][[1]][,-3],valid_rf[[3]][[1]][,-3])
res_cancer$Model<-rep(c("Lasso","SVM","RF"),each=1000)
res_cancer$Case<-"Cancer"
res_cancer$MeanCorr<-mean(abs(cor(cancer_patient_fix[,-which(colnames(cancer_patient_fix)=="Label")])))
res_cancer<-res_cancer %>% mutate("F4"=valid_comp_df$overlapping_my.F4[3],"N1"=valid_comp_df$neighborhood_my.N1[3])

res_validation<-rbind(res_lung,res_diabetes,res_cancer)

summary_val<- res_validation %>%
  group_by(Case,Model,N.samples,MeanCorr,F4,N1) %>%
  summarize(mean=mean(auc_test),sd=sd(auc_test),cv=sd/mean)

##model the sd
pred_val_lung<-as.data.frame(predict.lm(model_inv,summary_val,interval="prediction"))

combo_val<-cbind(summary_val,1/pred_val_lung)

combo_val%>% 
  ggplot() +
  geom_point( aes(x=Model, y=fit,color="Estimate"), size= 4, shape= 4) +
  geom_point( aes(x=Model, y=sd, color="True"), size=3) +
  scale_color_manual(values=c('black','#E69F00'))+#colors[c(2,3)])+
  geom_errorbar( aes(x=Model, ymin=lwr, ymax=upr), color ='black', linetype = "dashed",width=0.5)+
  facet_wrap(~Case,scales="free_y")+
  labs(x="Model",y="St.Dev",color="StDev",title="Fit for validation datasets, 95% Prediction Interval")+
  theme_bw(base_size=20)

