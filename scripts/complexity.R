#### measures of dataset complexity
#library(ImbCoL)
library(tidyverse)

## Extract all complexity measures available for my datasets
source("~/Desktop/Instability_ML/scripts/internal.R")
source("~/Desktop/Instability_ML/scripts/overlapping_my.R")
source("~/Desktop/Instability_ML/scripts/neighborhood_my.R")
source("~/Desktop/Instability_ML/scripts/linearity.class_my.R")
source("~/Desktop/Instability_ML/scripts/complexity_my.R")

complexity<-lapply(data_list_2, function(x) complexity_my(Label ~ ., x))
# transform in data frame
library(purrr)
complexity_all<-map_dfr(complexity, ~as.data.frame(t(.x)))
complexity_all$Sample<-samples

complexity_all_2<- complexity_all %>%
  select(overlapping_my.F1,overlapping_my.F1v,overlapping_my.F2,overlapping_my.F3,overlapping_my.F4,
         neighborhood_my.N1,neighborhood_my.N2,neighborhood_my.N3,neighborhood_my.N4,neighborhood_my.T1,
         linearity.class_my.L1,linearity.class_my.L2,linearity.class_my.L3) %>%
   gather(Measure, Value, overlapping_my.F1:linearity.class_my.L3, factor_key=TRUE) #%>%
   #separate(Measure, c("Measure", "Class"), "_partial.")

#complexity_all_2$Class<-as.factor(complexity_all_2$Class)

# compute complexity also for the radiomic case
complexity_rad<-lapply(data_sim_list, function(x) complexity_my(Label ~ ., x[,-170]))
complexity_rad_all<-map_dfr(complexity_rad, ~as.data.frame(t(.x)))
complexity_rad_all$Sample<-c(100,300,600)

complexity_rad_all_2<- complexity_rad_all %>%
  select(overlapping_my.F1,overlapping_my.F1v,overlapping_my.F2,overlapping_my.F3,overlapping_my.F4,
         neighborhood_my.N1,neighborhood_my.N2,neighborhood_my.N3,neighborhood_my.N4,neighborhood_my.T1,
         linearity.class_my.L1,linearity.class_my.L2,linearity.class_my.L3,Sample) %>%
  gather(Measure, Value, overlapping_my.F1:linearity.class_my.L3, factor_key=TRUE) #%>%

#  gather(Measure, Value, overlapping.F2_partial.0:linearity.class.L3_partial.1, factor_key=TRUE) %>%
#  separate(Measure, c("Measure", "Class"), "_partial.")
#complexity_rad_all_2$Class<-as.factor(complexity_rad_all_2$Class)

# now i want to use F4, N1 and N3
complexity_all_2 %>% 
  filter(Measure %in% c("overlapping_my.F4","neighborhood_my.N1","neighborhood_my.N4")) %>%
  ggplot()+
  geom_boxplot(aes(y=Value,x=Measure),alpha=0.8)+ #fill=Class
  #scale_fill_manual(values=colors[c(4,5)])+
  #facet_grid(~Measure)+
  #geom_point(data=complexity_rad_all %>% filter(Measure %in% c("overlapping.F4","neighborhood.N1","neighborhood.N3")),
  #           aes(x=Class,y=Value,color=as.factor(Sample)),size=3)+
  geom_point(data=complexity_rad_all_2 %>% filter(Measure %in% c("overlapping_my.F4","neighborhood_my.N1","neighborhood_my.N4")),
             aes(x=Measure,y=Value,color=as.factor(Sample)),size=5)+
  scale_color_manual(values=colors,labels=paste("Size",c(100,300,600),sep=" "))+
  #facet_grid(~Measure)+
  labs(title="Complexity for simulated datasets and validation",color="Radiomic datasets")+
  theme_bw(base_size=20) 

### try to add complexity to our summary to create a model

summary_res<-cbind(summary_res,"F4"=rep(complexity_all$overlapping_my.F4,3),
                   "N1"=rep(complexity_all$neighborhood_my.N1,3),
                   "N3"=rep(complexity_all$neighborhood_my.N3,3))
summary_res$N4<-rep(complexity_all$neighborhood_my.N4,3)

model_3<-lm(data=summary_res, sd ~ N.samples+Model+MeanCorr+F4+N1)
#model_3<-lm(data=rbind(summary_res[,-7],res_all_rad_summary), sd ~ N.samples+Model+MeanCorr+F4+N1)
summary(model_3)
diagnostic_model_3<-plot(model_3)

#interval prediction: 95% of our samples has value in that interval
res_all_rad_summary<-cbind(res_all_rad_summary,"F4"=rep(complexity_rad_all$overlapping_my.F4,3),
                           "N1"=rep(complexity_rad_all$neighborhood_my.N1,3),
                           "N3"=rep(complexity_rad_all$neighborhood_my.N3,3))
res_all_rad_summary$N4<-rep(complexity_rad_all$neighborhood_my.N4,3)

pred_rad<-as.data.frame(predict.lm(model_3,res_all_rad_summary,interval="prediction")) #se.fit=TRUE, 
combo<-cbind(res_all_rad_summary,pred_rad)

combo%>% ggplot() +
  geom_point( aes(x=as.factor(N.samples), y=sd, color="True"), size=3 ) +
  geom_point( aes(x=as.factor(N.samples), y=fit,color="Predicted"), size=3 ) +
  scale_color_manual(values=colors[c(2,3)])+
  geom_errorbar( aes(x=as.factor(N.samples), ymin=lwr, ymax=upr), color =colors[2], linetype = "dashed",width=0.4)+
  facet_wrap(~Model)+
  labs(x="N.samples",y="St.Dev",color="StDev",title="Fit for validation datasets")+
  theme_bw(base_size=20)

summary_res %>% 
  select(-c(mean,cv,mdm)) %>%
  #mutate(N1log=log(N1)) %>%
  gather(Measure, Value, -c(Model,sd,N3,N4), factor_key=TRUE) %>% #MeanCorr:N4, 
  ggplot()+
  geom_point(aes(x=log(Value),y=log(sd),color=Model))+
  facet_wrap(~Measure,scales="free_x")+
  #geom_line(aes(x=IntInf,y=sd,color=Model), linewidth=1.1)+
  scale_color_brewer(palette="Set1")+
  #geom_smooth(aes(x=Score_corr,y=sd,group=Model,color=Model))+
  labs(y="St.Dev(AUC)",x="Measure",title="StDev of test AUC over complexity")+
  theme_bw(base_size=20)

cor(summary_res[,c(2,4,6,9,10)])
    
model_log<-lm(data=summary_res, log(sd) ~ N.samples+Model+MeanCorr+F4+N1)
pred_rad_log<-as.data.frame(predict.lm(model_log,res_all_rad_summary,se.fit=TRUE))#interval="prediction")) #s
combo_log<-cbind(res_all_rad_summary,pred_rad_log)

combo_log%>% ggplot() +
  geom_point( aes(x=as.factor(N.samples), y=log(sd), color="True"), size=3 ) +
  geom_point( aes(x=as.factor(N.samples), y=fit,color="Predicted"), size=3 ) +
  scale_color_manual(values=colors[c(2,3)])+
  geom_errorbar( aes(x=as.factor(N.samples), ymin=fit-se.fitlwr, ymax=upr), color =colors[2], linetype = "dashed",width=0.4)+
  facet_wrap(~Model)+
  labs(x="N.samples",y="log(St.Dev)",color="StDev",title="Fit for validation datasets")+
  theme_bw(base_size=20)

model_inv<-lm(data=summary_res, (1/sd) ~ N.samples+Model+MeanCorr+F4+N1)
plot(model_inv)
summary(model_inv)
# partial plots
termplot(model_inv,partial.resid = TRUE, terms=c("N.samples","MeanCorr","F4","N1"))

pred_rad_inv<-as.data.frame(predict.lm(model_inv,res_all_rad_summary,interval="prediction"))#,se.fit=TRUE)) #interval="prediction
# since we are performing 1/y we have to exchange lower and upper bound
pred_rad_inv_fix<-pred_rad_inv%>% mutate(lwr=ifelse(lwr<0,min(pred_rad_inv$lwr[which(pred_rad_inv$lwr>0)]),
                                                    lwr))
combo_inv<-cbind(res_all_rad_summary,"fit"=1/pred_rad_inv_fix$fit,"lwr"=1/pred_rad_inv_fix$upr, "upr"=1/pred_rad_inv_fix$lwr)


# c.i 95%  for sd on radiomics: it's equal to set the interval as confidence above
#tstar<-qt(df=model_inv$df.residual,p=0.975)
#ci_low<-pred_rad_inv$fit.fit-tstar*pred_rad_inv$se.fit
#ci_up<-pred_rad_inv$fit.fit+tstar*pred_rad_inv$se.fit
#CIinfo<-as.data.frame(cbind(Lower=ci_low,Estimate=pred_rad_inv$fit.fit,Upper=ci_up))

#CI_sd<-1/CIinfo
#combo_inv<-cbind(res_all_rad_summary,CI_sd)

#remember that the LASSO at 00 has been fixed, to better see the others
combo_inv%>% 
  ggplot() +
  geom_point( aes(x=Model, y=fit,color="Estimate"), size= 4, shape= 4) +
  geom_point( aes(x=Model, y=sd, color="True"), size=3) +
  scale_color_manual(values=c('black','#E69F00'))+#colors[c(2,3)])+
  geom_errorbar( aes(x=Model, ymin=lwr, ymax=upr), color ='black', linetype = "dashed",width=0.5)+
  facet_wrap(~N.samples,scales="free_y")+
  labs(x="Model",y="St.Dev",color="StDev",title="Fit for validation datasets, 95% Prediction Interval")+
  theme_bw(base_size=20)

##### 
model_inv_log<-lm(data=summary_res, (1/sd) ~ N.samples+Model+MeanCorr+F4)#+N1)
plot(model_inv_log)
summary(model_inv_log)

pred_rad_inv_log<-as.data.frame(predict.lm(model_inv_log,res_all_rad_summary,interval="prediction"))#,se.fit=TRUE)) #interval="prediction
combo_inv_log<-cbind(res_all_rad_summary,1/pred_rad_inv_log)

combo_inv_log%>% ggplot() +
  geom_point( aes(x=Model, y=sd, color="True"), size=3 ) +
  geom_point( aes(x=Model, y=fit,color="Estimate"), size=3 ) +
  scale_color_manual(values=colors[c(2,3)])+
  geom_errorbar( aes(x=Model, ymin=lwr, ymax=upr), color =colors[2], linetype = "dashed",width=0.4)+
  facet_wrap(~N.samples,scales="free_y")+
  labs(x="Model",y="St.Dev",color="StDev",title="Fit for validation datasets, 95% Prediction Interval.")+
  theme_bw(base_size=20)

##### glm model
### since the outcome variable (sd) is positive and countinous we can use gamma or inverse gaussian model
model_glm<-glm(data=summary_res, formula=sd ~ N.samples+Model+MeanCorr+F4+N1, Gamma(link = "log"))
summary(model_glm)
exp(coef(model_glm))

plot(model_glm)

pred_rad_glm<-as.data.frame(predict.glm(model_glm,res_all_rad_summary,se.fit=TRUE))#)) #s

combo_glm<-cbind(res_all_rad_summary,"pred"=exp(pred_rad_glm$`predict.glm(model_glm, res_all_rad_summary)`))

combo_glm%>% ggplot() +
  geom_point( aes(x=as.factor(N.samples), y=sd, color="True"), size=3 ) +
  geom_point( aes(x=as.factor(N.samples), y=pred,color="Predicted"), size=3 ) +
  scale_color_manual(values=colors[c(2,3)])+
  #geom_errorbar( aes(x=as.factor(N.samples), ymin=fit-se.fitlwr, ymax=upr), color =colors[2], linetype = "dashed",width=0.4)+
  facet_wrap(~Model)+
  labs(x="N.samples",y="St.Dev",color="StDev",title="Fit for validation datasets")+
  theme_bw(base_size=20)

