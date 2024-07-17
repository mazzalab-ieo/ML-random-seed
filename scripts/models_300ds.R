#### model for new datasets
#start with a linear model for RF. Fit a model, so divide in training and test set
load("~/ML-random-seed/data/workspace_160224.RData")

library(ciTools)
nt<-75*nrow(summary_rf)/100

srf<-sample(seq(1,nrow(summary_rf)),nt)

#summary_rf$sqrt_sd<-sqrt(summary_rf$sd)
#train
rf_model_1<-lm(data=summary_rf[srf,], sd ~ N.samples+nFeatures+Perc0+F4+N1+MeanCorr)
summary(rf_model_1)
extractAIC(rf_model_1)
par(mfrow = c(2, 2))
plot(rf_model_1)

rf_model_2<-lm(data=summary_rf[srf,], sqrt_sd ~ N.samples+nFeatures+Perc0+F4+N1+MeanCorr)
summary(rf_model_2)
extractAIC(rf_model_2)
plot(rf_model_2)

rf_model_3<-lm(data=summary_rf[srf,], sqrt(sd) ~ I(1/N.samples)+nFeatures+poly(Perc0,2)+F4+N1+MeanCorr)
summary(rf_model_3)
extractAIC(rf_model_3)
plot(rf_model_3)

rf_model_4<-lm(data=summary_rf[srf,], log(sqrt(sd)+1) ~ N.samples+nFeatures+Perc0+F4+N1+MeanCorr)
summary(rf_model_4)
extractAIC(rf_model_4)
plot(rf_model_4)

#calculate prediction interval for model 2 and 3 on the training set!!!
train_1<-add_pi(summary_rf[srf,], rf_model_1)
train_1$model<-"lm_1"
train_2<-add_pi(summary_rf[srf,], rf_model_2)
train_2$model<-"lm_2"
train_3<-add_pi(summary_rf[srf,], rf_model_3)
train_3$model<-"lm_3"
train_3<-add_pi(summary_rf[srf,], rf_model_4)
train_3$model<-"lm_4"

# estimate stdev of yhat
#sum_errs = sarraysum((y - yhat)**2)
#stdev = sqrt(1/(len(y)-2) * sum_errs)
# calculate prediction interval
#interval = 1.96 * stdev
train_lm<-rbind(train_1,train_2,train_3)

pred_int<-train_lm %>% mutate(pred_2=ifelse(model=="lm_1",pred,pred^2)) %>% 
  group_by(model) %>%
  summarize(interval=1.96*sqrt(1/(nrow(train_1)-2)*sum((sd-pred_2)^2)),
            R2 = cor(sd, pred_2)^2,
            MSE = mean((sd - pred_2)^2),
            RMSE = sqrt(MSE),
            MAE = mean(abs(sd- pred_2))
            )

#add pi computes the prediction and the UP and LOW referred to the modeled variable, 
#thus sqrt(sd) for models 2 and 3
#to get those on sd, we should recompute them!
test_1<-add_pi(summary_rf[-srf,], rf_model_1)
test_1$model<-"lm_1"
test_2<-add_pi(summary_rf[-srf,], rf_model_2)
test_2$model<-"lm_2"
test_3<-add_pi(summary_rf[-srf,], rf_model_3)
test_3$model<-"lm_3"
test_4<-add_pi(summary_rf[-srf,], rf_model_4)
test_4$model<-"lm_4"

test_lm<-rbind(test_1,test_2,test_3,test_4)

test_lm<-test_lm %>% mutate(pred_2=ifelse(model=="lm_1",pred,pred^2))
test_lm<-left_join(test_lm,pred_int[,c("model","interval")],by="model")
test_lm <- test_lm %>% mutate(UPB0.975_2=pred_2+interval,
                              LPB0.025_2=pred_2-interval,
                              PI_width_1=UPB0.975-LPB0.025,
                              PI_width=UPB0.975_2-LPB0.025_2)

test_lm_summary<-test_lm %>%
  group_by(model) %>%
  summarize(
    R2 = cor(sd, pred_2)^2,
    MSE = mean((sd - pred_2)^2),
    RMSE = sqrt(MSE),
    MAE = mean(abs(sd- pred_2)),
    PIwidth=mean(PI_width)
  )

#read validation set
summary_validation_sets <- read.csv("~/ML-random-seed/summary_validation_sets.csv")

summary_validation_sets$sqrt_sd<-sqrt(summary_validation_sets$sd)
val_1<-add_pi(summary_validation_sets %>% filter(Model=="RF"), rf_model_1)
val_1$model<-"lm_1"
val_2<-add_pi(summary_validation_sets%>% filter(Model=="RF"), rf_model_2)
val_2$model<-"lm_2"
val_3<-add_pi(summary_validation_sets%>% filter(Model=="RF"), rf_model_3)
val_3$model<-"lm_3"
val_4<-add_pi(summary_validation_sets%>% filter(Model=="RF"), rf_model_4)
val_4$model<-"lm_4"

val_lm_rf<-rbind(val_1,val_2,val_3,val_4)

val_lm_rf<-val_lm_rf %>% 
  mutate(pred_2=ifelse(model=="lm_1",pred,pred^2))
         
val_lm_rf<-left_join(val_lm_rf,pred_int[,c("model","interval")],by="model")
val_lm_rf<- val_lm_rf %>% mutate(UPB0.975_2=pred_2+interval,
                                       LPB0.025_2=pred_2-interval,
                                       PI_width=UPB0.975_2-LPB0.025_2,
                                       PI_width_1=UPB0.975-LPB0.025)

val_lm_rf_summary<-val_lm_rf %>%
  group_by(Case,model) %>%
  summarize(
    R2 = cor(sd, pred_2)^2,
    MSE = mean((sd - pred_2)^2),
    RMSE = sqrt(MSE),
    MAE = mean(abs(sd- pred_2)),
    PIwidth=mean(PI_width)
  )

val_lm_rf %>% filter(model=="lm_3") %>%
  ggplot()+
  geom_point( aes(x=Case, y=pred,color="Estimate"), size= 4, shape= 4) +
  geom_point( aes(x=Case, y=sqrt(sd), color="True"), size=3) +
  scale_color_manual(values=c('black','#E69F00'))+#colors[c(2,3)])+
  #ylim(c(0.65,1.35))+
  geom_errorbar( aes(x=Case, ymin=LPB0.025, ymax=UPB0.975), color ='black', linetype = "dashed",width=0.5)+
  coord_flip()+
  labs(x="Cases",y="St.Dev",color="St.Dev",title="RF, validation sets")+
  guides(color=FALSE)+
  theme_bw(base_size=15)

val_lm_rf %>% filter(model=="lm_4") %>%
  ggplot()+
  geom_point( aes(x=Case, y=pred,color="Estimate"), size= 4, shape= 4) +
  geom_point( aes(x=Case, y=sd, color="True"), size=3) +
  scale_color_manual(values=c('black','#E69F00'))+#colors[c(2,3)])+
  #ylim(c(0.65,1.35))+
  geom_errorbar( aes(x=Case, ymin=LPB0.025, ymax=UPB0.975), color ='black', linetype = "dashed",width=0.5)+
  coord_flip()+
  labs(x="Cases",y="St.Dev",color="St.Dev",title="RF, validation sets")+
  guides(color=FALSE)+
  theme_bw(base_size=15)

###glm? which are the hypothesis??
### since the outcome variable (sd) is NON NEGATIVE and countinous we can use gamma or inverse gaussian model
# gamma has the prediciton intervals!

var(summary_rf$sd)

rf_glm_1<-glm(data=summary_rf, formula= sd ~ N.samples+nFeatures+Perc0+F4+N1+MeanCorr,family=tweedie(var.power=1.5,link.power=0))

summary(rf_glm_1)
exp(coef(rf_glm_1))

plot(rf_glm_1)

test_glm_1<-add_pi(summary_rf[-srf,], rf_glm_1)

add_pi(summary_validation_sets %>% filter(Model=="RF"), rf_glm_1)
