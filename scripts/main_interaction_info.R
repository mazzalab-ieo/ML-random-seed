## compute score of complexity for a dataset. A sort of entropy for the matrix
library(GGally)
library(entropy)
library(infotheo)
library(ggpubr)

ggpairs(data_list[[11]][,-21])

#study features
prova<-discretize(data_list[[11]][,-21])
#interaction information
interinformation(prova, method="emp")

int_inf<-numeric()
#int_inf_train<-numeric()
#int_inf_test<-numeric()

for (i in c(1:length(samples)))
{
  print(i)
  disc_data<-discretize(data_list[[i]][,-21])
  int_inf<-c(int_inf,interinformation(disc_data, method="emp"))
  # 
  # for (j in seeds_1000)
  # {
  #   # for each dataset, separate the train and test set
  #   set.seed(j)
  #   
  #   default_idx = createDataPartition(data_list[[i]]$Label, p = 0.75, list = FALSE)
  #   train <- data_list[[i]][default_idx, ]
  #   test<- data_list[[i]][-default_idx, ]
  #   
  #   disc_train<-discretize(train[,-ncol(train)])
  #   disc_test<-discretize(test[,-ncol(test)])
  #   
  #   int_inf_train<-c(int_inf_train,interinformation(disc_train, method="emp"))
  #   int_inf_test<-c(int_inf_test,interinformation(disc_test, method="emp"))
  # }
}

## add to summary the info about interaction information
summary_auc_tot_res<-summary_auc_tot_res %>%
  mutate("Interaction_info"=int_inf)

model_ii<-lm(summary_auc_tot_res$sd ~ summary_auc_tot_res$Interaction_info)
fit_ii <- nls(sd ~ SSasymp(Interaction_info, yf, y0, log_alpha), data = summary_auc_tot_res)
fm1 <- lm(sd ~ splines::bs(Interaction_info, df = 3), data =summary_auc_tot_res)
summary(fm1)

fm2 <- lm(sd ~ Interaction_info + I(Interaction_info^2),data =summary_auc_tot_res)#poly(Interaction_info, 2), data =summary_auc_tot_res)
summary(fm2)

# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.012368 -0.004478 -0.001241  0.004903  0.011892 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           1.066e-01  1.517e-02   7.028 1.47e-06 ***
#   Interaction_info      2.448e-03  7.547e-04   3.243  0.00451 ** 
#   I(Interaction_info^2) 1.962e-05  8.699e-06   2.255  0.03681 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.007419 on 18 degrees of freedom
# Multiple R-squared:  0.7306,	Adjusted R-squared:  0.7007 
# F-statistic: 24.41 on 2 and 18 DF,  p-value: 7.467e-06

formula_fm2<-y~fm2$coefficients[1]+fm2$coefficients[2]*x+fm2$coefficients[3]*x^2

summary_auc_tot_res %>% ggplot()+
  geom_point(aes(x=Interaction_info,y=sd),size=3)+
  #geom_line(aes(x=Interaction_info,y=sd), linewidth=1.1)+
  #geom_smooth(aes(x=Interaction_info,y=sd),method="lm",formula = y ~ splines::bs(x, 3),color="blue")+
  geom_smooth(aes(x=Interaction_info,y=sd),method="lm",formula=(y~poly(x, 2)),color="blue")+
  #geom_line(aes(x=Interaction_info,y=fm2$coefficients[1]+fm2$coefficients[2]*Interaction_info+fm2$coefficients[3]*Interaction_info^2),color="red", linewidth=1.1,linetype=2)+
  labs(y="St.Dev(AUC)",title="Standard deviation of AUC over interaction information",color="Fit")+ #c("y=exp(x^-1)","y=a=b*exp(-cx)")
  theme_bw(base_size=20)

#total model:

# summary_auc_tot_res %>% ggplot()+
#   geom_point(aes(x = N.samples, y = Interaction_info),size=3)+
#   geom_line(aes(x = N.samples, y = Interaction_info), linewidth=1.1)+
#   geom_smooth(aes(x=N.samples,y=Interaction_info),method="lm")+
#   labs(y="Interaction information",title="Correlation interaction information and sample size",color="Fit")+ #c("y=exp(x^-1)","y=a=b*exp(-cx)")
#   theme_bw(base_size=20)

summary_auc_tot_res %>%
  ggscatter(x = "N.samples", y = "Interaction_info",
          add = "reg.line",                                 # Add regression line
          conf.int = TRUE,                                  # Add confidence interval
          add.params = list(color = "blue",fill = "lightgray"),
          title="Interaction information VS sample size",ylab ="Interaction information" )+
  stat_cor(method = "pearson", label.x = 400, label.y = -20) + # Add correlation coefficient
  theme_bw(base_size=20)


### model final
yf<- 0.023935
y0<-0.113441

a<-yf
b<-y0-yf
formula_fm1<-y~a+b*exp(-alpha*x)

##### total model
model_tot<-lm(data=summary_auc_tot_res, sd~log(exp(-N.samples)+Interaction_info+Interaction_info^2))
summary(model_tot)

# Residuals:
#   Min         1Q     Median         3Q        Max 
# -0.0134244 -0.0044556  0.0009672  0.0047883  0.0126964 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)                                                   0.155632   0.016483   9.442 1.32e-08
# log(exp(-N.samples) + Interaction_info + Interaction_info^2) -0.015470   0.002237  -6.917 1.35e-06
# 
# (Intercept)                                                  ***
#   log(exp(-N.samples) + Interaction_info + Interaction_info^2) ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.007418 on 19 degrees of freedom
# Multiple R-squared:  0.7158,	Adjusted R-squared:  0.7008 
# F-statistic: 47.85 on 1 and 19 DF,  p-value: 1.354e-06

xnew <- data.frame(N.samples=seq(100, 500, by = 10))
p <- predict(fit,newdata=xnew, se = TRUE,interval="confidence")

g1 <- data.frame(N.samples= xnew$N.samples, 
                fit = p,
                lwr = p - 1.96*sigma(fit), 
                upr = p + 1.96*sigma(fit))

# total model
summary_auc_tot_res <-summary_auc_tot_res %>%
  mutate(new_var=a+b*exp(-alpha*N.samples)*(fm2$coefficients[1]+fm2$coefficients[2]*Interaction_info+fm2$coefficients[3]*(Interaction_info^2))) %>%
  mutate(new_var2=a+b*exp(-alpha*N.samples)+(fm2$coefficients[1]+fm2$coefficients[2]*Interaction_info+fm2$coefficients[3]*(Interaction_info^2)))%>%
  mutate(new_var3=exp(-N.samples)+Interaction_info+Interaction_info^2)

summary_auc_tot_res %>% ggplot()+
  geom_point(aes(x=new_var3,y=sd),size=3)+
  #geom_line(aes(x=new_var3,y=sd), linewidth=1.1)+
  geom_smooth(aes(x=new_var3,y=sd),method="lm",formula=y~log(x),color="blue")+
  #geom_line(aes(x=new_var3,y=model_tot$coefficients[1]+model_tot$coefficients[2]*log(new_var3)),color="red",linetype=2)+
  labs(y="St.Dev(AUC)",x="f(N.samples,Interaction info)",title="Complete model",color="Fit")+ #c("y=exp(x^-1)","y=a=b*exp(-cx)")
  theme_bw(base_size=20)
