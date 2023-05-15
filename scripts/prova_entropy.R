library(DescTools)
library(entropy)
library(tidyverse)

entropy<-data.frame("Case"=seq(1,length(data_list)),"N_sample"=samples)
m_e<-numeric()                   
sum_e<-numeric()   

for (i in 1:length(data_list))
{
  print(nrow(data_list[[i]]))
  e<-c()
  for (j in 1:20)
    {
    v1_d<-discretize(data_list[[i]][,j], 100)
    e<-c(e,entropy(v1_d))
    }
  print(e)
  m_e<-c(m_e,mean(e))
  sum_e<-c(sum_e,sum(e))
}

entropy$Mean_H<-m_e
entropy$Sum_H<-sum_e

#add to summary the info aboutaverage  entropy
summary_auc_tot_res<-summary_auc_tot_res %>%
  mutate("H_average"=m_e, "H_sum"=sum_e)

# sum
summary_auc_tot_res %>% ggplot()+
  geom_point(aes(x=H_sum,y=sd),size=3)+
  #geom_line(aes(x=H_sum,y=sd), linewidth=1.1)+
  geom_smooth(aes(x=H_sum,y=sd),method="lm",formula=( y~ splines::bs(x, 3)))+
  #geom_smooth(aes(x=Interaction_info,y=sd),method="lm",formula=(y~poly(x, 2)),color="blue")+
  #geom_line(aes(x=Interaction_info,y=fm2$coefficients[1]+fm2$coefficients[2]*Interaction_info+fm2$coefficients[3]*Interaction_info^2),color="red", linewidth=1.1,linetype=2)+
  labs(y="St.Dev(AUC)",title="Standard deviation of AUC over sum entropy",color="Fit")+ #c("y=exp(x^-1)","y=a=b*exp(-cx)")
  theme_bw(base_size=20)

## average
summary_auc_tot_res %>% ggplot()+
  geom_point(aes(x=H_average,y=sd),size=3)+
  #geom_line(aes(x=H_average,y=sd), linewidth=1.1)+
  geom_smooth(aes(x=H_average,y=sd),method="lm",formula=( y~ splines::bs(x, 3)))+
  #geom_smooth(aes(x=Interaction_info,y=sd),method="lm",formula=(y~poly(x, 2)),color="blue")+
  #geom_line(aes(x=Interaction_info,y=fm2$coefficients[1]+fm2$coefficients[2]*Interaction_info+fm2$coefficients[3]*Interaction_info^2),color="red", linewidth=1.1,linetype=2)+
  labs(y="St.Dev(AUC)",title="Standard deviation of AUC over average entropy",color="Fit")+ #c("y=exp(x^-1)","y=a=b*exp(-cx)")
  theme_bw(base_size=20)

fm_h <- lm(sd ~ splines::bs(H_average, df = 3), data =summary_auc_tot_res)
summary(fm_h)

# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.008873 -0.004255 -0.002141  0.005867  0.012344 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      0.068129   0.005096  13.370 1.89e-10 ***
#   splines::bs(H_average, df = 3)1  0.016740   0.020708   0.808   0.4300    
# splines::bs(H_average, df = 3)2 -0.033156   0.013871  -2.390   0.0287 *  
#   splines::bs(H_average, df = 3)3 -0.035298   0.006530  -5.405 4.74e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.006949 on 17 degrees of freedom
# Multiple R-squared:  0.7769,	Adjusted R-squared:  0.7375 
# F-statistic: 19.73 on 3 and 17 DF,  p-value: 8.917e-06

fit_he <- nls(sd ~ I(a * exp(b * H_average)),start = list(a =1, b = 0), data = summary_auc_tot_res)
summary(fit_he)
RSS.p <- sum(residuals(fit_he)^2)
TSS <- sum((summary_auc_tot_res$sd - mean(summary_auc_tot_res$sd))^2)
1 - (RSS.p/TSS)


# total model
summary_auc_tot_res <-summary_auc_tot_res %>%
  mutate(new_var=a+b*exp(-alpha*N.samples)*(fm2$coefficients[1]+fm2$coefficients[2]*Interaction_info+fm2$coefficients[3]*(Interaction_info^2))) %>%
  mutate(new_var2=a+b*exp(-alpha*N.samples)+(fm2$coefficients[1]+fm2$coefficients[2]*Interaction_info+fm2$coefficients[3]*(Interaction_info^2)))%>%
  mutate(new_var3=exp(-N.samples)+Interaction_info+Interaction_info^2)


library(rinform)
xs<-apply(data_list[[1]][,-21],2, function(x) discretize(x, 100))
mutual_info(xs)

