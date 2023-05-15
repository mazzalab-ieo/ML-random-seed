Score_correlation<-function(data,label,thr)
{
  #label: which column is the label to use?
  #thr: threshold to consider correlation high
  # we provide points assigned to couples of features according to the pvalues and correlation
  
  library(tidyverse)
  source("~/Desktop/Instability_ML/scripts/flattenCorMatrix.R")
  
  #compute correlation between features and output
  pval_1<-apply(data[,-label],2, function(x) wilcox.test(x,data$Label)$p.value)
  
  cor_1<-cor(data[,-label])
  cor_1_flat<-flattenCorrMatrix(cor_1)
  
  pvalf1<-numeric()
  pvalf2<-numeric()
  for (i in 1:nrow(cor_1_flat))
  {
    #print(i)
    pvalf1<-c(pvalf1,pval_1[which(names(pval_1)==cor_1_flat$row[i])])
    pvalf2<-c(pvalf2,pval_1[which(names(pval_1)==cor_1_flat$column[i])])
  }
  
  cor_1_flat$Pval_F1<-pvalf1
  cor_1_flat$Pval_F2<-pvalf2
  
  #assign points to couples
  cor_1_flat <-cor_1_flat %>% mutate(points=case_when((Pval_F1<0.05 & Pval_F2<0.05 & abs(cor)<thr)~1,
                                                      (Pval_F1<0.05 & Pval_F2<0.05 & abs(cor)>=thr)~0.5,
                                                      (Pval_F1<0.05 & Pval_F2>=0.05 & abs(cor)<thr)~0.75,
                                                      (Pval_F1<0.05 & Pval_F2>=0.05 & abs(cor)>=thr)~0.5,
                                                      (Pval_F1>=0.05 & Pval_F2<0.05 & abs(cor)<thr)~0.75,
                                                      (Pval_F1>=0.05 & Pval_F2<0.05 & abs(cor)>=thr)~0.5,
                                                      (Pval_F1>=0.05 & Pval_F2>=0.05 & abs(cor)<thr)~0,
                                                      (Pval_F1>=0.05 & Pval_F2>=0.05 & abs(cor)>=thr)~0))
  
  colnames(cor_1_flat)[c(1,2)]<-c("Feature_1","Feature_2")
  score<-sum(cor_1_flat$points)
  
  #we can normalize it on the maximum we could reach, that is if all the features were informative (all couples have 1 point)
  score_norm<-score/nrow(cor_1_flat)
  
  output<-list(score_norm,cor_1_flat)
  return(output)
}