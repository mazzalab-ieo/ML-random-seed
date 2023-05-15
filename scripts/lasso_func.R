#### function lasso (so we can always use this one)
lasso_func<-function(seeds,data_list, torem) #samples,
{
  library(tidyverse)
  library(caret)
  library(glmnet)
  library(pROC)
  
  samples<-as.numeric(lapply(data_list,nrow))
  print(samples)
  
  lambda<-numeric()
  auc_train<-numeric()
  auc_test<-numeric()
  ncoef<-numeric()
  
  total_res<-data.frame("Seed"=seeds,"N.samples"=rep(samples,each=length(seeds)))
  total_coef<-list()
  
  for (i in c(1:length(samples)))
  {
    print(i)
    nf<-ifelse(samples[i]<100,3,5)
    #print(nf)
    
    lab<-which(colnames(data_list[[i]])=="Label")
    if (torem>0)
      ll<-c(lab,torem)
    else
      ll<-lab
    
    listcoef<-data.frame("Feature"=c("Intercept",colnames(data_list[[i]])[-ll]))
    #print(listcoef)
    
    pb <- txtProgressBar(0, length(seeds), style = 3)
    for (j in seeds)
    {
      # for each dataset, separate the train and test set
      set.seed(j)
      
      default_idx <- createDataPartition(data_list[[i]]$Label, p = 0.75, list = FALSE)
      if (torem==0)
      {
        train <- data_list[[i]][default_idx, ]
        test<- data_list[[i]][-default_idx, ]
      } else
      {
        train <- data_list[[i]][default_idx, -torem]
        test<- data_list[[i]][-default_idx, -torem]
      }
      lab2<-which(colnames(train)=="Label")
      
      # scale the two datasets: useless, since glmnet already does it by its own
      #train[,-ncol(train)]<-apply(train[,-ncol(train)],2,scale)
      #test[,-ncol(train)]<-apply(test[,-ncol(test)],2,scale)
      
      #apply method
      set.seed(j)
      cv_lasso<-cv.glmnet(data.matrix(train[,-lab2]), train$Label, family = "binomial", 
                          alpha = 1,type.measure="auc",nfolds =nf) #10
      
      #compute lambda selected
      lambda<-c(lambda,cv_lasso$lambda.min)
      
      #compute complete model
      model <- glmnet(data.matrix(train[,-lab2]), train$Label, alpha = 1, 
                      family = "binomial",type.measure="auc",
                      lambda = cv_lasso$lambda.min)
      #apply on test set
      prob <- model %>% predict(newx = data.matrix(test[,-lab2]))
      pred<- ifelse(as.numeric(prob) > 0.5, 1, 0)
      
      #coefs
      coef<-as.vector(coef(model))
      #print(coef)
      
      listcoef<-cbind(listcoef,coef)
      #number of coefs different from 0
      ncoef<-c(ncoef,length(which(!(coef==0))))
      
      #auc on train set
      auc_train<-c(auc_train,max(cv_lasso$cvm))
      #auc on test set
      auc_t<-auc(roc(test$Label, pred,quiet=TRUE))
      auc_test<-c(auc_test,auc_t)
      
      setTxtProgressBar(pb, j)
      
    }
    total_coef[[i]]<-listcoef
  }
  close(pb)

  total_res<-cbind(total_res,lambda,ncoef,auc_train,auc_test)
  
  return(list(total_res,total_coef))
}