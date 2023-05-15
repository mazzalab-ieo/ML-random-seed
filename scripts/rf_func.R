#### function rf for now only test number of features (so we can always use this one)
rf_func<-function(seeds,data_list,torem)
{
  library(tidyverse)
  library(caret)
  library(randomForest)
  library(pROC)
  
  samples<-as.numeric(lapply(data_list,nrow))
  print(samples)
  
  parameter<-numeric()
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
    
    listcoef<-data.frame("Feature"=colnames(data_list[[i]])[-ll])
    
    pb <- txtProgressBar(0, length(seeds), style = 3)
    for (j in seeds)
    {
      # for each dataset, separate the train and test set
      set.seed(j)
      
      default_idx = createDataPartition(data_list[[i]]$Label, p = 0.75, list = FALSE)
      if (torem==0)
      {
        train <- data_list[[i]][default_idx, ]
        test<- data_list[[i]][-default_idx, ]
      } else
      {
        train <- data_list[[i]][default_idx, -torem]
        test<- data_list[[i]][-default_idx, -torem]
      }
      
      ## random forest ###finish
      #we are intereseted in XXX search
      # do we want to train on mtry, ntree or nothing??
      set.seed(j)
      fitControl <- trainControl(method = "cv",number = nf)#,search=)

      rf <- train(as.factor(Label) ~ ., data = train,
                  method = "rf",
                  trControl = fitControl,
                  #metric = "ROC",
                  tuneGrid = expand.grid(mtry = expand.grid(.mtry=c(1:10))), #15
                  verbose = FALSE)
      
      #compute lambda selected
      parameter<-c(parameter,rf$bestTune$mtry)
      
      imp <- varImp(rf,scale=TRUE)
      imp_ov<-imp$importance$Overall
      
      listcoef<-cbind(listcoef,imp_ov)
      
      #auc on train set
      pred_train<-as.numeric(rf %>% predict(train))
      auc_tr<-auc(roc(train$Label, pred_train,quiet=TRUE))
      auc_train<-c(auc_train, auc_tr)
      
      #auc on test set
      pred <- as.numeric(rf %>% predict(test))
      #pred<- ifelse(as.numeric(prob) > 0.5, 1, 0)
      auc_t<-auc(roc(test$Label, pred,quiet=TRUE))
      auc_test<-c(auc_test,auc_t)
      
      setTxtProgressBar(pb, j)
      
    }
    total_coef[[i]]<-listcoef
  }
  close(pb)
  
  #per rf dico che uttte le feature sono state selezionate
  #poi con total_coef salvo le più importanti
  ncoef<-rep(ncol(data_list[[i]])-1,nrow(total_res))
  total_res<-cbind(total_res,parameter, ncoef,auc_train, auc_test)
  
  return(list(total_res,total_coef))
}
