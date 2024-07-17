#### function svm for now only linear (so we can always use this one)
svm_func<-function(seeds,data_list,torem,case) #samples,
{
  #torem: extra features tp remove a part Label and real features
  # the default is svm with L2 regularization with hinge loss
  
  library(tidyverse)
  library(caret)
  library(pROC)
  
  samples<-as.numeric(lapply(data_list,nrow))
  print(samples)
  
  parameter<-numeric()
  auc_train<-numeric()
  auc_test<-numeric()
  ncoef<-numeric()
  
  total_res<-data.frame("Seed"=seeds,"N.samples"=rep(samples,each=length(seeds)),
                        "Dataset"=case)
  total_coef<-list()
  
  for (i in c(1:length(samples)))
  {
    print(samples[i])
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
      
      #svm
      # scale the two datasets in the model
      set.seed(j)
      fitControl <- trainControl(method = "cv",number = nf)#,search=)
      
      svml <- train(as.factor(Label) ~ ., data = train, 
                  method = "svmLinear",
                  preProcess = c("center","scale"),
                  trControl = fitControl,
                  tuneGrid = expand.grid(C = seq(0, 5, length = 50)[-1]),
                  #tuneLength=10,
                  verbose = FALSE)
      
      #compute lambda selected
      parameter<-c(parameter,svml$bestTune$C)
      
      coefs <- svml$finalModel@coef[[1]]
      mat <- svml$finalModel@xmatrix[[1]]
      
      weights<-coefs %*% mat
      listcoef<-cbind(listcoef,weights[1,])
      
      #auc on train set
      pred_train<-as.numeric(svml %>% predict(train))
      auc_tr<-auc(roc(train$Label, pred_train,quiet=TRUE))
      auc_train<-c(auc_train, auc_tr)
      
      #auc on test set
      pred <- as.numeric(svml %>% predict(test))
      #pred<- ifelse(as.numeric(prob) > 0.5, 1, 0)
      auc_t<-auc(roc(test$Label, pred,quiet=TRUE))
      auc_test<-c(auc_test,auc_t)
      
      setTxtProgressBar(pb, j)
      
    }
    total_coef[[i]]<-listcoef
  }
  close(pb)
  
  #per svm dico che tutte le feature sono state selezionate
  #poi con total_coef salvo le più importanti
  ncoef<-rep(nrow(listcoef),nrow(total_res))
  total_res<-cbind(total_res,parameter, ncoef,auc_train, auc_test)
  
  return(list(total_res,total_coef))
}
  