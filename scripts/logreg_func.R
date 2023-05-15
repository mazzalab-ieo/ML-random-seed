logreg_func<-function(seeds,dataset,samples,overlap)
{
  #n: n_datasets
  library(caret)
  source('~/Desktop/Instability_ML/scripts/train_test_split.R')
  n<-length(samples)
  ls<-length(seeds)
  
  dim_tot<-n*ls
  total_res<-data.frame("Seed"=seeds,"N.samples"=rep(samples,each=ls),"Overlap"=rep(overlap,dim_tot))
  acc_test<-numeric()
  sens_test<-numeric()
  spec_test<-numeric()
  prec_test<-numeric()
  f1_test<-numeric()
  
  for (i in 1:n)
  {
    print(i)
    
    for(j in 1:ls)
    {
      split1<-train_test_split(dataset[[i]],seeds[j])
      train<-split1$train
      train$Label<-as.factor(train$Label)
      test<-split1$test
      test$Label<-as.factor(test$Label)
      
      # logistic regression
      logreg<-train(Label~ ., data = train, method = "glm")
      #predictions
      probabilities<-predict(logreg, newdata=test, type = "prob")[2]
      predicted.classes <- as.factor(ifelse(probabilities > 0.5, "1", "0"))
      
      cm<-confusionMatrix(predicted.classes, test$Label)
      
      # Measures
      acc_test<-c(acc_test,cm$overall[1])
      spec_test<-c(spec_test,cm$byClass[2])
      sens_test<-c(sens_test,cm$byClass[1])
      prec_test<-c(prec_test,cm$byClass[5])
      f1_test<-c(f1_test,cm$byClass[7])

    }
  }
  total_res<-cbind(total_res,acc_test,spec_test,sens_test,prec_test,f1_test)
  
  return(total_res)
}