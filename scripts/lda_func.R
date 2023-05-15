lda_func<-function(seeds,dataset,samples,overlap)
{
  #n: n_datasets
  library(caret)
  source('~/Desktop/Instability_ML/scripts/train_test_split.R')
  n<-length(samples)
  ls<-length(seeds)
  #define the nunber of unique classes
  nclass<-length(unique(dataset[[1]]$Label))
  
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
      #print(j)
      split1<-train_test_split(dataset[[i]],seeds[j])
      train<-split1$train
      train$Label<-as.factor(train$Label)
      test<-split1$test
      test$Label<-as.factor(test$Label)
      
      # logistic regression
      lda<-train(Label~ ., data = train, method = "lda")
      #predictions
      predicted.classes<-predict(lda, newdata=test)
      
      cm<-confusionMatrix(factor(predicted.classes, levels=seq(0,nclass-1)),
                          factor(test$Label, levels=seq(0,nclass-1)))
      
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