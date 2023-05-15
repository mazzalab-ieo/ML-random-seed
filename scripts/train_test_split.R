train_test_split<-function(data,seed,proportion)
{
  #library(caTools)
  library(caret)
  
  set.seed(seed) 
  
  #sample = sample.split(data$Label, SplitRatio = .70)
  #train = subset(data, sample == TRUE)
  #test  = subset(data, sample == FALSE)
  
  trainIndex <- createDataPartition(data$Label, p = proportion, 
                                    list = FALSE, 
                                    times = 1)
  train<-data[trainIndex,]
  test<-data[-trainIndex,]
  
  splitted<-list("train"=train,"test"=test)
  return(splitted)
}