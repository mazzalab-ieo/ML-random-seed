create_dataset<-function(mixsim, samples)
{
  library(MixSim)
  
  data_list<-list()
  j<-0
  
  for (i in samples)
  {
    j<-j+1
    #print(i)
    
    # create datasets
    d_s<-simdataset(i, Pi=mixsim$Pi, Mu=mixsim$Mu, S=mixsim$S)
    
    data<-as.data.frame(d_s$X)
    data<-cbind(data,Label=d_s$id)
    
    data$Label<-data$Label-1

    data_list[[j]]<-data
    
    #print(table(data$Label)/i)
  }
  #return list
  return(data_list)
}