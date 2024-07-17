# creation of new and better datasets

library(clusterGeneration)
library(tidyverse)
library(purrr)

source("~/ML-random-seed/scripts/internal.R")
source("~/ML-random-seed/scripts/overlapping_my.R")
source("~/ML-random-seed/scripts/neighborhood_my.R")
source("~/ML-random-seed/scripts/linearity.class_my.R")
source("~/ML-random-seed/scripts/complexity_my.R")

#save data in specific folder
setwd(paste0(getwd(), "/data_simulated_Feb24"))

tmp <- simClustDesign(
  numClust = 2,
  sepVal = c(0.01,0.21,0.342), #low, medium and high separation
  sepLabels = c("L","M","H"),
  numNonNoisy = seq(10,50, by=10),
  numNoisy = c(5,10),
  #numOutlier =1/10,
  numReplicate = 10,
  rangeN = c(30,200),
  outputLogFlag = FALSE,
  outputEmpirical = FALSE,
  outputInfo = FALSE)

#check dimension of the produced datasets
dims<-numeric()
for (i in 1:length(tmp$datList))
  dims<-c(dims,as.numeric(lapply(tmp$datList[[i]], nrow)))

plot(density(dims))
range(dims)

# now compose the variables with the class
### for each dataset I want: number of samples, number of variables, ratio 0/1, 
dataSim<-list()
filenames<-character()
nfeat<-numeric()
nsample<-numeric()
frac0<-numeric()
k<-0
for (i in 1:length(tmp$datList))
{
  filenames<-c(filenames,names(tmp$memList[[i]]))
  for (j in 1:length(tmp$datList[[i]]))
  {
    k<-k+1
    clusts<-tmp$memList[[i]][[j]]-1
    dataSim[[k]]<-as.data.frame(cbind(tmp$datList[[i]][[j]],clusts))
    #dataSim[[k]]$Dataset<-paste("ds_",k,sep="")
    
    nc<-ncol(dataSim[[k]])
    colnames(dataSim[[k]])<-c(paste("V",seq(1,nc-2),sep=""),"Label","Dataset")
    
    nfeat<-c(nfeat,nc-2)
    nsample<-c(nsample,nrow(dataSim[[k]]))
    frac0<-c(frac0,(table(dataSim[[k]]$Label)[1]/nrow(dataSim[[k]])*100))
  }
}
info_data<-data.frame(Filename=filenames,nFeatures=nfeat,nSamples=nsample,Perc0=frac0)
info_data$Dataset<-seq(1,k)
info_data$Overlap<-rep(c("Close","Medium-separated","Well-separated"),each=100)
### change some of the variables in binary or categorical ones.
# select different thresholds to divide the binary var and the categorical ones

for (i in 1:length(dataSim))
{
  print(i)
  percbin<-floor((ncol(dataSim[[i]])-2)*0.25)
  perccat<-floor((ncol(dataSim[[i]])-2)*0.1)
  
  #print(c(percbin,perccat))
  random_bin<-sample(seq(1,ncol(dataSim[[i]])-2),percbin)
  random_cat<-sample(seq(1,ncol(dataSim[[i]])-2)[-random_bin],perccat)
  
  lb<-percbin/5
  lc<-perccat/2
  
  qb<-rep(c(2,3,5,-4,4),ceiling(lb))
  qc<-rep(c(2,3),ceiling(lc))
  
  for (j in 1:length(random_bin))
  {
    el<-random_bin[j]
    q<-qb[j]
    #I want alternatively the 1st quartile, the median the 3rd quartile and the mean
    if (q>0)
      dataSim[[i]][,el]<-ifelse(dataSim[[i]][,el]>=summary(dataSim[[i]][,el])[q],1,0)
    else #inverse correlation
      dataSim[[i]][,el]<-ifelse(dataSim[[i]][,el]>=summary(dataSim[[i]][,el])[abs(q)],0,1)
  }
  
  for(j in 1:length(random_cat))
  {
    el<-random_cat[j]
    q<-qc[j]
    
    dataSim[[i]][,el]<-ifelse(dataSim[[i]][,el]<summary(dataSim[[i]][,el])[q],0,
                                          ifelse(dataSim[[i]][,el]>=summary(dataSim[[i]][,el])[5],2,1))
  }
}

### compute the complexity of the datasets
#dataSim<-lapply(dataSim, function(x) {x<-x %>% select(-Dataset)})
ds_complexity<-lapply(dataSim, function(x) complexity_my(Label ~ ., x))

# transform in data frame
ds_complexity_all<-map_dfr(ds_complexity, ~as.data.frame(t(.x)))
ds_complexity_all$Dataset<-seq(1,300)

ds_complexity_all_2<- ds_complexity_all %>%
  select(overlapping_my.F1,overlapping_my.F1v,overlapping_my.F2,overlapping_my.F3,overlapping_my.F4,
         neighborhood_my.N1,neighborhood_my.N2,neighborhood_my.N3,neighborhood_my.N4,neighborhood_my.T1,
         linearity.class_my.L1,linearity.class_my.L2,linearity.class_my.L3) %>%
  gather(Measure, Value, overlapping_my.F1:linearity.class_my.L3, factor_key=TRUE)

#compute average correlation
mean_corr<-as.numeric(lapply(dataSim,function(x) mean(abs(cor(x[,-ncol(x)])))))

### save files to folder
#for (i in seq(1,length(nsample)))
#  write.csv(dataSim[[i]],paste('training/simulated_',i,'.csv',sep=""))

#calculating a separation index for all the datasets