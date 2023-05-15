library(MixSim)
library(caret)
library(reshape2)
library(tidyverse)
library(RColorBrewer)
library(viridis)
library(GGally)
library(infotheo)
library(EDMeasure)

source("~/Desktop/Instability_ML/scripts/create_dataset.R")
source('~/Desktop/Instability_ML/scripts/train_test_split.R')

### built a unique dataset, 2classes and then, from 60 to 1000 samples
set.seed(394)
mix_start_2<-MixSim(BarOmega = 0.1,K = 2, p = 20)

data_2<-create_dataset(mix_start_2,1000)[[1]]
tab<-as.data.frame(table(data_2$Label))

samples<-seq(60,1000, by=20)
ntimes<-(max(samples)-min(samples))/20 #20 is the increase step 
n_datasets<-length(samples)

### DEVO CREARE DATASET INCREMENTALI, NON TOTALMENTE SCOLLEGATI!!!!
f0<-(tab$Freq[1]/1000)*100
#first small dataset
c0<-round((min(samples)/100)*f0)
c1<-(min(samples))-c0

d0<-seq(1,c0) #first group with label 0
d1<-seq(tab$Freq[1]+1,tab$Freq[1]+c1) #first group with label 1

data_list_2<-list()
data_list_2[[1]]<-data_2[c(d0,d1),]

d0<-split(seq(c0+1,tab$Freq[1]), sort(seq(c0+1,tab$Freq[1])%%ntimes)) #create groups with label 0, 
d1<-split(seq(tab$Freq[1]+c1+1,max(samples)), sort(seq(tab$Freq[1]+c1+1,max(samples))%%ntimes)) #create groups with label 1

cent<-floor(length(seq(c0+1,tab$Freq[1]))/ntimes)
ceil<-ceiling(length(seq(c0+1,tab$Freq[1]))/ntimes)
floor<-20-ceil

all<-seq(floor,ceil)
# check the dimension of groups
#  as.numeric(lapply(d1,length))+as.numeric(lapply(d0,length))

#create couples to have always the same balance
d0_ind<-list()
d1_ind<-list()
for (i in seq(1,length(all)))
{
  d0_ind[[i]]<-as.numeric(which(lapply(d0,length)==all[i]))
  d1_ind[[i]]<-as.numeric(which(lapply(d1,length)==all[i]))
}

couples<-data.frame("d0"=d0_ind[[1]],"d1"=d1_ind[[length(all)]])
for(i in seq(2,length(all)))
{
  cou<-data.frame("d0"=d0_ind[[i]],"d1"=d1_ind[[length(all)-i+1]])
  couples<-rbind(couples,cou)
}

#let'a create 48 datasets for 2 classes
for (i in seq(1, ntimes))
  {
  data_list_2[[i+1]]<-rbind(data_list_2[[i]], data_2[c(d0[[couples$d0[i]]],d1[[couples$d1[i]]]),])
}

###### generate with 3 classes
set.seed(394)
#n<-200
mix_start_3<-MixSim(BarOmega = 0.1,K = 3, p = 20,resN=1000)

data_3<-create_dataset(mix_start_3,1000)[[1]]
tab3<-as.data.frame(table(data_3$Label))

f0<-(tab3$Freq[1]/1000)*100
f1<-(tab3$Freq[2]/1000)*100
#first small dataset
c0<-round((min(samples)/100)*f0)
c1<-round((min(samples)/100)*f1)
c2<-min(samples)-(c0+c1)

d0<-seq(1,c0) #first group with label 0
d1<-seq(tab3$Freq[1]+1,tab3$Freq[1]+c1) #first group with label 1
d2<-seq(tab3$Freq[1]+tab3$Freq[2]+1,tab3$Freq[1]+tab3$Freq[2]+c2) #first group with label 1

data_list_3<-list()
data_list_3[[1]]<-data_3[c(d0,d1,d2),]

d0<-split(seq(c0+1,tab3$Freq[1]), sort(seq(c0+1,tab3$Freq[1])%%ntimes)) #create groups with label 0, 
d1<-split(seq(tab3$Freq[1]+c1+1,tab3$Freq[1]+tab3$Freq[2]), sort(seq(tab3$Freq[1]+c1+1,tab3$Freq[1]+tab3$Freq[2])%%ntimes)) #create groups with label 1
d2<-split(seq(tab3$Freq[1]+tab3$Freq[2]+c2+1,max(samples)), sort(seq(tab3$Freq[1]+tab3$Freq[2]+c2+1,max(samples))%%ntimes)) #create groups with label 2

ceil<-max(as.numeric(lapply(d0,length)),as.numeric(lapply(d1,length)),as.numeric(lapply(d2,length)))
floor<-min(as.numeric(lapply(d0,length)),as.numeric(lapply(d1,length)),as.numeric(lapply(d2,length)))
all<-seq(floor,ceil)

#create triples to have always the same balance
d0_ind<-list()
d1_ind<-list()
d2_ind<-list()

for (i in seq(1,length(all)))
{
  d0_ind[[i]]<-as.numeric(which(lapply(d0,length)==all[i]))
  d1_ind[[i]]<-as.numeric(which(lapply(d1,length)==all[i]))
  d2_ind[[i]]<-as.numeric(which(lapply(d2,length)==all[i]))
}
###maybe its data specific but ok for now, needs manual checking
# i need 6+7+7 or 7+7+6 or 6+6+8
d0_l<-length(d0_ind[[1]])
d1_l<-length(d1_ind[[1]])#d1_l<-length(d1_ind[[length(all)]])
d2_l<-length(d2_ind[[length(all)]])

#triples<-data.frame("d0"=d0_ind[[1]][seq(1,d1_l)],"d1"=d1_ind[[length(all)]],"d2"=d2_ind[[1]][seq(1,d1_l)])
#j<-1
#k<-d0_l-d1_l
#d0_ind[[1]]<-d0_ind[[1]][-seq(1,d1_l)]
#d2_ind[[1]]<-d2_ind[[1]][-seq(1,d1_l)]
#for(i in seq(1,length(all)-1))
# {
#   tri<-data.frame("d0"=d0_ind[[i]],"d1"=d1_ind[[length(all)-1]][seq(j,k)],"d2"=d2_ind[[length(all)-i]])
#   j<-k+1
#   k<-length(d1_ind[[2]])
#   
#   triples<-rbind(triples,tri)
# }

triples<-data.frame("d0"=d0_ind[[1]],"d1"=d1_ind[[length(all)]][seq(1,d0_l)],"d2"=d2_ind[[length(all)]][seq(1,d0_l)])
tri<-data.frame("d0"=d0_ind[[2]][seq(1,d1_l)],"d1"=d1_ind[[1]],"d2"=d2_ind[[2]][seq(1,d1_l)])
triples<-rbind(triples,tri)
tri<-data.frame("d0"=d0_ind[[2]][seq(d1_l+1,length(d0_ind[[2]]))],"d1"=d1_ind[[2]][seq(d0_l+1,length(d1_ind[[2]]))],"d2"=d2_ind[[1]])
triples<-rbind(triples,tri)


#let'a create 48 datasets for 2 classes
for (i in seq(1, ntimes))
{
  data_list_3[[i+1]]<-rbind(data_list_3[[i]], data_3[c(d0[[triples$d0[i]]],d1[[triples$d1[i]]],d2[[triples$d2[[i]]]]),])
}

#### chcek correlation
ggcorr(data_list_2[[1]][,-21],label=TRUE)
mean(abs(cor(data_list_2[[1]][,-21])))
ggcorr(data_list_2[[20]][,-21],label=TRUE)
mean_corr_2<-as.numeric(lapply(data_list_2,function(x) mean(abs(cor(x[,-21])))))
mean_corr_3<-as.numeric(lapply(data_list_3,function(x) mean(abs(cor(x[,-21])))))

### dataset entropy: entropy = -(class0 * log2(class0) + class1 * log2(class1))
entropy_ds<-function(df)
  {
  prob<-as.numeric(table(df$Label)/nrow(df))
  ent<--sum(prob*log2(prob))
  return(ent)
}

dataset_ent<-as.numeric(lapply(data_list_2,entropy_ds))
dataset_ent_3<-as.numeric(lapply(data_list_3,entropy_ds))

#compute interaction information
int_inf_2<-numeric()
int_inf_3<-numeric()
for (i in c(1:length(samples)))
{
  print(i)
  disc_data2<-discretize(data_list_2[[i]][,-21])
  int_inf_2<-c(int_inf_2,interinformation(disc_data2, method="emp"))
  disc_data3<-discretize(data_list_3[[i]][,-21])
  int_inf_3<-c(int_inf_3,interinformation(disc_data3, method="emp"))
}

#compute mutual dependence
mdm_2<-numeric()
mdm_3<-numeric()
for (i in c(1:length(samples)))
{
  print(i)
  mdm_2<-c(mdm_2,mdm(data_list_2[[i]][,-21])[[1]])
  mdm_3<-c(mdm_3,mdm(data_list_3[[i]][,-21])[[1]])
}

# compute score based on correlation
source("~/Desktop/Instability_ML/scripts/Score_correlation.R")
source("~/Desktop/Instability_ML/scripts/flattenCorMatrix.R")

sc_2<-as.numeric(lapply(data_list_2, function(x) Score_correlation(x,21,0.4)[[1]]))
sc_2_matrix<-lapply(data_list_2, function(x) Score_correlation(x,21,0.4)[[2]])

sc_3<-as.numeric(lapply(data_list_3, function(x) Score_correlation(x,21,0.4)[[1]]))

#### now lets create a dataset with 100 samples but increasing number of features
#im not able with this one to generate more features than samples
set.seed(394)
data_list_feat<-list()

features<-c(5,10,20,30,40,50)
for (i in seq(1,length(features)))
{
  print(i)
  mix_start<-MixSim(BarOmega = 0.1,K = 2, p = features[i],resN=1000)
  data_list_feat[[i]]<-create_dataset(mix_start,100)[[1]]
}


### save files to folder
#for (i in seq(1,length(samples)))
#  write.csv(data_list_2[[i]],paste('/Users/ieo4991/Desktop/Instability_ML/datasets/training/simulated_',samples[i],'.csv',sep=""))
