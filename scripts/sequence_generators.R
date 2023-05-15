library(reshape2)
library(tidyverse)
library(ggpubr)

runif(1); .Random.seed[1:6]; 
runif(1); .Random.seed[1:6]

set.seed(1,kind ="Mersenne-Twister" )
u1<-runif(100)
set.seed(1,kind ="Wichmann-Hill" )
u2<-runif(100)
set.seed(1, kind="Super-Duper")
u3 <- runif(100)
set.seed(1, kind="Marsaglia-Multicarry")
u4 <- runif(1000)
set.seed(1, kind="Knuth-TAOCP-2002")
u5 <- runif(100)
set.seed(1, kind="Knuth-TAOCP")
u6 <- runif(100)
set.seed(1, kind="L'Ecuyer-CMRG")
u7 <- runif(100)


prova_rng<-data.frame("Case"=seq(1,100),"MT"=u1,"WH"=u2,"SD"=u3,"MM"=u4,"KT2002"=u5,"KT"=u6,"LCMRG"=u7)

prova_rng2<-melt(prova_rng,id.vars ="Case")

prova_rng2 %>% ggboxplot(x="variable",y="value")


#######
base<-seq(1,10)
seed_matrix<-data.frame("Seed_1"=base*10,"Seed_2"=base*10^2,"Seed_3"=base*10^3,
                        "Seed_4"=base*10^4,"Seed_5"=base*10^5)
cols<-colnames(seed_matrix)
for (el in cols)
{
  u<-numeric()
  for (i in 1:nrow(seed_matrix))
  {
    set.seed(seed_matrix[[el]][i])
    u<-c(u,runif(1))
    #print(seed_matrix[[el]][i])
  }
  seed_matrix<-cbind(seed_matrix,u)
}
colnames(seed_matrix)<-c(cols,paste(cols,"_gen",sep=""))

prova_seed<-melt(seed_matrix[,c(6,7,8,9,10)])

prova_seed %>% ggboxplot(x="variable",y="value")
