## Code for Table 5 "Richness"

# Importing the libraries -------------------------------------------------


library(readxl)
library(tidyverse)
library(ggExtra)
library(broom)
library(ggpubr)
library(jtools)
library(MASS)	
library(nlstools)


# Reading the data --------------------------------------------------------

corpus <- read_excel("data/2023-05-HM-Corpus.xlsx", sheet='Corpus', skip=2)

# Data Preprocessing ------------------------------------------------------

names(corpus)<-names(corpus)%>%make.names()
corpus<-corpus%>%subset(select=-c(2:10,39:58))

#make level of identification (Least specific N) ordered
corpus$Least.Specific.N<-corpus$Least.Specific.N%>%ordered()

#force “not reported” into NA
corpus$Visual.Sample.Time..min.<-as.numeric(corpus$Visual.Sample.Time..min.)
corpus$Visual.Sp.Richness<-as.numeric(corpus$Visual.Sp.Richness)

#creating unified abundance & richness measures
corpus<-corpus%>%mutate(Abundance=ifelse(is.na(Combined.Abundance)==TRUE,ifelse(is.na(Visual.Abundance)==TRUE,ifelse(is.na(Pan.Traps.Abundance)==TRUE,Net.Abundance,Pan.Traps.Abundance),Visual.Abundance),Combined.Abundance))

corpus<-corpus%>%mutate(Richness=ifelse(is.na(Combined.Sp.Richness)==TRUE,ifelse(is.na(Visual.Sp.Richness)==TRUE,ifelse(is.na(Pan.Traps.Sp.Richness)==TRUE,Net.Sp.Richness,Pan.Traps.Sp.Richness),Visual.Sp.Richness),Combined.Sp.Richness))

corpus$Least.Specific.ID<-corpus$Least.Specific.ID%>%ordered(levels=c("Species","Genus","Morphospecies","Family"))

#cleaning up sampling method names
corpus<-corpus%>%mutate(Sampling.Method=paste(ifelse(corpus$Net.Sampling==1,"Net",""),ifelse(corpus$Pan.Sampling==1,"Pan",""),ifelse(corpus$Visual.Sampling==1,"Visual",""),sep=",")%>%str_replace(",,",","))

corpus<-corpus%>%mutate(Sampling.Method=ifelse(substr(corpus$Sampling.Method,1,1)==",",sub(".","",corpus$Sampling.Method),corpus$Sampling.Method))

corpus<-corpus%>%mutate(Sampling.Method= ifelse(substr(corpus$Sampling.Method,nchar(corpus$Sampling.Method),nchar(corpus$Sampling.Method)+1)==",",substr(corpus$Sampling.Method,1,nchar(corpus$Sampling.Method)-1),corpus$Sampling.Method))

#create unified sampling time
corpus$Pan.Traps.Sample.Time..days..Avg.<-as.numeric(corpus$Pan.Traps.Sample.Time..days..Avg.)

corpus[c(15,16,19,20)][is.na(corpus[c(15,16,19,20)])]<-0

corpus$Sampling.Time.min<-corpus$Net.Sample.Time..min.+(corpus$Pan.Traps.Sample.Time..days..Avg.*24*60)+corpus$Visual.Sample.Time..min.+corpus$Combined.Sample.Time

corpus$Sampling.Time.min<-replace(corpus$Sampling.Time.min,corpus$Sampling.Time.min==0,NA)

#Sampling method and sampling time
#split descriptives for sampling time
corpus<-corpus%>%mutate(Inc.Pan=str_match(corpus$Sampling.Method,"Pan")%>%is.na(.),Sampling.Time.days=Sampling.Time.min/(60*24))


# Richness model ----------------------------------------------------------

#now to look at curvilinear regression elements to observe whether our "asymptote" hypothesis is accurate
corpus$Line<-rownames(corpus)
sig.corpus<-corpus%>%subset(select=c("Line","Richness","Abundance"))%>%na.omit()


#exponential component search
#https://www.desmos.com/calculator/rjtl4lqhkm
#equation we want to fit is y=b*(1-e-kx)
#b or the maximum should be around 75 
#k is the rate of increase, visual estimation suggests it should be around 0.0002 
#with these starting parameters, we ought to be able to get R to estimate a model
RAmodel<-nls(Richness~mx*(1-(exp(-rc*Abundance))),data=sig.corpus,start=list(mx=75,rc=0.0002))
#residuals
resid.RAmodel<-nlsResiduals(RAmodel)


#a lot of influence at the higher end of the abundance range
outliers<-c(13,27,31,46,64,67)
sig.corpus<-sig.corpus%>%mutate(size=2,shape=17)
sig.corpus$shape[outliers]<-8
sig.corpus$size[outliers]<-4

#bootstrap the confints
boot.RAmodel<-nlsBoot(RAmodel,niter=1000)
conf.RAmodel<-nlsBootPredict(boot.RAmodel,interval="confidence")
sig.corpus<-conf.RAmodel%>%as.data.frame()%>%cbind(sig.corpus,.)
names(sig.corpus)<-c("Line","Richness","Abundance","size","shape", "Median","Conf2.5","Conf97.5")


reg.corpus<-corpus%>%subset(select=c(58,4,5,6,11,54,32,55,52,53))

#inclusive, binary measures of collection method
reg.corpus<-reg.corpus%>%mutate(Net=as.numeric(str_detect(corpus$Sampling.Method,"Net")),Visual=as.numeric(str_detect(corpus$Sampling.Method,"Visual")),Pan=as.numeric(str_detect(corpus$Sampling.Method,"Pan")))

#binary measures of level of identification
reg.corpus<-reg.corpus%>%mutate(Species=as.numeric(str_detect(Least.Specific.ID,"Species")),Genus=as.numeric(str_detect(Least.Specific.ID,"Genus")),Morphospecies=as.numeric(str_detect(Least.Specific.ID,"Morphospecies")),Family=as.numeric(str_detect(Least.Specific.ID,"Family")))

reg.corpus<-reg.corpus%>%subset(select=-c(6,7))

#log-transforming data
reg.corpus<-reg.corpus[c(3:6)]%>%apply(.,MARGIN=2,log)%>%as_tibble()%>%cbind(reg.corpus[-c(3:6)],.)%>%as_tibble()

#Richness model
#joining residuals from the RAmodel to the regression database
sig.corpus<-cbind(sig.corpus,resid.RAmodel$resi1)
sig.corpus%>%subset(select=c("Line","Residuals"))
reg.corpus<-sig.corpus%>%subset(select=c("Line","Residuals"))%>%full_join(reg.corpus,.)

rch.residm1<-lm(Residuals~Start.Year+No..of.Years+No..of.Sites..Avg.+Sample.Trips.Year..Avg.+Net+Pan+Visual+Sampling.Time.min+Species+Genus+Morphospecies+Start.Year*No..of.Years+ +Net*Sampling.Time.min+Pan*Sampling.Time.min+Visual*Sampling.Time.min,data=reg.corpus)

step.rch.residm1<-stepAIC(rch.residm1,direction="both",trace=0)

# Print model summary and coefficients
summary(step.rch.residm1)
coef(step.rch.residm1)
