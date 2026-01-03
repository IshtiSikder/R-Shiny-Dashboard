## Code for Table 4 "Abundance"
# Importing the libraries -------------------------------------------------

library(readxl)
library(tidyverse)
library(ggExtra)
library(broom)
library(ggpubr)
library(jtools)
library(MASS)	


# Reading the Data --------------------------------------------------------

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

corpus$Line<-rownames(corpus)


# Abundance model ---------------------------------------------------------

reg.corpus<-corpus%>%subset(select=c(58,4,5,6,11,54,32,55,52,53))

#inclusive, binary measures of collection method
reg.corpus<-reg.corpus%>%mutate(Net=as.numeric(str_detect(corpus$Sampling.Method,"Net")),Visual=as.numeric(str_detect(corpus$Sampling.Method,"Visual")),Pan=as.numeric(str_detect(corpus$Sampling.Method,"Pan")))

#binary measures of level of identification
reg.corpus<-reg.corpus%>%mutate(Species=as.numeric(str_detect(Least.Specific.ID,"Species")),Genus=as.numeric(str_detect(Least.Specific.ID,"Genus")),Morphospecies=as.numeric(str_detect(Least.Specific.ID,"Morphospecies")),Family=as.numeric(str_detect(Least.Specific.ID,"Family")))

reg.corpus<-reg.corpus%>%subset(select=-c(6,7))

#log-transforming data
reg.corpus<-reg.corpus[c(3:6)]%>%apply(.,MARGIN=2,log)%>%as_tibble()%>%cbind(reg.corpus[-c(3:6)],.)%>%as_tibble()

#Abundance model
abd.llm1<- lm(log(Abundance)~Start.Year*No..of.Years+No..of.Sites..Avg.+Sample.Trips.Year..Avg.+Net *Sampling.Time.min+Pan *Sampling.Time.min+Visual*Sampling.Time.min,data=reg.corpus)

#variable selection
step.abd.llm1<-stepAIC(abd.llm1,direction="both",trace=0)

# Print model summary and coefficients
summary(step.abd.llm1)
coef(step.abd.llm1)


