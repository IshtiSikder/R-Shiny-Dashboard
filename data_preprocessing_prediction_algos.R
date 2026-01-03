corpus<-read_excel("2023-05-HM-Corpus.xlsx",sheet="Corpus",skip=2)
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

corpus<-corpus%>%mutate(Inc.Pan=str_match(corpus$Sampling.Method,"Pan")%>%is.na(.),Sampling.Time.days=Sampling.Time.min/(60*24))

corpus$Line<-rownames(corpus)

LI.corpus<-corpus%>%subset(select=c(58,4,5,6,11,54,32,55,52,53))

#inclusive, binary measures of collection method
LI.corpus<-LI.corpus%>%mutate(Net=as.numeric(str_detect(corpus$Sampling.Method,"Net")),Visual=as.numeric(str_detect(corpus$Sampling.Method,"Visual")),Pan=as.numeric(str_detect(corpus$Sampling.Method,"Pan")))

#binary measures of level of identification
LI.corpus<-LI.corpus%>%mutate(Species=as.numeric(str_detect(Least.Specific.ID,"Species")),Genus=as.numeric(str_detect(Least.Specific.ID,"Genus")),Morphospecies=as.numeric(str_detect(Least.Specific.ID,"Morphospecies")),Family=as.numeric(str_detect(Least.Specific.ID,"Family")))

LI.corpus<-LI.corpus%>%subset(select=-c(6,7))

#Step 1: Log Abundance model coefficients; use these to calculate abundance from user-input data.  Default to median values I suppose.
lm.Abd<-lm(Abundance~Start.Year+Net+Visual+Pan+Species+Genus+Morphospecies+Family+No..of.Years+No..of.Sites..Avg.+Sample.Trips.Year..Avg.+Sampling.Time.min,data=LI.corpus)
par(mfrow=c(2,2))
plot(lm.Abd, which=1:4)
par(mfrow=c(1,1))
#lot of leverage here: we should discuss whether we ought to remove study 13,68,70 from the list

lm.Abd%>%summary()
lm.Abd%>%anova()
#If we're assuming that we're including those non-significant model features, we're going to need some indicators that these don't necessarily matter?
lm.Abd%>%coef()

#Step 2: Exponential model: use this to predict the Richness from the Abundance calculated in Step 1.

#exponential component search
#https://www.desmos.com/calculator/rjtl4lqhkm
#equation we want to fit is y=b*(1-e^(-kx))
#b or the maximum should be around 75 
#k is the rate of increase, visual estimation suggests it should be around 0.0002 
#with these starting parameters, we ought to be able to get R to estimate a model
exp.Rch<-nls(Richness~mx*(1-(exp(-rc*Abundance))),data=LI.corpus,start=list(mx=75,rc=0.0002))

#coeffs
coef(exp.Rch)

#residuals
resid.exp.Rch<-nlsResiduals(exp.Rch)
par(mfrow=c(2,2))
plot(resid.exp.Rch)
test.nlsResiduals(resid.exp.Rch)
#residuals look okay, but there’s some deviance on the QQ and the resid’s aren’t normally distributed.

#inputing residuals back into main database: 
LI.corpus<-filter(LI.corpus,Richness!=0)%>%
  subset(select=Line)%>%
  cbind(.,resid(exp.Rch))%>%
  as_tibble%>%full_join(LI.corpus,.)%>%
  rename(.,r.Rch='resid(exp.Rch)')


#Step 3: Adjusting richness based on residuals: Use this to adjust richness up and down off the exponential line based on the coefficients. 

resid.Rch<-lm(r.Rch~Net+Visual+Pan+Species+Genus+Morphospecies+No..of.Years+No..of.Sites..Avg.+Sample.Trips.Year..Avg.+Sampling.Time.min,data=LI.corpus)

#residuals
par(mfrow=c(2,2))
plot(resid.Rch, which=1:4)
par(mfrow=c(1,1))
#influential points are the usual offenders


resid.Rch%>%summary()
resid.Rch%>%anova()
#If we're assuming that we're including those non-significant model features, we're going to need some indicators that these don't necessarily matter?
resid.Rch%>%coef()


#Retrieving co-efficients for the regression model predicting Abundance
model_summary <- summary(lm.Abd)

# Extract coefficients and p-values
Abundance_coeffs <- as.data.frame(model_summary$coefficients)

# Select desired columns and rename
Abundance_coeffs <- Abundance_coeffs[, c("Estimate", "Pr(>|t|)")]

#Filtering out significant features 
# Set Estimate to 0 where Pr(>|t|) >= 0.05
#Abundance_coeffs$Estimate[Abundance_coeffs$`Pr(>|t|)` >= 0.05] <- 0





