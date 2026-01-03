# Read and process data
corpus <- read_excel("2023-05-HM-Corpus.xlsx", sheet="Corpus", skip=2)
names(corpus) <- names(corpus) %>% make.names()
corpus <- corpus %>% subset(select=-c(2:10,39:58))
# 
# Force "not reported" into NA
corpus$Visual.Sample.Time..min. <- as.numeric(corpus$Visual.Sample.Time..min.)
corpus$Visual.Sp.Richness <- as.numeric(corpus$Visual.Sp.Richness)
# 
# Creating unified abundance & richness measures
corpus <- corpus %>% mutate(Abundance=ifelse(is.na(Combined.Abundance)==TRUE,
                                             ifelse(is.na(Visual.Abundance)==TRUE,
                                                    ifelse(is.na(Pan.Traps.Abundance)==TRUE,
                                                           Net.Abundance,Pan.Traps.Abundance),
                                                    Visual.Abundance),Combined.Abundance))
corpus <- corpus %>% mutate(Richness=ifelse(is.na(Combined.Sp.Richness)==TRUE,
                                            ifelse(is.na(Visual.Sp.Richness)==TRUE,
                                                   ifelse(is.na(Pan.Traps.Sp.Richness)==TRUE,
                                                          Net.Sp.Richness,Pan.Traps.Sp.Richness),
                                                   Visual.Sp.Richness),Combined.Sp.Richness))

# Create Line variable and subset
corpus$Line <- rownames(corpus)
sig.corpus <- corpus %>% subset(select=c("Line","Richness","Abundance")) %>% na.omit()

# Fit NLS model
RAmodel <- nls(Richness~mx*(1-(exp(-rc*Abundance))),
               data=sig.corpus,
               start=list(mx=75,rc=0.0002))

# Process outliers
outliers <- c(13,27,31,46,64,67)
sig.corpus <- sig.corpus %>% mutate(size=2,shape=17)
sig.corpus$shape[outliers] <- 8
sig.corpus$size[outliers] <- 4
# 
# Bootstrap confidence intervals
boot.RAmodel <- nlsBoot(RAmodel,niter=1000)
conf.RAmodel <- nlsBootPredict(boot.RAmodel,interval="confidence")
sig.corpus <- conf.RAmodel %>% as.data.frame() %>% cbind(sig.corpus,.)
names(sig.corpus) <- c("Line","Richness","Abundance","size","shape", "Median","Conf2.5","Conf97.5")
# 
# Fit log-log model
RAmodel2 <- lm(log(Richness)~log(Abundance),data=sig.corpus)
sig.corpus <- predict(RAmodel2,interval = "confidence") %>% 
  as.data.frame() %>% 
  cbind(sig.corpus,.)
sig.corpus <- rename(sig.corpus,"RA2.Median"="fit","RA2.Conf2.5"="lwr","RA2.Conf97.5"="upr")

# Create lds data frame
lds <- data.frame(mx=c(143.0,143.0*.5,143.0*.95)) %>% 
  mutate(.,Abundance=(log(1-mx/143.0)/-(1.786*10^-4))) %>% 
  slice(-1)
lds$lab <- c("50%=72","95%=136")