# Load required libraries
library(readxl)
library(tidyverse)
library(ggExtra)
library(broom)
library(ggpubr)
library(nlstools)

# Read and prepare data
corpus <- read_excel("data/2023-05-HM-Corpus.xlsx", 
                     sheet='Corpus', skip=2)
names(corpus) <- names(corpus) %>% make.names()
corpus <- corpus %>% subset(select=-c(2:10,39:58))

# Make level of identification ordered
corpus$Least.Specific.N <- corpus$Least.Specific.N %>% ordered()

# Force "not reported" into NA and convert to numeric
corpus$Visual.Sample.Time..min. <- as.numeric(corpus$Visual.Sample.Time..min.)
corpus$Visual.Sp.Richness <- as.numeric(corpus$Visual.Sp.Richness)

# Create unified abundance & richness measures
corpus <- corpus %>% 
  mutate(Abundance = ifelse(is.na(Combined.Abundance) == TRUE,
                            ifelse(is.na(Visual.Abundance) == TRUE,
                                   ifelse(is.na(Pan.Traps.Abundance) == TRUE,
                                          Net.Abundance,
                                          Pan.Traps.Abundance),
                                   Visual.Abundance),
                            Combined.Abundance))

corpus <- corpus %>% 
  mutate(Richness = ifelse(is.na(Combined.Sp.Richness) == TRUE,
                           ifelse(is.na(Visual.Sp.Richness) == TRUE,
                                  ifelse(is.na(Pan.Traps.Sp.Richness) == TRUE,
                                         Net.Sp.Richness,
                                         Pan.Traps.Sp.Richness),
                                  Visual.Sp.Richness),
                           Combined.Sp.Richness))

# Create identifier for each row
corpus$Line <- rownames(corpus)

# Create dataset for abundance-richness analysis
sig.corpus <- corpus %>% 
  subset(select=c("Line","Richness","Abundance")) %>% 
  na.omit()

# Fit nonlinear model
# Equation: y = b*(1-e^(-kx))
# b (mx) = maximum richness (around 75)
# k (rc) = rate of increase (around 0.0002)
RAmodel <- nls(Richness ~ mx*(1-(exp(-rc*Abundance))),
               data=sig.corpus,
               start=list(mx=75, rc=0.0002))

# Model diagnostics
resid.RAmodel <- nlsResiduals(RAmodel)

# Identify influential points
jack.RAmdel <- nlsJack(RAmodel)

# Mark outliers
outliers <- c(13,27,31,46,64,67)
sig.corpus <- sig.corpus %>% 
  mutate(size=2, shape=17)
sig.corpus$shape[outliers] <- 8
sig.corpus$size[outliers] <- 4

# Bootstrap confidence intervals
boot.RAmodel <- nlsBoot(RAmodel, niter=1000)
conf.RAmodel <- nlsBootPredict(boot.RAmodel, interval="confidence")
sig.corpus <- conf.RAmodel %>% 
  as.data.frame() %>% 
  cbind(sig.corpus, .)
names(sig.corpus) <- c("Line","Richness","Abundance","size","shape", 
                       "Median","Conf2.5","Conf97.5")

# Fit log-log model for comparison
RAmodel2 <- lm(log(Richness) ~ log(Abundance), data=sig.corpus)

# Add log-log model predictions
sig.corpus <- predict(RAmodel2, interval="confidence") %>% 
  as.data.frame() %>% 
  cbind(sig.corpus, .)
sig.corpus <- rename(sig.corpus,
                     "RA2.Median"="fit",
                     "RA2.Conf2.5"="lwr",
                     "RA2.Conf97.5"="upr")

# Calculate standardized residuals
sig.corpus$RA1.std <- resid.RAmodel$resi2[,2]
sig.corpus$RA2.std <- rstandard(RAmodel2)

# Calculate RSE (Root Square Error)
sqrt(sum((sig.corpus$Median-sig.corpus$Richness)^2)/86)  # For nonlinear model
sqrt(sum((exp(sig.corpus$RA2.Median)-sig.corpus$Richness)^2)/86)  # For log-log model

# Create reference lines data (50% and 95% of maximum)
lds <- data.frame(mx=c(143.0,143.0*.5,143.0*.95)) %>% 
  mutate(., Abundance=(log(1-mx/143.0)/-(1.786*10^-4))) %>% 
  slice(-1)
lds$lab <- c("50%=72","95%=136")

# Create the main plot
p3a <- ggplot(data=sig.corpus, aes(x=Abundance)) +
  # Add confidence bands
  geom_ribbon(aes(ymin=Conf2.5, ymax=Conf97.5), 
              fill="#005AB5", alpha=.2) +
  geom_ribbon(aes(ymin=exp(RA2.Conf2.5), ymax=exp(RA2.Conf97.5)), 
              fill="#DC3220", alpha=.2) +
  # Add reference lines
  geom_segment(data=lds, 
               aes(x=Abundance, xend=Abundance, y=1, yend=mx),
               color="olivedrab", linetype="dotdash", linewidth=1) +
  geom_segment(data=lds, 
               aes(x=10, xend=Abundance, y=mx, yend=mx),
               color="olivedrab", linetype="dotdash", linewidth=1) +
  # Add model fits
  geom_line(aes(y=Median), color="#005AB5", linewidth=1) +
  geom_line(aes(y=exp(RA2.Median)), 
            color="#DC3220", linewidth=1, linetype="dashed") +
  # Add data points
  geom_point(aes(y=Richness, size=size, shape=shape), 
             color="firebrick3") +
  # Add labels
  geom_text(data=lds, 
            aes(x=10, y=mx+1, label=lab),
            vjust=0, hjust=0, size=5) +
  geom_text(data=lds, 
            aes(x=Abundance, y=1, label=round(Abundance,0)),
            vjust=1, hjust=0, size=5) +
  # Scale and theme settings
  scale_x_continuous(breaks=c(10,100,1000,10000,30000), trans="log10") +
  scale_y_continuous("Richness", trans="log10") +
  scale_size_identity() +
  scale_shape_identity() +
  theme_minimal() +
  theme(axis.title=element_text(face="bold",size=20),
        axis.text=element_text(size=12))

# Add marginal plots
p3aa <- ggMarginal(p3a, type="histogram", 
                   fill="olivedrab", color="grey60")

# Save the plot
ggsave("Abundance_vs_Richness.png", plot=p3aa, 
       height=10, width=7.5, units="in")

# First, we get the coefficients from our nonlinear model (RAmodel)
nls_coef <- coef(RAmodel)  
mx <- nls_coef["mx"]       # mx is the maximum richness parameter
rc <- nls_coef["rc"]       # rc is the rate parameter

# The prediction function
predict_richness <- function(abundance, model="nonlinear") {
  # The function takes two arguments:
  # 1. abundance: number of bees
  # 2. model: which model to use ("nonlinear" or "loglog")
  
  if(model == "nonlinear") {
    # NONLINEAR (ASYMPTOTIC) MODEL
    # Formula: Richness = mx * (1 - exp(-rc * abundance))
    # This models how species richness approaches a maximum (mx) 
    # as abundance increases
    richness <- mx * (1 - exp(-rc * abundance))
    
    # Calculate uncertainty
    rse <- sqrt(sum(resid(RAmodel)^2) / df.residual(RAmodel))
    margin <- qt(0.975, df.residual(RAmodel)) * rse
    
    return(list(
      prediction = richness,          # Predicted number of species
      lower_ci = richness - margin,   # Lower confidence bound
      upper_ci = richness + margin    # Upper confidence bound
    ))
    
  } else if(model == "loglog") {
    # LOG-LOG MODEL
    # Formula: log(Richness) = β₀ + β₁ * log(Abundance)
    # This models a power-law relationship between 
    # abundance and richness
    log_pred <- predict(RAmodel2, 
                        newdata = data.frame(Abundance = abundance),
                        interval = "confidence")
    
    return(list(
      prediction = exp(log_pred[,"fit"]),    # Transform back from log scale
      lower_ci = exp(log_pred[,"lwr"]),      # Lower confidence bound
      upper_ci = exp(log_pred[,"upr"])       # Upper confidence bound
    ))
  }
}

# Example usage with explanations
example_abundance <- 1000  # Let's predict for 1000 bees

# Using nonlinear model
nonlinear_result <- predict_richness(example_abundance, "nonlinear")
# This uses the formula: mx * (1 - exp(-rc * 1000))

# Using log-log model
loglog_result <- predict_richness(example_abundance, "loglog")
# This uses the formula: exp(β₀ + β₁ * log(1000))

# Print results with explanation
cat("For a sample of", example_abundance, "bees:\n")
cat("\nNonlinear Model Prediction:")
cat("\n- Predicted number of species:", round(nonlinear_result$prediction, 2))
cat("\n- 95% Confidence Interval:", 
    round(nonlinear_result$lower_ci, 2), "to", 
    round(nonlinear_result$upper_ci, 2))

cat("\n\nLog-log Model Prediction:")
cat("\n- Predicted number of species:", round(loglog_result$prediction, 2))
cat("\n- 95% Confidence Interval:", 
    round(loglog_result$lower_ci, 2), "to", 
    round(loglog_result$upper_ci, 2))

# Let's also see how predictions change with abundance
abundance_sequence <- c(100, 500, 1000, 5000, 10000)
results <- data.frame(
  Abundance = abundance_sequence,
  Nonlinear_Prediction = sapply(abundance_sequence, 
                                function(x) predict_richness(x, "nonlinear")$prediction),
  Loglog_Prediction = sapply(abundance_sequence, 
                             function(x) predict_richness(x, "loglog")$prediction)
)
print("\n\nPredictions for different abundances:")
print(round(results, 2))