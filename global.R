# Install required packages if not already installed
if (!require("shiny")) install.packages("shiny")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggExtra")) install.packages("ggExtra")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("bslib")) install.packages("bslib")
if (!require("fontawesome")) install.packages("fontawesome")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("plotly")) install.packages("plotly")
if (!require("DT")) install.packages("DT")
if (!require("readxl")) install.packages("readxl")
if (!require("nlstools")) install.packages("nlstools")
if (!require("rsconnect")) install.packages("rsconnect")


# Load all required packages
library(shiny)
library(ggplot2)
library(ggExtra)
library(shinythemes)
library(bslib)
library(fontawesome)
library(tidyverse)
library(plotly)
library(DT)
library(readxl)
library(nlstools)
library(rsconnect)


# Load dataset
#tryCatch({
#  data <- read_excel("/cloud/project/R-Shiny-Bees/data.csv")
#}, error = function(e) {
#  message("Error reading CSV file: ", e$message)
#})


# Source all function files
source("worldMap.R")
source("usaStateMap.R")
source("siteCounts.R")
source("collectionMethods.R")
source("identificationMethods.R")
source("durationTrend.R")
source("samplingTrips.R")
source("ui_components.R")
source("model_coefficients.R")
source("data_preprocessing_prediction_algos.R")
source("data_preprocessing_prediction_plot.R")
source("prediction_algos.R")
source("prediction_plot.R")


#rsconnect::deployApp('cloud/project/R-Shiny-Bees')

