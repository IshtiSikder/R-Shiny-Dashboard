library(shiny)
library(bslib)
library(ggplot2)
library(tidyverse)
library(shinythemes)
library(MASS)
library(nlstools)
library(readxl)

# Actual model coefficients [unchanged]
abundance_model_coefficients <- c(
  `(Intercept)` = 4.9672061,
  `No..of.Years` = 0.8957990,
  `No..of.Sites..Avg.` = 0.3640503,
  `Sampling.Time.min` = 0.1696292,
  `Visual` = -0.6669834,
  `Sampling.Time.min:Visual` = 0.3874213
)

richness_model_coefficients <- c(
  `(Intercept)` = 3778.348471,
  `Start.Year` = -1.902323,
  `No..of.Sites..Avg.` = 10.811558,
  `Sample.Trips.Year..Avg.` = 20.531091,
  `Visual` = -33.329740,
  `Species` = 26.267014
)

# Reading and preprocessing the data
corpus <- read_excel("2023-05-HM-Corpus.xlsx", sheet='Corpus', skip=2)

# Data Preprocessing
names(corpus)<-names(corpus)%>%make.names()
corpus<-corpus%>%subset(select=-c(2:10,39:58))

#make level of identification (Least specific N) ordered
corpus$Least.Specific.N<-corpus$Least.Specific.N%>%ordered()

#force "not reported" into NA
corpus$Visual.Sample.Time..min.<-as.numeric(corpus$Visual.Sample.Time..min.)
corpus$Visual.Sp.Richness<-as.numeric(corpus$Visual.Sp.Richness)

#creating unified abundance & richness measures
corpus<-corpus%>%mutate(Abundance=ifelse(is.na(Combined.Abundance)==TRUE,
                                         ifelse(is.na(Visual.Abundance)==TRUE,
                                                ifelse(is.na(Pan.Traps.Abundance)==TRUE,Net.Abundance,Pan.Traps.Abundance),
                                                Visual.Abundance),Combined.Abundance))

corpus<-corpus%>%mutate(Richness=ifelse(is.na(Combined.Sp.Richness)==TRUE,
                                        ifelse(is.na(Visual.Sp.Richness)==TRUE,
                                               ifelse(is.na(Pan.Traps.Sp.Richness)==TRUE,Net.Sp.Richness,Pan.Traps.Sp.Richness),
                                               Visual.Sp.Richness),Combined.Sp.Richness))

corpus$Least.Specific.ID<-corpus$Least.Specific.ID%>%ordered(levels=c("Species","Genus","Morphospecies","Family"))

#cleaning up sampling method names
corpus<-corpus%>%mutate(Sampling.Method=paste(
  ifelse(corpus$Net.Sampling==1,"Net",""),
  ifelse(corpus$Pan.Sampling==1,"Pan",""),
  ifelse(corpus$Visual.Sampling==1,"Visual",""),sep=",")%>%str_replace(",,",","))

corpus<-corpus%>%mutate(Sampling.Method=ifelse(substr(corpus$Sampling.Method,1,1)==",",
                                               sub(".","",corpus$Sampling.Method),corpus$Sampling.Method))

corpus<-corpus%>%mutate(Sampling.Method= ifelse(
  substr(corpus$Sampling.Method,nchar(corpus$Sampling.Method),
         nchar(corpus$Sampling.Method)+1)==",",
  substr(corpus$Sampling.Method,1,nchar(corpus$Sampling.Method)-1),
  corpus$Sampling.Method))

#create unified sampling time
corpus$Pan.Traps.Sample.Time..days..Avg.<-as.numeric(corpus$Pan.Traps.Sample.Time..days..Avg.)

corpus[c(15,16,19,20)][is.na(corpus[c(15,16,19,20)])]<-0

corpus$Sampling.Time.min<-corpus$Net.Sample.Time..min.+
  (corpus$Pan.Traps.Sample.Time..days..Avg.*24*60)+
  corpus$Visual.Sample.Time..min.+corpus$Combined.Sample.Time

corpus$Sampling.Time.min<-replace(corpus$Sampling.Time.min,corpus$Sampling.Time.min==0,NA)

# Create mappings for the app
corpus <- corpus %>%
  rename(
    identification_level = Least.Specific.ID,
    trips_per_year = Sample.Trips.Year..Avg.,
    study_duration = No..of.Years,
    num_sites = No..of.Sites..Avg.
  )

# UI Section ----------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  
  tags$head(
    tags$style(HTML("
      .sidebar { background-color: #E6EEF2; }
      .sidebar-title { font-size: 0.9rem !important; }
      .form-group { margin-bottom: 0.5rem !important; }
      .form-label { font-size: 0.75rem !important; }
    "))
  ),
  
  titlePanel("The Levenson Index 🐝"),
  
  # Top right home button
  tags$a(
    href = "#",
    icon("home"),
    style = "position:absolute; top:10px; right:10px;",
    onclick = "window.location.reload(); return false;"
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      style = "border-right: 1px solid #D1DCE1;",
      
      selectInput("select_var", "Select Variable:",
                  choices = c("Abundance", "Richness"),
                  selected = "Abundance"),
      
      # Abundance inputs
      conditionalPanel(
        condition = "input.select_var == 'Abundance'",
        sliderInput("study_duration", "Study Duration (years)",
                    min = min(corpus$study_duration, na.rm=TRUE),
                    max = max(corpus$study_duration, na.rm=TRUE),
                    value = median(corpus$study_duration, na.rm=TRUE)),
        numericInput("num_sites", "Number of Sites",
                     min = min(corpus$num_sites, na.rm=TRUE),
                     max = max(corpus$num_sites, na.rm=TRUE),
                     value = median(corpus$num_sites, na.rm=TRUE)),
        sliderInput("sampling_time", "Sampling Time (minutes)",
                    min = min(corpus$Sampling.Time.min, na.rm=TRUE),
                    max = max(corpus$Sampling.Time.min, na.rm=TRUE),
                    value = median(corpus$Sampling.Time.min, na.rm=TRUE)),
        radioButtons("sampling_method_abundance", "Collection Method",
                     choices = unique(corpus$Sampling.Method))
      ),
      
      # Richness inputs (simplified)
      conditionalPanel(
        condition = "input.select_var == 'Richness'",
        sliderInput("value_Trips_year", "Trips per Year",
                    min = min(corpus$trips_per_year, na.rm=TRUE),
                    max = max(corpus$trips_per_year, na.rm=TRUE),
                    value = median(corpus$trips_per_year, na.rm=TRUE)),
        radioButtons("id_level", "Level of Identification",
                     choices = c("Species", "Other")),
        radioButtons("sampling_method_richness", "Collection Method",
                     choices = unique(corpus$Sampling.Method))
      )
    ),
    
    mainPanel(
      width = 9,
      
      # Conditional panels for different plot sets
      conditionalPanel(
        condition = "input.select_var == 'Abundance'",
        tabsetPanel(
          id = "abundanceTabset",
          tabPanel("Study Duration", plotOutput("plot_duration")),
          tabPanel("Number of Sites", plotOutput("plot_sites")),
          tabPanel("Sampling Time", plotOutput("plot_sampling")),
          tabPanel("Collection Method", plotOutput("plot_method_abundance"))
        )
      ),
      
      conditionalPanel(
        condition = "input.select_var == 'Richness'",
        tabsetPanel(
          id = "richnessTabset",
          tabPanel("Trips per Year", plotOutput("plot_trips")),
          tabPanel("Level of Identification", plotOutput("plot_level")),
          tabPanel("Collection Method", plotOutput("plot_method_richness"))
        )
      )
    )
  )
)

# Server Section--------------------------------------------------------
server <- function(input, output, session) {
  
  # Abundance prediction function remains the same
  predict_abundance <- reactive({
    is_visual <- as.numeric(input$sampling_method_abundance == "Visual")
    
    pred <- abundance_model_coefficients["(Intercept)"] +
      abundance_model_coefficients["No..of.Years"] * log(input$study_duration) +
      abundance_model_coefficients["No..of.Sites..Avg."] * log(input$num_sites) +
      abundance_model_coefficients["Sampling.Time.min"] * log(input$sampling_time) +
      abundance_model_coefficients["Visual"] * is_visual +
      abundance_model_coefficients["Sampling.Time.min:Visual"] * log(input$sampling_time) * is_visual
    
    return(exp(pred))
  })
  
  # Simplified richness prediction function
  predict_richness <- reactive({
    is_visual <- as.numeric(input$sampling_method_richness == "Visual")
    is_species <- as.numeric(input$id_level == "Species")
    
    # Using fixed values for other parameters
    fixed_year <- 2023
    fixed_sites <- 10  # You can adjust these default values
    
    pred <- richness_model_coefficients["(Intercept)"] +
      richness_model_coefficients["Start.Year"] * fixed_year +
      richness_model_coefficients["No..of.Sites..Avg."] * fixed_sites +
      richness_model_coefficients["Sample.Trips.Year..Avg."] * input$value_Trips_year +
      richness_model_coefficients["Visual"] * is_visual +
      richness_model_coefficients["Species"] * is_species
    
    return(max(0, pred))
  })
  
  # Richness-specific plots
  output$plot_trips <- renderPlot({
    ggplot(corpus, aes(x = trips_per_year, y = Richness)) +
      geom_point(color = "#69b3a2", alpha = 0.6) +
      geom_point(data = data.frame(x = input$value_Trips_year, y = predict_richness()),
                 aes(x = x, y = y),
                 color = "red", size = 4) +
      theme_minimal() +
      labs(x = "Trips per Year", y = "Richness")
  })
  
  output$plot_level <- renderPlot({
    corpus_level <- corpus %>%
      group_by(identification_level) %>%
      summarise(Richness = mean(Richness, na.rm = TRUE))
    
    ggplot(corpus_level, aes(x = identification_level, y = Richness)) +
      geom_col(fill = "#69b3a2") +
      geom_point(data = data.frame(x = input$id_level, y = predict_richness()),
                 aes(x = x, y = y),
                 color = "red", size = 4) +
      theme_minimal() +
      labs(x = "Level of Identification", y = "Richness")
  })
  
  output$plot_method_richness <- renderPlot({
    corpus_method <- corpus %>%
      group_by(Sampling.Method) %>%
      summarise(Richness = mean(Richness, na.rm = TRUE))
    
    ggplot(corpus_method, aes(x = Sampling.Method, y = Richness)) +
      geom_col(fill = "#69b3a2") +
      geom_point(data = data.frame(x = input$sampling_method_richness, y = predict_richness()),
                 aes(x = x, y = y),
                 color = "red", size = 4) +
      theme_minimal() +
      labs(x = "Collection Method", y = "Richness")
  })
  
  # Keep abundance plots (renamed to avoid conflicts)
  output$plot_duration <- renderPlot({
    ggplot(corpus, aes(x = study_duration, y = Abundance)) +
      geom_point(color = "#69b3a2", alpha = 0.6) +
      geom_point(data = data.frame(x = input$study_duration, y = predict_abundance()),
                 aes(x = x, y = y),
                 color = "red", size = 4) +
      theme_minimal() +
      labs(x = "Study Duration (years)", y = "Abundance")
  })
  
  output$plot_sites <- renderPlot({
    ggplot(corpus, aes(x = num_sites, y = Abundance)) +
      geom_point(color = "#69b3a2", alpha = 0.6) +
      geom_point(data = data.frame(x = input$num_sites, y = predict_abundance()),
                 aes(x = x, y = y),
                 color = "red", size = 4) +
      theme_minimal() +
      labs(x = "Number of Sites", y = "Abundance")
  })
  
  output$plot_sampling <- renderPlot({
    ggplot(corpus, aes(x = sampling_time, y = Abundance)) +
      geom_point(color = "#69b3a2", alpha = 0.6) +
      geom_point(data = data.frame(x = input$sampling_time, y = predict_abundance()),
                 aes(x = x, y = y),
                 color = "red", size = 4) +
      theme_minimal() +
      labs(x = "Sampling Time (minutes)", y = "Abundance")
  })
  
  output$plot_method_abundance <- renderPlot({
    corpus_method <- corpus %>%
      group_by(Sampling.Method) %>%
      summarise(Abundance = mean(Abundance, na.rm = TRUE))
    
    ggplot(corpus_method, aes(x = Sampling.Method, y = Abundance)) +
      geom_col(fill = "#69b3a2") +
      geom_point(data = data.frame(x = input$sampling_method_abundance, y = predict_abundance()),
                 aes(x = x, y = y),
                 color = "red", size = 4) +
      theme_minimal() +
      labs(x = "Collection Method", y = "Abundance")
  })
}

shinyApp(ui = ui, server = server)