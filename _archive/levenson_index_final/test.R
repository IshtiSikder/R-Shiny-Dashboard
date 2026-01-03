# load necessary packages
library(shiny)
library(ggplot2)
library(ggExtra)
library(shinythemes)
library(bslib)
library(fontawesome)
library(tidyverse)
library(plotly)
library(DT)

# Load the prepared data
load("/cloud/project/R-Shiny-Bees/levenson_index_final/model_results.RData")

# Define base plot function (for abundance vs richness) 
make_base_plot <- function() {
  p3a <- ggplot(data=sig.corpus, aes(x=Abundance)) +
    geom_ribbon(aes(ymin=Conf2.5, ymax=Conf97.5), fill="#005AB5", alpha=.2) +
    geom_segment(data=lds, aes(x=Abundance, xend=Abundance, y=1, yend=mx),
                 color="grey65", linetype="dotdash", linewidth=1) +
    geom_segment(data=lds, aes(x=10, xend=Abundance, y=mx, yend=mx),
                 color="grey65", linetype="dotdash", linewidth=1) +
    geom_line(aes(y=Median), color="#005AB5", linewidth=1) +
    geom_point(aes(y=Richness, size=size, shape=shape), color="grey75") +
    geom_text(data=lds, aes(x=10, y=mx+1, label=lab), vjust=0, hjust=0, size=5) +
    geom_text(data=lds, aes(x=Abundance, y=1, label=round(Abundance,0)), vjust=1, hjust=0, size=5) +
    scale_x_continuous(breaks=c(10,100,1000,10000,30000), trans="log10") +
    scale_y_continuous("Richness", trans="log10") +
    scale_size_identity() +
    scale_shape_identity() +
    theme_minimal() +
    theme(
      axis.title = element_text(face="bold", size=12),
      axis.text = element_text(size=10),
      plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")
    )
  return(p3a)
}

# define UI skeleton
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  tags$style(HTML("
    .navbar {
      background-color: white !important;
      border-color: #ddd !important;
    }
    .navbar-default .navbar-nav > li > a {
      color: #666 !important;
    }
    .navbar-default .navbar-nav > .active > a,
    .navbar-default .navbar-nav > .active > a:focus,
    .navbar-default .navbar-nav > .active > a:hover {
      background-color: #f8f8f8 !important;
      color: #333 !important;
    }
    .navbar-default .navbar-nav > li > a:hover {
      background-color: #e6f3ff !important;
    }
  ")),
  uiOutput("main_container")
)

# define server function
server <- function(input, output, session) {
  current_view <- reactiveVal("landing")
  
# Data loading for explore view (why is this loading data_clean.csv instead of model_results.RData)
  data <- reactive({
    read.csv("data_clean.csv")
  })
  

#Lnading page + main dashboard + exploratory analysis  
  output$main_container <- renderUI({
    if (current_view() == "landing") {
      fluidPage(
        titlePanel("Levenson Index"),
        p("Authors, APA 7th Reference, doi/link to publication"),
        tags$a(
          href = "#",
          icon("home"),
          style = "position:absolute; top:10px; right:10px;",
          onclick = "window.location.reload(); return false;"
        ),
        p("Description. Lorem ipsum dolor sit amet, consectetur adipiscing elit..."),
        actionButton("predict_button", "Predict Abundance/Richness with Your Study Design",
                     style = "background-color: #1976D2; color: white; font-weight: bold;"),
        br(), br(),
        p("Description. Lorem ipsum dolor sit amet..."),
        actionButton("explore_button", "Explore the Levenson Index",
                     style = "background-color: #1976D2; color: white; font-weight: bold;")
      )
    } else if (current_view() == "predict") {
      fluidPage(
        titlePanel("The Levenson Index 🐝"),
        tags$a(
          href = "#",
          icon("home"),
          style = "position:absolute; top:10px; right:10px;",
          onclick = "window.location.reload(); return false;"
        ),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            h4("Set Your Study Design"),
            numericInput("num_sites", "Number of Sites", 
                         min = 1, max = 100, value = 10),
            sliderInput("study_duration", "Study Duration (years)",
                        min = 1, max = 10, value = 3),
            sliderInput("sampling_time", "Sampling Time (minutes)",
                        min = 0, max = 1000, value = 100),
            sliderInput("trips_per_year", "Trips per Year",
                        min = 1, max = 52, value = 12),
            radioButtons("Least.Specific.N", "Level of Identification",
                         choices = c("Genus", "Family", "Species", "Morphospecies")),
            radioButtons("sampling_method", "Collection Method",
                         choices = c("Visual", "Net", "Pan")),
            hr(),
            h4("Predictions"),
            verbatimTextOutput("predictions")
          ),
          mainPanel(
            width = 9,
            plotOutput("abundance_richness_plot", height = "600px")
          )
        )
      )
    } else if (current_view() == "explore") {
      fluidPage(
        titlePanel("Explore Levenson Index 2024"),
        tags$a(
          href = "#",
          icon("home"),
          style = "position:absolute; top:10px; right:10px;",
          onclick = "window.location.reload(); return false;"
        ),
        navbarPage(
          title = "",
          tabPanel("Geographic Distribution",
                   div(style = "width: 100%; margin: auto;",
                       plotlyOutput("worldMap", height = "400px")
                   ),
                   br(),
                   div(style = "width: 80%; margin: auto;",
                       plotlyOutput("usaStateMap", height = "400px")
                   )
          ),
          tabPanel("Study Duration Trend",
                   h3("Study Duration Trend"),
                   div(style = "margin-top: 30px;",
                       plotlyOutput("durationTrend", height = "400px")
                   )
          ),
          tabPanel("Sites Information",
                   h3("Sites Information"),
                   div(style = "margin-top: 30px;",
                       fluidRow(
                         column(6, plotlyOutput("siteCounts")),
                         column(6, plotlyOutput("samplingTrips"))
                       )
                   )
          ),
          tabPanel("Collection Methods",
                   h3("Collection Methods"),
                   div(style = "margin-top: 30px;",
                       plotlyOutput("collectionMethods", height = "400px")
                   )
          ),
          tabPanel("Identification Methods",
                   h3("Identification Methods"),
                   div(style = "margin-top: 30px;",
                       plotlyOutput("identificationMethods", height = "300px")
                   )
          )
        )
      )
    }
  })
  
  # Prediction algos (abundance + richness)
  predict_abundance <- reactive({
    req(input$sampling_method, input$sampling_time, input$study_duration, input$num_sites)
    is_visual <- as.numeric(input$sampling_method == "Visual")
    
    pred <- abundance_model_coefficients["(Intercept)"] +
      abundance_model_coefficients["No..of.Years"] * log(input$study_duration) +
      abundance_model_coefficients["No..of.Sites..Avg."] * log(input$num_sites) +
      abundance_model_coefficients["Sampling.Time.min"] * log(input$sampling_time) +
      abundance_model_coefficients["Visual"] * is_visual +
      abundance_model_coefficients["Sampling.Time.min:Visual"] * log(input$sampling_time) * is_visual
    
    return(exp(pred))
  })
  
  predict_richness <- reactive({
    req(input$sampling_method, input$Least.Specific.N, input$num_sites, input$trips_per_year)
    is_visual <- as.numeric(input$sampling_method == "Visual")
    is_species <- as.numeric(input$Least.Specific.N == "Species")
    
    pred <- richness_model_coefficients["(Intercept)"] +
      richness_model_coefficients["Start.Year"] * 2023 +
      richness_model_coefficients["No..of.Sites..Avg."] * input$num_sites +
      richness_model_coefficients["Sample.Trips.Year..Avg."] * input$trips_per_year +
      richness_model_coefficients["Visual"] * is_visual +
      richness_model_coefficients["Species"] * is_species
    
    return(max(0, pred))
  })
   
  # Prediction outputs (building the abundance vs richness plot)
  output$abundance_richness_plot <- renderPlot({
    req(predict_abundance(), predict_richness())
    abundance_pred <- predict_abundance()
    richness_pred <- predict_richness()
    
    base_plot <- make_base_plot() +
      geom_hline(yintercept = richness_pred, color = "firebrick", 
                 linetype = "dashed", alpha = 0.5) +
      geom_vline(xintercept = abundance_pred, color = "firebrick", 
                 linetype = "dashed", alpha = 0.5) +
      geom_point(data = data.frame(x = abundance_pred, y = richness_pred),
                 aes(x = x, y = y),
                 color = "firebrick", size = 10, shape = 15, alpha = 0.3) +
      geom_point(data = data.frame(x = abundance_pred, y = richness_pred),
                 aes(x = x, y = y),
                 color = "firebrick", size = 3, shape = 15)
    
    ggMarginal(
      base_plot,
      type = "histogram",
      fill = "grey75",
      color = "grey75",
      size = 3,
      margins = "both",
      xparams = list(bins = 30),
      yparams = list(bins = 30)
    )
  })
  
  # Print abundance and richness outputs on the side panel
  output$predictions <- renderPrint({
    req(predict_abundance(), predict_richness())
    cat("Predicted Abundance:", round(predict_abundance(), 2),
        "\nPredicted Richness:", round(predict_richness(), 2))
  })
  
  # Explore view outputs
  output$worldMap <- renderPlotly({
    req(data())
    country_counts <- data() %>%
      group_by(country) %>%
      summarise(count = n()) %>%
      ungroup()
    
    plot_ly(country_counts,
            type = 'choropleth',
            locationmode = 'country names',
            locations = ~country,
            z = ~count,
            text = ~paste(country, ": ", count, " studies"),
            colorscale = list(
              c(0, "#f7fbff"),
              c(0.2, "#deebf7"),
              c(0.4, "#c6dbef"),
              c(0.6, "#9ecae1"),
              c(0.8, "#6baed6"),
              c(1, "#2171b5")
            )) %>%
      layout(
        title = list(text = "Number of Studies by Country", y = 0.95),
        geo = list(showframe = TRUE,
                   showcoastlines = TRUE,
                   projection = list(type = 'miller'))
      )
  })
  
  output$usaStateMap <- renderPlotly({
    req(data())
    state_counts <- data() %>%
      filter(!is.na(state_usa)) %>%
      group_by(state_usa) %>%
      summarise(count = n()) %>%
      ungroup()
    
    plot_ly(state_counts, 
            type = "choropleth",
            locationmode = "USA-states",
            locations = ~state_usa,
            z = ~count,
            text = ~paste(state_usa, ": ", count, " studies"),
            colorscale = list(
              c(0, "#f7fbff"),
              c(0.2, "#deebf7"),
              c(0.4, "#c6dbef"),
              c(0.6, "#9ecae1"),
              c(0.8, "#6baed6"),
              c(1, "#2171b5")
            )) %>%
      layout(
        title = list(text = "Studies per US State", y = 0.95),
        geo = list(scope = 'usa')
      )
  })
  
  output$durationTrend <- renderPlotly({
    req(data())
    df <- data()
    
    lm_fit <- lm(duration_years ~ start_year, data = df)
    trend_data <- data.frame(
      start_year = seq(min(df$start_year), max(df$start_year), length.out = 100)
    )
    trend_data$duration_years <- predict(lm_fit, trend_data)
    
    plot_ly() %>%
      add_trace(data = df,
                x = ~start_year,
                y = ~duration_years,
                type = "scatter",
                mode = "markers",
                marker = list(color = "#c6dbef")) %>%
      add_trace(data = trend_data,
                x = ~start_year,
                y = ~duration_years,
                type = "scatter",
                mode = "lines",
                line = list(color = '#2171b5'))
  })
  
  output$siteCounts <- renderPlotly({
    req(data())
    df <- data()
    
    plot_ly(df,
            x = ~log10(site_count),
            type = "histogram",
            marker = list(color = "#9ecae1")) %>%
      layout(
        xaxis = list(title = "Number of Sites (log10 scale)"),
        yaxis = list(title = "Frequency")
      )
  })
  
  output$samplingTrips <- renderPlotly({
    req(data())
    df <- data()
    
    plot_ly(df,
            x = ~sample_trips_per_year_avrg,
            type = "histogram",
            marker = list(color = "#4292c6")) %>%
      layout(
        xaxis = list(title = "Trips per Year"),
        yaxis = list(title = "Frequency")
      )
  })
  
  output$collectionMethods <- renderPlotly({
    req(data())
    df <- data()
    
    net_data <- data.frame(
      method = "Net",
      time = as.numeric(df$net_time_minute),
      type = "minutes"
    ) %>% filter(!is.na(time))
    
    
    days_data <- data.frame(
      method = c(rep("Pan", nrow(df)), rep("Visual", nrow(df))),
      time = as.numeric(c(df$pan_time_days, df$visual_time_days)),
      type = "days"
    ) %>% filter(!is.na(time))
    
    subplot(
      plot_ly(net_data,
              x = ~method,
              y = ~time,
              type = "box",
              name = "Net (minutes)",
              marker = list(color = "#c6dbef"),
              boxpoints = "all",
              jitter = 0.3,
              pointpos = 0) %>%
        layout(yaxis = list(title = "Time (minutes)")),
      
      plot_ly(days_data,
              x = ~method,
              y = ~time,
              type = "box",
              color = ~method,
              colors = c("#6baed6", "#2171b5"),
              boxpoints = "all",
              jitter = 0.3,
              pointpos = 0) %>%
        layout(yaxis = list(title = "Time (days)")),
      
      nrows = 1,
      widths = c(0.4, 0.6)
    )
  })
  
  output$identificationMethods <- renderPlotly({
    req(data())
    id_counts <- data() %>%
      group_by(identification_least) %>%
      summarise(count = n()) %>%
      filter(!is.na(identification_least))
    
    plot_ly(id_counts,
            labels = ~identification_least,
            values = ~count,
            type = "pie",
            textinfo = "label+percent",
            marker = list(
              colors = c("#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#08519c")
            ))
  })
  
  # Button observers
  observeEvent(input$predict_button, {
    current_view("predict")
  })
  
  observeEvent(input$explore_button, {
    current_view("explore")
  })
}

shinyApp(ui = ui, server = server)
