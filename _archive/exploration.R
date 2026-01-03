library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(shinythemes)

# UI Definition
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
    background-color: #e6f3ff !important;  /* Light blue hover color */
  }
")),
  
  titlePanel("Explore Levenson Index 2024"),
  
  # Top right home button
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

# Server logic
server <- function(input, output, session) {
  data <- reactive({
    read.csv("data_clean.csv")
  })
  
  
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
            ),
            showscale = TRUE,
            zmin = 0,
            zmax = max(country_counts$count),
            colorbar = list(title = "Number of Studies")) %>%
      layout(
        title = list(text = "Number of Studies by Country", y = 0.95),
        geo = list(
          showframe = TRUE,
          showcoastlines = TRUE,
          projection = list(type = 'miller'),
          lataxis = list(range = c(-65, 75)),
          bgcolor = 'rgb(255, 255, 255)',
          showland = TRUE,
          landcolor = 'rgb(250, 250, 250)',
          countrycolor = 'rgb(220, 220, 220)'
        ),
        paper_bgcolor = 'white',
        plot_bgcolor = 'white',
        margin = list(l = 0, r = 0, t = 50, b = 0)
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
        geo = list(scope = 'usa'),
        paper_bgcolor = 'white',
        plot_bgcolor = 'white',
        margin = list(l = 0, r = 0, t = 50, b = 0)
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
                marker = list(
                  symbol = "triangle-up",
                  size = 12,
                  color = "#c6dbef",
                  line = list(color = "#08306b", width = 1)
                ),
                text = ~paste("Study:", title,
                              "<br>Year:", start_year,
                              "<br>Duration:", duration_years, "years",
                              "<br>Country:", country),
                hoverinfo = "text",
                name = "Studies") %>%
      add_trace(data = trend_data,
                x = ~start_year,
                y = ~duration_years,
                type = "scatter",
                mode = "lines",
                line = list(color = '#2171b5', width = 2),
                name = "Trend") %>%
      layout(
        title = NULL,
        xaxis = list(title = "Start Year"),
        yaxis = list(title = "Duration (Years)"),
        paper_bgcolor = 'white',
        plot_bgcolor = 'rgb(250, 250, 250)'
      )
  })
  
  output$siteCounts <- renderPlotly({
    req(data())
    df <- data()
    
    stats <- summary(df$site_count)
    median_sites <- stats["Median"]
    iqr_sites <- IQR(df$site_count, na.rm = TRUE)
    
    plot_ly(df,
            x = ~log10(site_count),
            type = "histogram",
            nbinsx = 30,
            marker = list(
              color = "#9ecae1",
              line = list(color = "white", width = 0.5)
            )) %>%
      layout(
        title = NULL,
        xaxis = list(title = "Number of Sites (log10 scale)"),
        yaxis = list(title = "Frequency"),
        paper_bgcolor = 'white',
        plot_bgcolor = 'rgb(250, 250, 250)'
      )
  })
  
  output$samplingTrips <- renderPlotly({
    req(data())
    df <- data()
    
    stats <- summary(df$sample_trips_per_year_avrg)
    median_trips <- stats["Median"]
    iqr_trips <- IQR(df$sample_trips_per_year_avrg, na.rm = TRUE)
    
    plot_ly(df,
            x = ~sample_trips_per_year_avrg,
            type = "histogram",
            nbinsx = 30,
            marker = list(
              color = "#4292c6",
              line = list(color = "white", width = 0.5)
            )) %>%
      layout(
        title = NULL,
        xaxis = list(title = "Trips per Year"),
        yaxis = list(title = "Frequency"),
        paper_bgcolor = 'white',
        plot_bgcolor = 'rgb(250, 250, 250)'
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
        layout(
          yaxis = list(title = "Time (minutes)"),
          margin = list(l = 50, r = 50, b = 50, t = 50)
        ),
      
      plot_ly(days_data,
              x = ~method,
              y = ~time,
              type = "box",
              color = ~method,
              colors = c("#6baed6", "#2171b5"),
              boxpoints = "all",
              jitter = 0.3,
              pointpos = 0) %>%
        layout(
          yaxis = list(title = "Time (days)"),
          margin = list(l = 50, r = 50, b = 50, t = 50)
        ),
      
      nrows = 1,
      widths = c(0.4, 0.6),
      margin = 0.05
    ) %>%
      layout(
        title = NULL,
        showlegend = FALSE,
        paper_bgcolor = 'white',
        plot_bgcolor = 'rgb(250, 250, 250)',
        height = 500
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
            textfont = list(size = 14, weight = "bold"),
            marker = list(
              colors = c("#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#08519c")
            )) %>%
      layout(
        title = NULL,
        showlegend = TRUE,
        legend = list(font = list(size = 14, weight = "bold")),
        paper_bgcolor = 'white',
        plot_bgcolor = 'white',
        height = 600,
        width = 800
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
