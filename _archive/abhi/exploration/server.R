# server_exploration.R
server <- function(input, output, session) {
  
  # Geographic visualizations
  output$worldMap <- renderPlotly({
    country_counts <- data %>%
      count(country)
    
    plot_ly(country_counts,
            type = 'choropleth',
            locationmode = 'country names',
            locations = ~country,
            z = ~n,
            text = ~paste(country, ": ", n, " studies"),
            colorscale = list(c(0,'rgb(239, 243, 255)'), 
                              c(1,'rgb(17, 95, 154)')),
            showscale = TRUE) %>%
      layout(
        title = "Number of Studies by Country",
        geo = list(
          showframe = TRUE,
          showcoastlines = TRUE,
          projection = list(type = 'miller'),
          bgcolor = 'rgb(240, 240, 240)',
          showland = TRUE,
          landcolor = 'rgb(240, 240, 240)'
        ),
        margin = list(l = 0, r = 0, t = 30, b = 0)
      )
  })
  
  output$usaStateMap <- renderPlotly({
    state_counts <- data %>%
      filter(!is.na(state_usa)) %>%
      count(state_usa)
    
    plot_ly(state_counts, 
            type = "choropleth",
            locationmode = "USA-states",
            locations = ~state_usa,
            z = ~n,
            text = ~paste(state_usa, ": ", n, " studies"),
            colorscale = "Viridis") %>%
      layout(
        geo = list(scope = 'usa'),
        title = "Studies per US State"
      )
  })
  
  # Study duration trend
  output$durationTrend <- renderPlotly({
    lm_fit <- lm(duration_years ~ start_year, data = data)
    trend_data <- data.frame(
      start_year = seq(min(data$start_year), 
                       max(data$start_year), 
                       length.out = 100)
    )
    trend_data$duration_years <- predict(lm_fit, trend_data)
    
    plot_ly() %>%
      add_trace(data = data,
                x = ~start_year,
                y = ~duration_years,
                type = "scatter",
                mode = "markers",
                marker = list(symbol = "triangle-up", size = 12),
                text = ~paste("Study:", title,
                              "<br>Duration:", duration_years, "years",
                              "<br>Country:", country),
                name = "Studies") %>%
      add_trace(data = trend_data,
                x = ~start_year,
                y = ~duration_years,
                type = "scatter",
                mode = "lines",
                line = list(color = 'red'),
                name = "Trend") %>%
      layout(
        title = "Study Duration Trend Over Time",
        xaxis = list(title = "Start Year"),
        yaxis = list(title = "Duration (Years)")
      )
  })
  
  # Site information plots
  output$siteCounts <- renderPlotly({
    plot_ly(data,
            x = ~log10(site_count),
            type = "histogram",
            nbinsx = 30) %>%
      layout(
        title = sprintf("Distribution of Sites<br>Median = %.1f, IQR = %.1f",
                        median(data$site_count, na.rm = TRUE),
                        IQR(data$site_count, na.rm = TRUE)),
        xaxis = list(title = "Sites (log10 scale)"),
        yaxis = list(title = "Count")
      )
  })
  
  output$samplingTrips <- renderPlotly({
    plot_ly(data,
            x = ~sample_trips_per_year_avrg,
            type = "histogram",
            nbinsx = 30) %>%
      layout(
        title = sprintf("Sampling Trips per Year<br>Median = %.1f, IQR = %.1f",
                        median(data$sample_trips_per_year_avrg, na.rm = TRUE),
                        IQR(data$sample_trips_per_year_avrg, na.rm = TRUE)),
        xaxis = list(title = "Trips/Year"),
        yaxis = list(title = "Count")
      )
  })
  
  # Method visualizations
  output$collectionMethods <- renderPlotly({
    collection_data <- data.frame(
      method = rep(c("Net", "Pan", "Visual"), each = nrow(data)),
      time = c(data$net_time_minute, 
               data$pan_time_days * 1440, 
               data$visual_time_days * 1440)
    ) %>% na.omit()
    
    plot_ly(collection_data, 
            x = ~method,
            y = ~time,
            type = "box",
            boxpoints = "all",
            jitter = 0.3,
            marker = list(size = 8)) %>%
      layout(
        title = "Sampling Time by Method",
        xaxis = list(title = "Method"),
        yaxis = list(
          title = "Time (minutes)",
          type = "log"
        )
      )
  })
  
  output$identificationMethods <- renderPlotly({
    id_counts <- data %>%
      count(identification_least) %>%
      filter(!is.na(identification_least))
    
    plot_ly(id_counts,
            labels = ~identification_least,
            values = ~n,
            type = "pie",
            textinfo = "label+percent") %>%
      layout(
        title = "Lowest Level of Identification",
        showlegend = TRUE
      )
  })
}