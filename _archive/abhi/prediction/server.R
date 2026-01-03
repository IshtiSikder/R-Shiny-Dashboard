# prediction/server.R

# Model coefficients based on statistical analysis
richness_model <- list(
  intercept = 3778.348461,
  start_year = -1.902323,
  no_of_sites = 10.811558,
  trips_year = 20.531091,
  visual = -33.329740,
  species = 26.267014
)

server <- function(input, output, session) {
  
  # Reactive values for input validation
  valid_inputs <- reactive({
    req(input$value_Start_year)
    req(input$value_No_of_sites)
    req(input$value_Trips_year)
    req(input$collection_method)
    req(input$identification_level)
    
    return(TRUE)
  })
  
  # Prediction function for richness
  predict_richness <- function(start_year, sites, trips, method, level) {
    # Base prediction from intercept and main effects
    pred <- richness_model$intercept +
      (start_year * richness_model$start_year) +
      (sites * richness_model$no_of_sites) +
      (trips * richness_model$trips_year)
    
    # Add method effect if Visual
    if (method == "Visual") {
      pred <- pred + richness_model$visual
    }
    
    # Add species level effect
    if (level == "Species") {
      pred <- pred + richness_model$species
    }
    
    return(max(0, pred))  # Ensure prediction is not negative
  }
  
  # Prediction output
  output$prediction <- renderText({
    req(valid_inputs())
    
    pred <- predict_richness(
      input$value_Start_year,
      input$value_No_of_sites,
      input$value_Trips_year,
      input$collection_method,
      input$identification_level
    )
    
    paste("Predicted Richness:", round(pred, 1))
  })
  
  # Plot rendering
  output$richness_plot <- renderPlot({
    req(valid_inputs())
    
    # Create sequence of trips/year for plotting
    trips <- seq(1, max(input$value_Trips_year * 2, 50), length.out = 100)
    
    # Generate predictions
    predictions <- sapply(trips, function(t) {
      predict_richness(
        input$value_Start_year,
        input$value_No_of_sites,
        t,
        input$collection_method,
        input$identification_level
      )
    })
    
    # Create plot data
    plot_data <- data.frame(
      trips = trips,
      richness = predictions
    )
    
    # Current prediction point
    current_pred <- predict_richness(
      input$value_Start_year,
      input$value_No_of_sites,
      input$value_Trips_year,
      input$collection_method,
      input$identification_level
    )
    
    # Generate plot
    ggplot(plot_data, aes(x = trips, y = richness)) +
      geom_line(color = "#69b3a2", size = 1) +
      geom_point(data = data.frame(x = input$value_Trips_year, y = current_pred),
                 aes(x = x, y = y), color = "red", size = 4) +
      theme_minimal() +
      labs(
        title = "Predicted Richness vs Sampling Trips",
        x = "Trips per Year",
        y = "Predicted Species Richness"
      )
  })
}