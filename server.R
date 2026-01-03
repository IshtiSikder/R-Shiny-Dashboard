server <- function(input, output, session) {
  
  # Data loading 
  data <- reactive({
    read.csv("exploratory_data.csv")
  })
  
  # Main container output
  output$main_container <- renderUI({
    create_main_container(current_view())
  })
  
  # Button observers for navigation
  observeEvent(input$predict_button, {
    current_view("predict")
  })
  
  observeEvent(input$explore_button, {
    current_view("explore")
  })
  
  observeEvent(input$home_button, {
    current_view("landing")
  })
  
  # Reactive values for predictions
  abundance_prediction <- reactive({
    predict_abundance(input)
  })
  
  richness_prediction <- reactive({
    # Call abundance_prediction() to get its numeric value
    pred_abundance <- abundance_prediction()
    
    # Use that value in predict_richness
    predict_richness(pred_abundance)
  })
  
  # Output renderers
  output$abundance_richness_plot <- renderPlot({
    req(abundance_prediction(), richness_prediction())
    create_prediction_plot(abundance_prediction(), richness_prediction())
  })
  
  output$predictions <- renderPrint({
    req(abundance_prediction(), richness_prediction())
    cat("Predicted Abundance:", round(abundance_prediction(), 2),
        "\nPredicted Richness:", round(richness_prediction(), 2))
  })


  # World map output
  output$worldMap <- renderPlotly({
    req(data())
    create_world_map(data())
  })
  
  # USA state map output
  output$usaStateMap <- renderPlotly({
    req(data())
    create_usa_state_map(data())
  })
  
  # Duration trend output
  output$durationTrend <- renderPlotly({
    req(data())
    create_duration_trend_plot(data())
  })
  
  # Site counts output
  output$siteCounts <- renderPlotly({
    req(data())
    create_site_counts_plot(data())
  })
  
  # Sampling trips output
  output$samplingTrips <- renderPlotly({
    req(data())
    create_sampling_trips_plot(data())
  })
  
  # Collection methods output
  output$collectionMethods <- renderPlotly({
    req(data())
    create_collection_methods_plot(data())
  })
  
  # Identification methods output
  output$identificationMethods <- renderPlotly({
    req(data())
    create_identification_methods_plot(data())
  })
}