library(shiny)
library(bslib)
library(ggplot2)
library(reshape2)
library(bsicons)

# Define random weights for Richness prediction
richness_weights <- list(
  Trips_year = 0.15,
  Method_net = 2.5,
  Method_visual = 1.8,
  Method_pan = 2.0,
  Level_species = 3.0,
  Level_genus = 2.5,
  Level_family = 2.0,
  Level_morphospecies = 2.2,
  intercept = 10
)

# Define random weights for Abundance prediction
abundance_weights <- list(
  Study_duration = 0.8,
  No_of_sites = 0.12,
  Sampling_time = 0.05,
  Method_net = 15,
  Method_visual = 12,
  Method_pan = 10,
  intercept = 20
)

ui <- page_sidebar(
  title = "Bees for life 🐝",
  theme = bs_theme(
    bootswatch = "minty"
  ) |> bs_add_rules(
    ".sidebar { background-color: #E6EEF2; }
     .sidebar-title { font-size: 0.9rem !important; }
     .form-group { margin-bottom: 0.5rem !important; }
     .form-label { font-size: 0.75rem !important; margin-bottom: 2px !important; }
     .form-select { font-size: 0.8rem !important; padding: 4px 8px !important; }
     .float-inputs-section .form-label { font-size: 0.7rem !important; }
     .float-inputs-section .irs { font-size: 0.75rem !important; }
     .float-inputs-section .btn { font-size: 0.75rem !important; }
     .float-inputs-section .form-control { font-size: 0.75rem !important; }
     .float-inputs-section h4 { font-size: 0.75rem !important; margin-bottom: 4px !important; }
     .button-grid { display: grid; grid-template-columns: repeat(2, 1fr); gap: 4px; }
     .button-grid .btn { width: 100%; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
     .variable-label { font-size: 0.7rem !important; }"
  ),
  
  sidebar = sidebar(
    width = "288px",
    style = "border-right: 1px solid #D1DCE1;",
    
    div(
      style = "margin-bottom: 8px;",
      selectInput(
        "select_var", 
        "Select Variable:",
        choices = c("Richness", "Abundance")
      )
    ),
    
    uiOutput("variable_panels")
  ),
  
  navset_card_tab(
    title = "Plots",
    full_screen = TRUE,
    nav_panel(
      "Trips/year vs Richness",
      plotOutput("plot_trips")
    ),
    nav_panel(
      "Collection method vs Richness",
      plotOutput("plot_method")
    ),
    nav_panel(
      "Level of identification vs Richness",
      plotOutput("plot_level")
    )
  ),
  
  card(
    card_header("Additional Information"),
    verbatimTextOutput("text1"),
    tags$hr(),
    h4("Model Prediction:"),
    verbatimTextOutput("prediction")
  )
)

server <- function(input, output, session) {
  
  selected_method <- reactiveVal(NULL)
  selected_level <- reactiveVal(NULL)
  
  # Reactive dataframe for Richness
  df_Richness <- reactive({
    # Create method indicators
    method_net <- as.numeric(identical(selected_method(), "net"))
    method_visual <- as.numeric(identical(selected_method(), "visual"))
    method_pan <- as.numeric(identical(selected_method(), "pan"))
    
    # Create level indicators
    level_species <- as.numeric(identical(selected_level(), "species"))
    level_genus <- as.numeric(identical(selected_level(), "genus"))
    level_family <- as.numeric(identical(selected_level(), "family"))
    level_morphospecies <- as.numeric(identical(selected_level(), "morphospecies"))
    
    data.frame(
      Trips_year = input$value_Trips_year,
      Method_net = method_net,
      Method_visual = method_visual,
      Method_pan = method_pan,
      Level_species = level_species,
      Level_genus = level_genus,
      Level_family = level_family,
      Level_morphospecies = level_morphospecies
    )
  })
  
  # Reactive dataframe for Abundance
  df_Abundance <- reactive({
    # Create method indicators
    method_net <- as.numeric(identical(selected_method(), "net"))
    method_visual <- as.numeric(identical(selected_method(), "visual"))
    method_pan <- as.numeric(identical(selected_method(), "pan"))
    
    data.frame(
      Study_duration = input$value_Study_duration,
      No_of_sites = input$value_No_of_sites,
      Sampling_time = input$value_Sampling_time,
      Method_net = method_net,
      Method_visual = method_visual,
      Method_pan = method_pan
    )
  })
  
  # Prediction function for Richness
  predict_richness <- function(df) {
    with(df, 
         richness_weights$intercept +
           Trips_year * richness_weights$Trips_year +
           Method_net * richness_weights$Method_net +
           Method_visual * richness_weights$Method_visual +
           Method_pan * richness_weights$Method_pan +
           Level_species * richness_weights$Level_species +
           Level_genus * richness_weights$Level_genus +
           Level_family * richness_weights$Level_family +
           Level_morphospecies * richness_weights$Level_morphospecies
    )
  }
  
  # Prediction function for Abundance
  predict_abundance <- function(df) {
    with(df,
         abundance_weights$intercept +
           Study_duration * abundance_weights$Study_duration +
           No_of_sites * abundance_weights$No_of_sites +
           Sampling_time * abundance_weights$Sampling_time +
           Method_net * abundance_weights$Method_net +
           Method_visual * abundance_weights$Method_visual +
           Method_pan * abundance_weights$Method_pan
    )
  }
  output$variable_panels <- renderUI({
    if(input$select_var == "Richness") {
      tagList(
        card(
          style = "margin-bottom: 8px;",
          sliderInput(
            inputId = "value_Trips_year",
            label = tags$span(class = "variable-label", "Trips/year"),
            min = 0,
            max = 200,
            value = 50,
            width = "100%"
          )
        ),
        
        card(
          style = "margin-bottom: 8px;",
          tags$span(class = "variable-label", "Collection method"),
          div(
            style = "margin-top: 4px;",
            class = "button-grid",
            actionButton(
              inputId = "net_btn",
              label = "Net",
              class = if(identical(selected_method(), "net")) "btn-dark btn-sm" else "btn-primary btn-sm"
            ),
            actionButton(
              inputId = "visual_btn",
              label = "Visual",
              class = if(identical(selected_method(), "visual")) "btn-dark btn-sm" else "btn-primary btn-sm"
            ),
            actionButton(
              inputId = "pan_btn",
              label = "Pan",
              class = if(identical(selected_method(), "pan")) "btn-dark btn-sm" else "btn-primary btn-sm"
            )
          )
        ),
        
        card(
          style = "margin-bottom: 8px;",
          tags$span(class = "variable-label", "Level of identification"),
          div(
            style = "margin-top: 4px;",
            class = "button-grid",
            actionButton(
              inputId = "species_btn",
              label = "Species",
              class = if(identical(selected_level(), "species")) "btn-dark btn-sm" else "btn-primary btn-sm"
            ),
            actionButton(
              inputId = "genus_btn",
              label = "Genus",
              class = if(identical(selected_level(), "genus")) "btn-dark btn-sm" else "btn-primary btn-sm"
            ),
            actionButton(
              inputId = "family_btn",
              label = "Family",
              class = if(identical(selected_level(), "family")) "btn-dark btn-sm" else "btn-primary btn-sm"
            ),
            actionButton(
              inputId = "morphospecies_btn",
              label = "Morphospecies",
              class = if(identical(selected_level(), "morphospecies")) "btn-dark btn-sm" else "btn-primary btn-sm"
            )
          )
        )
      )
    } else {
      tagList(
        card(
          style = "margin-bottom: 8px;",
          sliderInput(
            inputId = "value_Study_duration",
            label = tags$span(class = "variable-label", "Study duration (years)"),
            min = 0,
            max = 10,
            value = 5,
            step = 0.1,
            width = "100%"
          )
        ),
        
        card(
          style = "margin-bottom: 8px;",
          sliderInput(
            inputId = "value_No_of_sites",
            label = tags$span(class = "variable-label", "No. of sites"),
            min = 0,
            max = 200,
            value = 50,
            width = "100%"
          )
        ),
        
        card(
          style = "margin-bottom: 8px;",
          sliderInput(
            inputId = "value_Sampling_time",
            label = tags$span(class = "variable-label", "Sampling time (minutes/day)"),
            min = 0,
            max = 200,
            value = 50,
            width = "100%"
          )
        ),
        
        card(
          style = "margin-bottom: 8px;",
          tags$span(class = "variable-label", "Collection method"),
          div(
            style = "margin-top: 4px;",
            class = "button-grid",
            actionButton(
              inputId = "net_btn",
              label = "Net",
              class = if(identical(selected_method(), "net")) "btn-dark btn-sm" else "btn-primary btn-sm"
            ),
            actionButton(
              inputId = "visual_btn",
              label = "Visual",
              class = if(identical(selected_method(), "visual")) "btn-dark btn-sm" else "btn-primary btn-sm"
            ),
            actionButton(
              inputId = "pan_btn",
              label = "Pan",
              class = if(identical(selected_method(), "pan")) "btn-dark btn-sm" else "btn-primary btn-sm"
            )
          )
        )
      )
    }
  })
  
  observeEvent(input$net_btn, {
    selected_method("net")
  })
  
  observeEvent(input$visual_btn, {
    selected_method("visual")
  })
  
  observeEvent(input$pan_btn, {
    selected_method("pan")
  })
  
  observeEvent(input$species_btn, {
    selected_level("species")
  })
  
  observeEvent(input$genus_btn, {
    selected_level("genus")
  })
  
  observeEvent(input$family_btn, {
    selected_level("family")
  })
  
  observeEvent(input$morphospecies_btn, {
    selected_level("morphospecies")
  })
  
  output$plot1 <- renderPlot({
    if(input$select_var == "Richness") {
      df <- df_Richness()
      
      # Convert data to long format for plotting
      df_long <- reshape2::melt(df)
      
      ggplot(df_long, aes(x = variable, y = value)) +
        geom_bar(stat = "identity", fill = "#69b3a2") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Current Richness Values",
             x = "Variables",
             y = "Value")
    } else {
      df <- df_Abundance()
      
      # Convert data to long format for plotting
      df_long <- reshape2::melt(df)
      
      ggplot(df_long, aes(x = variable, y = value)) +
        geom_bar(stat = "identity", fill = "#69b3a2") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Current Abundance Values",
             x = "Variables",
             y = "Value")
    }
  })
  
  output$text1 <- renderText({
    if(input$select_var == "Richness") {
      df <- df_Richness()
    } else {
      df <- df_Abundance()
    }
    paste("Current dataframe values:\n",
          paste(names(df), df[1,], sep = ": ", collapse = "\n"))
  })
  
  output$prediction <- renderText({
    if(input$select_var == "Richness") {
      df <- df_Richness()
      pred <- predict_richness(df)
      paste("Predicted Richness:", round(pred, 1))
    } else {
      df <- df_Abundance()
      pred <- predict_abundance(df)
      paste("Predicted Abundance:", round(pred, 1))
    }
  })

    # Generate random data for plots
  random_data <- reactive({
    trips <- seq(0, 200, by = 10)
    richness_trips <- 10 + 0.15 * trips + rnorm(length(trips), 0, 2)
    
    methods <- c("net", "visual", "pan")
    richness_methods <- c(15, 12, 10) + rnorm(3, 0, 1)
    
    levels <- c("species", "genus", "family", "morphospecies")
    richness_levels <- c(18, 15, 12, 14) + rnorm(4, 0, 1)
    
    list(
      trips = data.frame(trips = trips, richness = richness_trips),
      methods = data.frame(method = methods, richness = richness_methods),
      levels = data.frame(level = levels, richness = richness_levels)
    )
  })
  
  output$plot_trips <- renderPlot({
    req(input$select_var == "Richness")
    data <- random_data()
    current_pred <- predict_richness(df_Richness())
    current_trips <- input$value_Trips_year
    
    ggplot(data$trips, aes(x = trips, y = richness)) +
      geom_line(color = "#69b3a2", size = 1) +
      geom_point(data = data.frame(x = current_trips, y = current_pred),
                aes(x = x, y = y),
                color = "red", size = 4) +
      geom_segment(data = data.frame(x = current_trips, y = current_pred),
                  aes(x = x, y = y, xend = x, yend = y - 1),
                  color = "red", size = 1, arrow = arrow(length = unit(0.3, "cm"))) +
      annotate("text", x = current_trips, y = current_pred, 
               label = paste("Predicted:", round(current_pred, 1)),
               hjust = -0.1, vjust = -0.5, color = "red") +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      ) +
      labs(x = "Trips/year", y = "Richness")
  })
  
    output$plot_method <- renderPlot({
    req(input$select_var == "Richness")
    data <- random_data()
    current_pred <- predict_richness(df_Richness())
    current_method <- selected_method()
    
    ggplot(data$methods, aes(x = method, y = richness)) +
      geom_col(fill = "#69b3a2") +
      geom_point(data = data.frame(x = current_method, y = current_pred),
                aes(x = x, y = y),
                color = "yellow", size = 4, shape = 24, fill = "yellow") +
      geom_segment(data = data.frame(x = current_method, y = current_pred),
                  aes(x = x, y = y, xend = x, yend = y - 1),
                  color = "yellow", size = 1, arrow = arrow(length = unit(0.3, "cm"))) +
      annotate("text", x = current_method, y = current_pred, 
               label = paste("Predicted:", round(current_pred, 1)),
               hjust = -0.1, vjust = -0.5, color = "yellow") +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "gray20"),
        plot.background = element_rect(fill = "gray20"),
        panel.grid.major = element_line(color = "gray40"),
        text = element_text(color = "white"),
        axis.text.x = element_text(color = "white"),
        axis.text.y = element_text(color = "white")
      ) +
      labs(x = "Collection method", y = "Richness")
  })
  
  output$plot_level <- renderPlot({
    req(input$select_var == "Richness")
    data <- random_data()
    current_pred <- predict_richness(df_Richness())
    current_level <- selected_level()
    
    ggplot(data$levels, aes(x = level, y = richness)) +
      geom_col(fill = "#69b3a2") +
      geom_point(data = data.frame(x = current_level, y = current_pred),
                aes(x = x, y = y),
                color = "yellow", size = 4, shape = 24, fill = "yellow") +
      geom_segment(data = data.frame(x = current_level, y = current_pred),
                  aes(x = x, y = y, xend = x, yend = y - 1),
                  color = "yellow", size = 1, arrow = arrow(length = unit(0.3, "cm"))) +
      annotate("text", x = current_level, y = current_pred, 
               label = paste("Predicted:", round(current_pred, 1)),
               hjust = -0.1, vjust = -0.5, color = "yellow") +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "gray20"),
        plot.background = element_rect(fill = "gray20"),
        panel.grid.major = element_line(color = "gray40"),
        text = element_text(color = "white"),
        axis.text.x = element_text(color = "white"),
        axis.text.y = element_text(color = "white")
      ) +
      labs(x = "Level of identification", y = "Richness")
  })
  
}

shinyApp(ui = ui, server = server)
