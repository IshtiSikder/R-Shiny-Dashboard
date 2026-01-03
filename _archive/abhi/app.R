# app.R
library(shiny)
library(shinythemes)
library(shinyjs)
library(ggplot2)
library(bslib)

ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("cosmo"),
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      .action-button {
        margin: 10px 0;
        padding: 10px 20px;
        font-size: 16px;
        width: 400px;
      }
      .content-container {
        max-width: 800px;
        margin: 0 auto;
        padding: 20px;
        text-align: center;
      }
    "))
  ),
  
  div(
    id = "main-content",
    
    # Landing page
    div(
      id = "landing-page",
      class = "content-container",
      titlePanel("Levenson Index"),
      
      actionButton(
        "predict_button",
        "Predict Abundance/Richness with Your Study Design",
        class = "btn-primary action-button"
      ),
      
      br(), br(),
      
      actionButton(
        "explore_button",
        "Explore the Levenson Index",
        class = "btn-primary action-button"
      )
    ),
    
    # Prediction page
    div(
      id = "prediction-page",
      style = "display: none;",
      uiOutput("prediction_ui")
    ),
    
    # Exploration page
    div(
      id = "exploration-page",
      style = "display: none;",
      uiOutput("exploration_ui")
    )
  )
)

server <- function(input, output, session) {
  # Function to show/hide pages
  showPage <- function(page_id) {
    hide("landing-page")
    hide("prediction-page")
    hide("exploration-page")
    show(page_id)
  }
  
  # Load prediction UI and server when needed
  output$prediction_ui <- renderUI({
    source("prediction/ui.R", local = TRUE)$value
  })
  
  # Load exploration UI and server when needed
  output$exploration_ui <- renderUI({
    source("exploration/ui.R", local = TRUE)$value
  })
  
  # Button handlers
  observeEvent(input$predict_button, {
    showPage("prediction-page")
    # Create new environment for prediction server
    pred_env <- new.env()
    # Source prediction server in the new environment
    source("prediction/server.R", local = pred_env)
    # Call the server function with the current session
    pred_env$server(input, output, session)
  })
  
  observeEvent(input$explore_button, {
    showPage("exploration-page")
    # Create new environment for exploration server
    explore_env <- new.env()
    # Source exploration server in the new environment
    source("exploration/server.R", local = explore_env)
    # Call the server function with the current session
    explore_env$server(input, output, session)
  })
  
  observeEvent(input$home_button, {
    showPage("landing-page")
  })
}

shinyApp(ui = ui, server = server)