library(shiny)
library(shinythemes)
library(fontawesome)

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  
  # Container for all content
  uiOutput("main_container")
)

server <- function(input, output, session) {
  # Reactive value to track current view
  current_view <- reactiveVal("landing")
  
  # Main container logic
  output$main_container <- renderUI({
    if (current_view() == "landing") {
      fluidPage(
        titlePanel("Levenson Index"),
        p("Authors, APA 7th Reference, doi/link to publication"),
        
        # Top right home button
        tags$a(
          href = "#",
          icon("home"),
          style = "position:absolute; top:10px; right:10px;",
          onclick = "window.location.reload(); return false;"
        ),
        
        # Landing page content
        p("Description. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."),
        actionButton("predict_button", "Predict Abundance/Richness with Your Study Design",
                    style = "background-color: #1976D2; color: white; font-weight: bold;"),
        br(), br(),
        p("Description. Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."),
        actionButton("explore_button", "Explore the Levenson Index",
                    style = "background-color: #1976D2; color: white; font-weight: bold;")
      )
    } else if (current_view() == "predict") {
      fluidPage(
        titlePanel("Predict Abundance/Richness"),
        tabsetPanel(
          tabPanel("Tab 1", p("Content for Tab 1")),
          tabPanel("Tab 2", p("Content for Tab 2")),
          tabPanel("Tab 3", p("Content for Tab 3"))
        )
      )
    } else if (current_view() == "explore") {
      fluidPage(
        titlePanel("Explore Levenson Index"),
        tabsetPanel(
          tabPanel("Tab 1", p("Content for Tab 1")),
          tabPanel("Tab 2", p("Content for Tab 2")),
          tabPanel("Tab 3", p("Content for Tab 3")),
          tabPanel("Tab 4", p("Content for Tab 4")),
          tabPanel("Tab 5", p("Content for Tab 5"))
        )
      )
    }
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
