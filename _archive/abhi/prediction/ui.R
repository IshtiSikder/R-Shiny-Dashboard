# prediction/ui.R
fluidPage(
  actionButton("home_button", "Back to Home", class = "btn-secondary"),
  br(), br(),
  
  titlePanel("Predict Species Richness"),
  
  sidebarLayout(
    sidebarPanel(
      # Start year input
      numericInput(
        "value_Start_year",
        "Start Year",
        value = 2020,
        min = 2000,
        max = 2024
      ),
      
      # Number of sites
      numericInput(
        "value_No_of_sites",
        "Number of Sites",
        value = 50,
        min = 1,
        max = 200
      ),
      
      # Trips per year
      numericInput(
        "value_Trips_year",
        "Sampling Trips per Year",
        value = 10,
        min = 1,
        max = 50
      ),
      
      # Collection method - simplified to match model
      radioButtons(
        "collection_method",
        "Collection Method",
        choices = c("Standard", "Visual"),
        selected = "Standard"
      ),
      
      # Identification level - simplified to match model
      radioButtons(
        "identification_level",
        "Level of Identification",
        choices = c("Species", "Other"),
        selected = "Species"
      )
    ),
    
    mainPanel(
      plotOutput("richness_plot", height = "400px"),
      br(),
      verbatimTextOutput("prediction")
    )
  )
)