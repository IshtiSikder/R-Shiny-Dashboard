# UI_exploration.R

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("Explore Levenson Index 2024"),
  
  navbarPage(
    "",
    # Tab 1: Geographic Distribution
    tabPanel("Geographic Distribution",
             div(style = "width: 100%; margin: auto;",
                 plotlyOutput("worldMap", height = "400px", width = "1000px")
             ),
             br(),
             div(style = "width: 80%; margin: auto;",
                 plotlyOutput("usaStateMap", height = "400px")
             )
    ),
    
    # Tab 2: Study Duration Trend
    tabPanel("Study Duration Trend",
             plotlyOutput("durationTrend", height = "400px")
    ),
    
    # Tab 3: Sites Information
    tabPanel("Sites Information",
             fluidRow(
               column(6, plotlyOutput("siteCounts")),
               column(6, plotlyOutput("samplingTrips"))
             )
    ),
    
    # Tab 4: Collection Methods
    tabPanel("Collection Methods",
             plotlyOutput("collectionMethods", height = "400px")
    ),
    
    # Tab 5: Identification Methods
    tabPanel("Identification Methods",
             plotlyOutput("identificationMethods", height = "300px")
    )
  )
)