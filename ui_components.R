# Create landing page UI
create_landing_page <- function() {
  fluidPage(
    br(),
    br(),
    titlePanel(
      div(
        "The Levenson Index", 
        style = "color: #1976D2;"
      )
    ),
    p("Hannah K Levenson, Bradley N Metz, David R Tarpy, Effects of study design parameters on estimates of bee abundance and richness in agroecosystems: a meta-analysis, Annals of the Entomological Society of America, Volume 117, Issue 2, March 2024, Pages 92–106, https://doi.org/10.1093/aesa/saae001"),
    # Replace the tags$a with an actionButton
    actionButton(
      "home_button",
      label = icon("home"),
      style = "position:absolute; top:10px; right:10px; background-color: transparent; 
             color: #1976D2; 
             border: none;"
    ),
    # tags$a(
    # href = "#",
    # icon("home"),
    # style = "position:absolute; top:10px; right:10px;",
    # onclick = "window.location.reload(); return false;"
    # ),
    # Add spacing and horizontal line
    br(),
    p("Pollinators are critical for agricultural production and food security, leading to many ongoing surveys of pollinators (especially bees) in crop and adjacent landscapes. These surveys have become increasingly important to better understand the community of potential pollinators, quantify relative insect abundance, and secure crop ecosystem services. However, as some bee populations are declining, there is a need to align and improve survey efforts, so that they can best meet research and conservation goals, particularly in light of the logistical and financial constraints of conducting such studies. Here, we mined the existing literature on bee surveys in or around agricultural lands to better understand how sampling methods can be optimized to maximize estimates
      of 2 key measures of bee communities (abundance and richness). After reviewing 72 papers spanning 20 yr of publication, we found that study duration, number of sites, sampling time, and sampling method most significantly influenced abundance, while the number of trips per year and collection method significantly influenced richness. Our analysis helps to derive thresholds, priorities, and recommendations that can be applied to future studies describing bee communities in agroecosystems."
      ),
    br(),
    tags$hr(style = "border-top: 1px solid #808080;"),
    br(),
    br(),
  #  p("Use your study design to predict abundance and richness according to the Levenson Index."),
  #  actionButton("predict_button", "Predict Abundance & Richness",
  #               style = "background-color: #1976D2; color: white; font-weight: bold;"),
  #  br(), br(),
  #  p("Explore the data included in the Levenson Index."),
  #  actionButton("explore_button", "Explore the Levenson Index",
  #               style = "background-color: #1976D2; color: white; font-weight: bold;")
  #)
  p("Use your study design to predict abundance and richness according to the Levenson Index."),
  actionButton("predict_button", HTML("➤ Predict Abundance & Richness "),
               style = "background-color: white; 
                     color: #1976D2; 
                     
                     border: 2px solid #1976D2; 
                     border-radius: 25px; 
                     padding: 10px 20px;
                     font-size: 16px;"),
  br(), 
  br(),
  br(),
  p("Explore the data included in the Levenson Index."),
  actionButton("explore_button", HTML("➤ Explore the Levenson Index"),
               style = "background-color: white; 
                     color: #1976D2; 
                     
                     border: 2px solid #1976D2; 
                     border-radius: 25px; 
                     padding: 10px 20px;
                     font-size: 16px;"),
  br(), 
  br(),
  br(),
  br(),
  br(),
  # Add spacer to push acknowledgment to bottom
  div(style = "margin-top: auto; flex-grow: 1;"),
  # Add acknowledgment text
  div(
    style = "text-align: center; color: #808080; padding: 20px; margin-top: 50px;",
    p("We are thankful for support from The Data Science Consulting Program at NC State University, 
        a joint effort between the Libraries' Department of Data & Visualization Services and 
        the Data Science & AI Academy")
    )
  )
}

# Create predict page UI
create_predict_page <- function() {
  fluidPage(
    titlePanel("The Levenson Index 🐝"),
    # Replace the tags$a with an actionButton
    actionButton(
      "home_button",
      label = icon("home"),
      style = "position:absolute; top:10px; right:10px; background-color: transparent; 
             color: #1976D2; 
             border: none;"
    ),
    # tags$a(
    # href = "#",
    # icon("home"),
    # style = "position:absolute; top:10px; right:10px;",
    # onclick = "window.location.reload(); return false;"
    # ),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Set Your Study Design"),
        numericInput("start_year", "Start Year", 
                     min = 1900, max = 2035, value = 2020),
        numericInput("num_sites", "Number of Sites", 
                     min = 1, max = 100, value = 1),
        sliderInput("study_duration", "Study Duration (years)",
                    min = 1, max = 10, value = 1),
        radioButtons("sampling_method", "Collection Method",
                     choices = c("Visual", "Net", "Pan")),
      # # Conditional panels for sampling time based on the collection method
      # conditionalPanel(
      #   condition = "input.sampling_method != 'Pan'",
      #   sliderInput("sampling_time_minutes", "Sampling Time (minutes)",
      #               min = 10, max = 120, value = 10)),
      # conditionalPanel(
      #   condition = "input.sampling_method == 'Pan'",
      #   sliderInput("sampling_time_days", "Sampling Time (days)",
      #               min = 0.17, max = 7, value = 0.17, step = 0.01)),
       sliderInput("sampling_time", "Sampling Time (minutes)",
                   min = 1, max = 120, value = 50),
        sliderInput("trips_per_year", "Trips per Year",
                    min = 1, max = 22, value = 1),
        radioButtons("Least.Specific.N", "Level of Identification",
                     choices = c("Genus", "Family", "Species", "Morphospecies")),
        
        hr(),
        h4("Predictions"),
        verbatimTextOutput("predictions")
      ),
      #mainPanel(
      #  width = 9,
      #  plotOutput("abundance_richness_plot", height = "600px")
      #)
    #)
  #),

  mainPanel(
      width = 9,
      fluidRow(
        column(8,
               plotOutput("abundance_richness_plot", height = "600px")
        ),
        column(4,
               div(
                 style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 20px;",
                 h4("Study Predictions", style = "color: #005AB5;"),
                 uiOutput("prediction_text"),
                 hr(),
                 p("Note about predictions:", style = "font-weight: bold;"),
                 p("These predictions are based on patterns from historical studies, but bee communities can be highly variable. Our model captures some of this variation (R² = 0.296), but local conditions and other factors may lead to different outcomes.")
               )
        )
      )
    )
  )
)
}

# Create explore page UI
create_explore_page <- function() {
  fluidPage(
    titlePanel("Explore The Levenson Index"),
    # Replace the tags$a with an actionButton
    actionButton(
      "home_button",
      label = icon("home"),
      style = "position:absolute; top:10px; right:10px; background-color: transparent; 
             color: #1976D2; 
             border: none;"
    ),
    # tags$a(
    # href = "#",
    # icon("home"),
    # style = "position:absolute; top:10px; right:10px;",
    # onclick = "window.location.reload(); return false;"
    # ),
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
                   plotlyOutput("identificationMethods", height = "500px")
               )
      )
    )
  )
}

current_view <- reactiveVal("landing")

# Main container function that handles view switching
create_main_container <- function(current_view) {
  if (current_view == "landing") {
    create_landing_page()
  } else if (current_view == "predict") {
    create_predict_page()
  } else if (current_view == "explore") {
    create_explore_page()
  }
}
