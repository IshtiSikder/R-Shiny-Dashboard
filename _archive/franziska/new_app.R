library(shiny)
library(ggplot2)
library(ggExtra)
library(shinythemes)
library(bslib)

# Load the prepared data
load("model_results.RData")

# Define base plot function
make_base_plot <- function() {
  p3a <- ggplot(data=sig.corpus, aes(x=Abundance)) +
    geom_ribbon(aes(ymin=Conf2.5, ymax=Conf97.5), fill="#005AB5", alpha=.2) +
    geom_segment(data=lds, aes(x=Abundance, xend=Abundance, y=1, yend=mx),
                 color="grey65", linetype="dotdash", linewidth=1) +
    geom_segment(data=lds, aes(x=10, xend=Abundance, y=mx, yend=mx),
                 color="grey65", linetype="dotdash", linewidth=1) +
    geom_line(aes(y=Median), color="#005AB5", linewidth=1) +
    geom_point(aes(y=Richness, size=size, shape=shape), color="grey75") +
    geom_text(data=lds, aes(x=10, y=mx+1, label=lab), vjust=0, hjust=0, size=5) +
    geom_text(data=lds, aes(x=Abundance, y=1, label=round(Abundance,0)), vjust=1, hjust=0, size=5) +
    scale_x_continuous(breaks=c(10,100,1000,10000,30000), trans="log10") +
    scale_y_continuous("Richness", trans="log10") +
    scale_size_identity() +
    scale_shape_identity() +
    theme_minimal() +
    theme(
      axis.title = element_text(face="bold", size=12),
      axis.text = element_text(size=10),
      plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")
    )
  return(p3a)
}

ui <- fluidPage(
  theme = bs_theme(version = 5),
  titlePanel("The Levenson Index 🐝"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Set Your Study Design"),
      numericInput("num_sites", "Number of Sites", 
                   min = 1, max = 100, value = 10),
      sliderInput("study_duration", "Study Duration (years)",
                  min = 1, max = 10, value = 3),
      sliderInput("sampling_time", "Sampling Time (minutes)",
                  min = 0, max = 1000, value = 100),
      sliderInput("trips_per_year", "Trips per Year",
                  min = 1, max = 52, value = 12),
      radioButtons("Least.Specific.N", "Level of Identification",
                   choices = c("Genus", "Family", "Species", "Morphospecies")),
      radioButtons("sampling_method", "Collection Method",
                   choices = c("Visual", "Net", "Pan")),
      hr(),
      h4("Predictions"),
      verbatimTextOutput("predictions")
    ),
    mainPanel(
      width = 9,
      plotOutput("abundance_richness_plot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  
  predict_abundance <- reactive({
    is_visual <- as.numeric(input$sampling_method == "Visual")
    
    pred <- abundance_model_coefficients["(Intercept)"] +
      abundance_model_coefficients["No..of.Years"] * log(input$study_duration) +
      abundance_model_coefficients["No..of.Sites..Avg."] * log(input$num_sites) +
      abundance_model_coefficients["Sampling.Time.min"] * log(input$sampling_time) +
      abundance_model_coefficients["Visual"] * is_visual +
      abundance_model_coefficients["Sampling.Time.min:Visual"] * log(input$sampling_time) * is_visual
    
    return(exp(pred))
  })
  
  predict_richness <- reactive({
    is_visual <- as.numeric(input$sampling_method == "Visual")
    is_species <- as.numeric(input$Least.Specific.N == "Species")
    
    pred <- richness_model_coefficients["(Intercept)"] +
      richness_model_coefficients["Start.Year"] * 2023 +
      richness_model_coefficients["No..of.Sites..Avg."] * input$num_sites +
      richness_model_coefficients["Sample.Trips.Year..Avg."] * input$trips_per_year +
      richness_model_coefficients["Visual"] * is_visual +
      richness_model_coefficients["Species"] * is_species
    
    return(max(0, pred))
  })
  
  output$abundance_richness_plot <- renderPlot({
    abundance_pred <- predict_abundance()
    richness_pred <- predict_richness()
    
    base_plot <- make_base_plot() +
      geom_point(data = data.frame(x = abundance_pred, y = richness_pred),
                 aes(x = x, y = y),
                 color = "black", size = 4, shape = 15)
    
    ggMarginal(
      base_plot,
      type = "histogram",
      fill = "grey75",
      color = "grey75",
      size = 3,
      margins = "both",
      xparams = list(bins = 30),
      yparams = list(bins = 30)
    )
  })
  
  output$predictions <- renderPrint({
    cat("Predicted Abundance:", round(predict_abundance(), 2),
        "\nPredicted Richness:", round(predict_richness(), 2))
  })
}

shinyApp(ui = ui, server = server)
