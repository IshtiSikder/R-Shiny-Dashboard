# Function to create sampling trips histogram
create_sampling_trips_plot <- function(df) {
  plot_ly(df,
          x = ~sample_trips_per_year_avrg,
          type = "histogram",
          marker = list(color = "#4292c6")) %>%
    layout(
      xaxis = list(title = "Trips per Year"),
      yaxis = list(title = "Frequency")
    )
}