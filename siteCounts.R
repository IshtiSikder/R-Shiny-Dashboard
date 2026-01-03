# Function to create site counts histogram
create_site_counts_plot <- function(df) {
  plot_ly(df,
          x = ~log10(site_count),
          type = "histogram",
          marker = list(color = "#9ecae1")) %>%
    layout(
      xaxis = list(title = "Number of Sites (log10 scale)"),
      yaxis = list(title = "Frequency")
    )
}