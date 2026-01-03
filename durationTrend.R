create_duration_trend_plot <- function(df) {
  lm_fit <- lm(duration_years ~ start_year, data = df)
  trend_data <- data.frame(
    start_year = seq(min(df$start_year), max(df$start_year), length.out = 100)
  )
  trend_data$duration_years <- predict(lm_fit, trend_data)
  
  plot_ly() %>%
    add_trace(data = df,
              x = ~start_year,
              y = ~duration_years,
              type = "scatter",
              mode = "markers",
              marker = list(color = "#000")) %>%
    add_trace(data = trend_data,
              x = ~start_year,
              y = ~duration_years,
              type = "scatter",
              mode = "lines",
              line = list(color = '#2171b5'))
}