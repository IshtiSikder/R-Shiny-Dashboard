# Function to create collection methods visualization
create_collection_methods_plot <- function(df) {
  net_data <- data.frame(
    method = "Net",
    time = as.numeric(df$net_time_minute),
    type = "minutes"
  ) %>% filter(!is.na(time))
  
  days_data <- data.frame(
    method = c(rep("Pan", nrow(df)), rep("Visual", nrow(df))),
    time = as.numeric(c(df$pan_time_days, df$visual_time_days)),
    type = "days"
  ) %>% filter(!is.na(time))
  
  subplot(
    plot_ly(net_data,
            x = ~method,
            y = ~time,
            type = "box",
            name = "Net (minutes)",
            marker = list(color = "#c6dbef"),
            boxpoints = "all",
            jitter = 0.3,
            pointpos = 0) %>%
      layout(yaxis = list(title = "Time (minutes)")),
    
    plot_ly(days_data,
            x = ~method,
            y = ~time,
            type = "box",
            color = ~method,
            colors = c("#6baed6", "#2171b5"),
            boxpoints = "all",
            jitter = 0.3,
            pointpos = 0) %>%
      layout(yaxis = list(title = "Time (days)")),
    
    nrows = 1,
    widths = c(0.4, 0.6)
  )
}