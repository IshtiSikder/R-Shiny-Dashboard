# Function to create identification methods pie chart
create_identification_methods_plot <- function(df) {
  id_counts <- df %>%
    group_by(identification_least) %>%
    summarise(count = n()) %>%
    filter(!is.na(identification_least))
  
  plot_ly(id_counts,
          labels = ~identification_least,
          values = ~count,
          type = "pie",
          textinfo = "label+percent",
          marker = list(
            colors = c("#9ecae1", "#50789cff", "#2b4a69ff", "#12283cff", "#02070bff")
          ))
}