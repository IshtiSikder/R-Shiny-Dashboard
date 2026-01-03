create_world_map <- function(df) {
  country_counts <- df %>%
    group_by(country) %>%
    summarise(count = n()) %>%
    ungroup()
  
  plot_ly(country_counts,
          type = 'choropleth',
          locationmode = 'country names',
          locations = ~country,
          z = ~count,
          text = ~paste(country, ": ", count, " studies"),
          colorscale = list(
            c(0, "#deebf7"),
            c(0.2, "#2b4a69ff"),
            c(0.4, "#2b4a69ff"),
            c(0.6, "#2b4a69ff"),
            c(0.8, "#12283cff"),
            c(1, "#12283cff")
          )) %>%
    layout(
      annotations = list(
        list(
          text = "Number of Studies by Country",
          x = 0.5,
          y = 1.1,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 20),
          bgcolor = "white",
          bordercolor = "white",
          borderpad = 4
        )
      ),
      geo = list(showframe = TRUE,
                 showcoastlines = TRUE,
                 projection = list(type = 'miller'))
    )
}