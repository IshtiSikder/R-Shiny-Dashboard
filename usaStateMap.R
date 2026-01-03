# Function to create USA state map visualization
create_usa_state_map <- function(df) {
  state_counts <- df %>%
    filter(!is.na(state_usa)) %>%
    group_by(state_usa) %>%
    summarise(count = n()) %>%
    ungroup()
  
  plot_ly(state_counts, 
          type = "choropleth",
          locationmode = "USA-states",
          locations = ~state_usa,
          z = ~count,
          text = ~paste(state_usa, ": ", count, " studies"),
          colorscale = list(
            c(0, "#95b4cfff"),
            c(0.2, "#2b4a69ff"),
            c(0.4, "#2b4a69ff"),
            c(0.6, "#2b4a69ff"),
            c(0.8, "#12283cff"),
            c(1, "#12283cff")
          )) %>%
    layout(
      annotations = list(
        list(
          text = "Studies per US State",
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
      geo = list(scope = 'usa')
    )
}
