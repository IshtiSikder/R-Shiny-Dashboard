# Define base plot function (for abundance vs richness) 
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


create_prediction_plot <- function(abundance_pred, richness_pred) {
  base_plot <- make_base_plot() +
    geom_hline(yintercept = richness_pred, color = "firebrick", 
               linetype = "dashed", alpha = 0.5) +
    geom_vline(xintercept = abundance_pred, color = "firebrick", 
               linetype = "dashed", alpha = 0.5) +
    geom_point(data = data.frame(x = abundance_pred, y = richness_pred),
               aes(x = x, y = y),
               color = "firebrick", size = 10, shape = 15, alpha = 0.3) +
    geom_point(data = data.frame(x = abundance_pred, y = richness_pred),
               aes(x = x, y = y),
               color = "firebrick", size = 3, shape = 15)
  
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
}