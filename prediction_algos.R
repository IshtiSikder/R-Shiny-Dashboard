# Algorithm functions for predictions

predict_abundance <- function(input_data) {
  req(input_data$start_year,input_data$num_sites,input_data$study_duration,
      input_data$sampling_method, input_data$sampling_time, 
      input_data$trips_per_year, input_data$Least.Specific.N
      )
  
  is_visual <- as.numeric(input_data$sampling_method == "Visual")
  is_net <- as.numeric(input_data$sampling_method == "Net")
  is_pan <- as.numeric(input_data$sampling_method == "Pan")
  
  is_genus <- as.numeric(input_data$Least.Specific.N == "Genus")
  is_family <- as.numeric(input_data$Least.Specific.N == "Family")
  is_species <- as.numeric(input_data$Least.Specific.N == "Species")
  is_morphospecies <- as.numeric(input_data$Least.Specific.N == "Morphospecies")
  
  
  pred <- Abundance_coeffs["(Intercept)","Estimate"] +
    Abundance_coeffs["Start.Year","Estimate"] * input_data$start_year+
    Abundance_coeffs["No..of.Sites..Avg.","Estimate"] * input_data$num_sites+
    Abundance_coeffs["No..of.Years","Estimate"] * input_data$study_duration+
    Abundance_coeffs["Net","Estimate"] * is_net+
    Abundance_coeffs["Visual","Estimate"] * is_visual+
    Abundance_coeffs["Pan","Estimate"] * is_pan
    Abundance_coeffs["Sampling.Time.min","Estimate"] * input_data$sampling_time+
    Abundance_coeffs["Species","Estimate"] * is_species+
    Abundance_coeffs["Genus","Estimate"] * is_genus+
    Abundance_coeffs["Morphospecies","Estimate"] * is_morphospecies+
    0 * is_family +
    Abundance_coeffs["Sample.Trips.Year..Avg.","Estimate"] * input_data$trips_per_year +
    
  return(pred)
}

predict_richness <- function(x) {
  # Input validation
  if (!is.numeric(x)) {
    stop("x must be a numeric value")
  }
  
  # Calculate using the formula
  pred <- 75 * (1 - exp(-0.0002 * x))
  
  
  return(pred)
}
