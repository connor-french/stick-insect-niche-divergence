species_pca_fun <- function(data, genus){
  #make a matrix of only bioclim values
  clim_mat <- data[data$genus == genus, grep("bio", names(data))] %>% as.matrix()
  
  #run pca on climate variables
  clim_pca <- prcomp(clim_mat, scale = TRUE)
  summary_pca <- summary(clim_pca) #check out the components
  
  #add pca results to data data frame
  loc_clim <- data.frame(data[data$genus == genus,], clim_pca$x)
  
  #list of two objects
  summary_list <- list(summary_pca = summary_pca, 
                        loc_clim = loc_clim)

  return(summary_list)
}