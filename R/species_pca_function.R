species_pca_fun <- function(data, genus){
  #make a matrix of only bioclim values
  clim.mat <- data[data$genus == genus, grep("bio", names(data))] %>% as.matrix()
  
  #run pca on climate variables
  clim.pca <- prcomp(clim.mat, scale = TRUE)
  summary.pca <- summary(clim.pca) #check out the components
  
  #add pca results to data data frame
  loc.clim <- data.frame(data[data$genus == genus,], clim.pca$x)
  
  #list of two objects
  summary.list <- list(summary.pca = summary.pca, 
                        loc.clim = loc.clim)

  return(summary.list)
}