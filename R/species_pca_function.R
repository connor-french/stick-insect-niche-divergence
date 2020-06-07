library(dplyr)


species_pca_fun <- function(data, genus, species){
  gen <- genus
  sp <- species
  
  #make a matrix of only bioclim values
  clim_mat <- data %>% 
    dplyr::filter(genus == gen, species == sp) %>% 
    dplyr::select(starts_with("bio")) %>% 
    as.matrix()
  
  #run pca on climate variables
  clim_pca <- prcomp(clim_mat, scale = TRUE)
  summary_pca <- summary(clim_pca) #check out the components
  
  clim_pca_x <- as_tibble(clim_pca$x)
  
  #add pca results to data data frame
  #make a matrix of only bioclim values
  df <- data %>% 
      dplyr::filter(genus == gen, species == sp) %>% 
      dplyr::bind_cols(clim_pca_x)
  
  #list of two objects
  summary_list <- list(summary_pca = summary_pca, 
                        loc_clim = df)

  return(summary_list)
}