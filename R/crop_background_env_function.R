bg_env_crop <- function(locs, species, environment, buffer = 0.5) {
  
  locs_filtered <- locs %>% 
    filter(species == species) %>% 
    dplyr::select(longitude, latitude)
  
  sp::coordinates(locs_filtered) <- ~longitude + latitude
  
  bg_poly <- min_conv_poly(locs_filtered) %>% 
    rgeos::gBuffer(width = buffer)
  
  bg_env <- raster::crop(environment, bg_poly)
  
  bg_env
}