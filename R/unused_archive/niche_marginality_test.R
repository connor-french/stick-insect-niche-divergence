library(raster)
library(dplyr)
library(rgeos)
library(adehabitatHS)

niche_test_fun <- function(data, clim, species_str = NULL, clim_str = "bio", buffer = 0.5, numrep = 1000){
  
  #filter data set to only include species, longitude, and latitude
  occs <- data %>% 
    dplyr::select(species, longitude, latitude) 
  
  #convert to spatialpointsdataframe for downstream processing
  sp::coordinates(occs) <- ~ longitude + latitude
  
  #set projection type of occs to wgs84
  crs(occs) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  #crop raster to background extent (this is also helpful if not using background bc of my inefficient way of extracting env't)
  
  #get background extent
  bg_ext <- min_conv_poly(occs) %>% 
    rgeos::gBuffer(width = buffer)
  
  # crop the environmental rasters by the background extent shape
  env_crop <- suppressWarnings(raster::crop(clim, bg_ext))
  
  print("Cropped environmental data")
  
  #convert to spatialpixelsdataframe for niche marginality test and extract data
  env_pix <- as(env_crop, "SpatialPixelsDataFrame") 

  #filter the occs for only the species we're interested in
  occs <- occs[which(occs$species == species_str),]
  
  #run a niche test for marginality and tolerance
  env_niche_test <- niche.test(env_pix, pts = occs, nrep = numrep)
  
  #make easy to understand data frames of niche test results
  simulation_df <- tibble(sim_marginality = env_niche_test$dfxy$V1, sim_tolerance = env_niche_test$dfxy$V2)
  observed_df <- tibble(obs_marginality = env_niche_test$obs[1], obs_tolerance = env_niche_test$obs[2])
  
  list(simulation_df = simulation_df, observed_df = observed_df, climate = env_crop, occs = occs)
  
}
  
  
