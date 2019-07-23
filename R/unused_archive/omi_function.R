#function to perform an OMI given coordinates,climate data, and species. Need to specify buffer size in degrees and 
#the number of background env't points to subsample- defaults are 0.5 degree buffer and 1000 bg points.
#data needs to have at least three columns- species, longitude, latitude
#clim_str is the string that indicates the environmental variables being used. It needs to be present in every env'tal variable name and not be present in species names. 
#It defaults to "bio" for bioclim worldclim variables extracted using the getData function from the raster package
#Outputs the  data used for the omi test (data_omi), the pca of total e-space (pca_omi), omi calculation (omi), and statistical test for significanct (test_omi)
#use_bg indicates whether you want to include bavkground sampling (new way, TRUE) or no background sampling (trad way, FALSE)
#numrep indicates the number of replicates to perform for the omi significance test. default is 1000 like that used in Doledec 2000

#packages needed- tidyverse, sp, rgeos, raster, ade4
omi_fun <- function(data, clim, use_bg = FALSE, clim_str = "bio", buffer = 0.5, bg_n = 1000, numrep = 1000){
  
  #filter data set to only include species, longitude, and latitude
  occs <- data %>% 
    select(species, longitude, latitude)
  
  #count the number of individuals per species (just to help interpret results)
  sp_count <- occs %>% 
    dplyr::group_by(species) %>% 
    count()
  
  #convert to spatialpointsdataframe for downstream processing
  sp::coordinates(occs) <- ~ longitude + latitude
  
  #crop raster to background extent (this is also helpful if not using background bc of my inefficient way of extracting env't)
  
  #get background extent
  bg_ext <- min_conv_poly(occs) %>% 
    rgeos::gBuffer(width = buffer)
  
  # crop the environmental rasters by the background extent shape
  env_crop <- suppressWarnings(raster::crop(clim, bg_ext))
  
  print("Cropped environmental data")
    
  
  
  #extract species localities
  env_crop_species <- env_crop %>% 
    raster::extract(occs, method = "simple", df = TRUE, cellnumbers = TRUE) %>% 
    bind_cols(as_tibble(occs)) %>% 
    select(cells, species, -ID)
  
  print("Extracted environmental data from species localities")
  
  #convert cropped env to data frame and retain coordinates
  env_crop_coords <- env_crop %>% 
    as.data.frame(xy = TRUE) %>% 
    na.omit() %>% 
    mutate(longitude = x, latitude = y) %>% 
    select(longitude, latitude)
  
  #use raster::extract to obtain cell numbers of the raster values. Seems redundant, but I can't figure
  #out a different way to do this
  env_crop_df <- env_crop %>% 
    raster::extract(env_crop_coords, method = "simple", df = TRUE, cellnumbers = TRUE) %>% 
    select(-ID)
  
  print("Obtained cell numbers")
  
  #make the background env't a species (or don't)
  if(use_bg){ 
    #make the background environment a "species" for OMI analysis. 
    env_crop_total <- dplyr::full_join(env_crop_df, env_crop_species, by = "cells") %>%
      tibble::rowid_to_column() %>% 
      mutate(species = replace_na(species, "env"))
    #subsample the background env't.
    env_crop_env <- env_crop_total %>% 
      dplyr::filter(species == "env") %>% 
      sample_n(bg_n)
    #subset env't to only that occupied by the species. Need to do this to put bind with the subsampled background env't
    env_crop_sp_only <- env_crop_total %>% 
      dplyr::filter(species != "env")
    #bind the background env't with the species env't and spread. This makes binary presence/absence points for each species across the env'tal space
    env_crop_all <- bind_rows(env_crop_env, env_crop_sp_only) %>% 
      mutate(value = 1) %>% 
      spread(species, value, sep = "_", fill = 0)
    
    print("Successfully completed species table")
        }
  else{
    #don't make the background env't a "species" (the traditional version of the method)
    env_crop_all <- dplyr::full_join(env_crop_df, env_crop_species, by = "cells") %>%
      drop_na(species) %>% 
      tibble::rowid_to_column() %>% 
      mutate(value = 1) %>% 
      spread(species, value, sep = "_", fill = 0)
    
    print("Successfully completed species table")
          }
  
  #get list of species presence/absence and env'tal variables for omi analysis
  omi_data <- list(species = env_crop_all %>% 
                     select(contains("species")), 
                   env = env_crop_all %>% 
                     select(contains(clim_str)))
  
  
  #pca of env'tal variables. Retaining the first three PC axes
  dudi_omi <- dudi.pca(omi_data$env, scale = TRUE, scannf = FALSE, nf = 3)

  print("PCA complete")
  
  #conduct omi analysis
  omi_out <- niche(dudi_omi, omi_data$species, scann = FALSE)
  
  print("OMI complete")
  
  #conduct statistical significance test
  omi_test <- rtest(omi_out, numrep)
  
  print("OMI test complete")
  
  results <- list(data_omi = omi_data, pca_omi = dudi_omi, omi = omi_out, test_omi = omi_test, sp_count = sp_count)
  
  return(results)
  
}
