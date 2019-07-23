enfa_calc_fun <- function(locs, species, reproductive_mode, mask_raster) {
  
  source("R/presence_absence_raster_function.R")
  
  #subset localities for species and reproductive mode
  locs_filtered <- locs %>%
    filter(species == {{species}},
           reproductive_mode == {{reproductive_mode}})
  
  
  #subset localities df for the coordinates
  locs_coords <- locs_filtered %>% dplyr::select(longitude, latitude)
  
  #generate presence-absence raster
  presence_absence <- presence_absence_raster(mask_raster[[1]],
                                              locs_coords)
  #convert to vector of 0s and 1s
  locs_pr <- na.omit(values(presence_absence)) %>% as.vector()
  
  
  ###conduct environmental pca ----
  env_df <- as.data.frame(mask_raster) %>% na.omit()
  env_pca <- dudi.pca(env_df, scannf = FALSE, nf = 6)
  
  #for some reason locs_pr may have one or two cells more than the env_df, which confuses the enfa function.
  #in this case, I'm removing enough background cells ("0" cells) to make the locs_pr vector the same length as the env_df
  #I'm too lazy to figure out how to only subset "0" cells and I don't think the last few cells will contain a presence, so I'm just going to shoot a warning if that edge case comes up for me
  if ( length(locs_pr) > length(env_df[,1]) ) {
    l_dif <- length(locs_pr) - length(env_df[,1])
    l_vec <- locs_pr[(length(locs_pr)-l_dif):length(locs_pr)]
    if ( length(l_vec[l_vec > 0] > 0) ){
      print("The ends contain presence localities. Remove by hand.") 
    }
    
    else {
        locs_pr <- locs_pr[1:(length(locs_pr) - l_dif)] 
      }
    
  }
  
  
  ###perform enfa analysis ----
  enfa_obj <- adehabitatHS::enfa(env_pca, locs_pr, scannf = FALSE, nf = 6)
  
  #enfa_obj
  enfa_obj
  #list(env_pca = env_pca, locs_pr = locs_pr, locs_filtered = locs_filtered, pr_raster = presence_absence, env_df = env_df)
}