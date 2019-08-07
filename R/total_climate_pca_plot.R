require(ggplot2)
require(dplyr)
require(ggnewscale)

total_climate_pca_plot <- function(bg_env, locs, genus, species) {
  env_df <- raster::sampleRandom(bg_env, size = 10000) %>% 
    as_tibble() %>% 
    mutate(reproductive_mode = "none")
  
  sp_df <- raster::extract(bg_env, 
                           locs %>% filter(species == species) %>% 
                             dplyr::select(longitude, latitude)) %>% 
    as_tibble() %>% 
    mutate(reproductive_mode = locs$reproductive_mode)
  
  total_df <- dplyr::bind_rows(env_df, sp_df)
  
  pca_df <- total_df %>% dplyr::select(-reproductive_mode)
  
  env_pca <- prcomp(pca_df, center = TRUE, scale. = TRUE)
  
  pca_out_df <- env_pca$x %>% as_tibble() %>% 
    mutate(reproductive_mode = total_df$reproductive_mode)
  
  ggplot() +
    geom_hex(data = pca_out_df %>% filter(reproductive_mode == "none"), aes(x = PC1, y = PC2, fill = ..count..)) +
    scale_fill_gradient(low = "lightgray", high = "black") +
    labs(fill = "Count") +
    ggnewscale::new_scale_fill() +
    geom_point(data = pca_out_df %>% filter(reproductive_mode != "none"), aes(x = PC1, y = PC2, fill = reproductive_mode, color = reproductive_mode), alpha = 0.8, size = 3, pch = 21) +
    scale_fill_viridis_d(option = "inferno") +
    scale_color_manual(values = c("white", "black"), guide = FALSE) +
    labs(title = paste("Total climate space PCA for", genus, species),
         fill = "Reproductive Mode") +
    theme_minimal()
  
}

