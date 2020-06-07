library(ggplot2)
library(plyr)
library(dplyr)
library(plotly)


plot_clim_pca <- function(data, pca, factor = "species"){
  #make convex hull of genera
  find_hull <- function(df) df[chull(df$PC1, df$PC2), ]
  hulls <- plyr::ddply(data, factor, find_hull)
  
  #make an interactive plot in plotly
  p <- ggplot(data = data, aes_string(x = "PC1", y = "PC2", col = factor, fill = factor)) +
    geom_polygon(data = hulls, alpha = 0.4)  + 
    geom_point(alpha = 1, size = 1, aes(text = paste("ID:", id, "<br>",
                                                      genus, species, "<br>",
                                                      "Long-Lat:", latitude, longitude,"<br>",
                                                      "Locality:", locality))) + 
    scale_color_viridis_d(option = "inferno") + 
    scale_fill_viridis_d(option = "inferno") + 
    labs(title = "PCA of extracted climate", x = paste("PC1 (", round(pca$importance[2,1] * 100, 1),"%)"), y = paste("PC2 (", round(pca$importance[2,2] * 100, 1),"%)")) 
    theme_minimal()
  ggplotly(p, tooltip = c("text"))
  
  
}

