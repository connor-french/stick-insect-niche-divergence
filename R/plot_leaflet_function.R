plot_locs_leaflet <- function(data, factor = "species") {
  
  #make color palette
  pal <- colorFactor(palette = "inferno", domain = NULL)
  
  #create popup labels for points
  popup.label <- paste(data$genus, data$species, "<br>",
                       data$id, "<br>",
                       data$locality)
  
  #set the map bounds
  max.lat <- max(data$latitude) %>% ceiling()
  min.lat <- min(data$latitude) %>% floor()
  max.lon <- max(data$longitude) %>% ceiling()
  min.lon <- min(data$longitude) %>% floor()
  
  
  
  #plot in leaflet
  leaflet() %>% 
  addTiles() %>%
  fitBounds(lng1 = min.lon, lng2 = max.lon, lat1 = min.lat, lat2 = max.lat) %>%
  addCircleMarkers(data = data,
                   color = ~pal(data[[factor]]),
                   radius = 3,
                   popup = popup.label) %>%
  addLegend(data = data, "bottomright", pal = pal, values = ~data[[factor]], opacity = .9, title = factor)
}

