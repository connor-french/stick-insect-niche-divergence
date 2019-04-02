packages <- c("raster", "tidyverse", "ggbiplot", "sf", "RStoolbox", "plotly") #RStoolbox has some dependencies like openMP that can be difficult to compile on a Mac (needed for the dependent package "caret"). If you have High Sierra OS or newer, search for instructions specific to your OS- it's a lot easier than older OS's.
lapply(packages, require, character.only = TRUE)
setwd("/Users/connorfrench/Dropbox/Old_Mac/Science/Stick-Insect-Project")

###read in spreadsheet
loc <- read.csv("worksheets/all-species.csv")
loc <- loc[loc$qual != "b",] #remove poor quality localities

#make locality shape file and assign WGS coord system
coord.points <- st_as_sf(loc, coords = c("longitude", "latitude"), 
                         crs = 4326, agr = "constant")
##get worldclim data
w <- getData("worldclim", var = "bio", res = 0.5, lon = 173, lat = -35)

#check it out
plot(w,1, xlim = c(165, 185), ylim= c(-55, -33))

#extract data from worldclim for each locality.
loc.clim <- data.frame(genus = coord.points$genus,
                       species = coord.points$species,
                       id = coord.points$id,
                       locality = coord.points$locality,
                       coords = as.character(coord.points$geometry),
                       raster::extract(w, coord.points, method = "simple")) %>% na.omit()

#make a matrix of only bioclim values
clim.mat <- loc.clim[,grep("bio", names(loc.clim))] %>% as.matrix()

#run pca on climate variables
clim.pca <- prcomp(clim.mat, scale = TRUE)
summary(clim.pca) #check out the components
knitr::kable(round(clim.pca$rotation[,1:3],3)) #Table of loading scores for the first 3 PCs. 

#add pca results to loc.clim data frame
loc.clim <- data.frame(loc.clim, clim.pca$x)

#make convex hull of genera
find_hull <- function(df) df[chull(df$PC1, df$PC2), ]
hulls <- plyr::ddply(loc.clim, "genus", find_hull)

#plot pca values with convex hull
pt.labels <- paste("Long-Lat:", coord.points$geometry,"<br>",
      coord.points$id, "<br>",
      coord.points$locality)

#make an interactive plot in plotly
p <- ggplot(data = loc.clim, aes(x = PC1, y = PC2, col = genus, fill = genus)) +
  geom_polygon(data = hulls, alpha = 0.4)  + 
  geom_point(alpha = .7, size = 3, aes(text = paste("ID:", id, "<br>",
                                                    genus, species, "<br>",
                                                    "Long-Lat:", coords, "<br>",
                                                    "Locality:", locality))) + 
  scale_color_viridis_d(option = "inferno") + 
  scale_fill_viridis_d(option = "inferno") + 
  labs(title = "PCA of extracted climate", x = "PC1 (41.8%; Temperature)", y = "PC2 (34.6%; Precipitation)")
p.plotly <- ggplotly(p, tooltip = c("text"))
p.plotly

#plot biplot
b <- ggbiplot(clim.pca, groups = loc.clim$genus, alpha = .7) +
  scale_color_viridis_d(option = "inferno") + 
  ggtitle("Biplot of extracted climate")
b


######PCA on all climate data.##########

#subset the world map to just New Zealand to make the analysis tractable
lims<-c(165,185, -55,-33)
submap<-crop(w,lims)

#run a scaled PCA on the climate data
pcamap<-rasterPCA(submap, spca=TRUE)

#Table of loading scores for the first 3 PCs. 
knitr::kable(round(pcamap$model$loadings[,1:3],3)) 

#proportion of variance for each component
summary(pcamap$model)

#plot the PCs
plot(pcamap$map, 1:3)


#extract data from pca for each locality. Making this into a data drame with columns labeled so the row labeling lines up after I remove the NAs.
loc.total.clim <- data.frame(genus = coord.points$genus,
                       species = coord.points$species,
                       id = coord.points$id,
                       locality = coord.points$locality,
                       coords = as.character(coord.points$geometry),
                       raster::extract(pcamap$map, coord.points, method = "simple")) %>% na.omit()

#make convex hull of genera
find.hull.total <- function(df) df[chull(df$PC1, df$PC2), ]
hulls.total <- plyr::ddply(loc.total.clim, "genus", find.hull.total)


#make an interactive plot in plotly
p.total <- ggplot(data = loc.total.clim, aes(x = PC1, y = PC2, col = genus, fill = genus)) +
  geom_polygon(data = hulls.total, alpha = 0.4)  + 
  geom_point(alpha = .7, size = 3, aes(text = paste("ID:", id, "<br>",
                                                    genus, species, "<br>",
                                                    "Long-Lat:", coords, "<br>",
                                                    "Locality:", locality))) + 
  scale_color_viridis_d(option = "inferno") + 
  scale_fill_viridis_d(option = "inferno") + 
  labs(title = "PCA of extracted climate", x = "PC1 (45.4%)", y = "PC2 (31.3%)") +
  theme_minimal()
p.plotly.total <- ggplotly(p.total, tooltip = c("text"))
p.plotly.total



