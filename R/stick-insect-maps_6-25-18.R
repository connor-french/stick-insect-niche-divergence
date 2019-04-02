packages <- c("tmap", "tmaptools", "RColorBrewer", "sf", "ggmap") #load in necessary map packages
lapply(packages, require, character.only = TRUE)
setwd("/Users/Connor/Dropbox/Old_Mac/Science/Stick-Insect-Project") #set working directory for project

###read in spreadsheet
loc <- read.csv("worksheets/all-species.csv")
loc <- loc[loc$qual != "b",] #remove poor quality localities

#make locality shape file and assign WGS coord system
coord_points <- st_as_sf(loc, coords = c("longitude", "latitude"), 
                         crs = 4326, agr = "constant")

#make a bounding box for the map so that the little NZ islands that go into negative longitudes don't make the map wrap around
sbbox <- make_bbox(lon = loc$longitude, lat = loc$latitude, f = .1)

#read in world boundaries shape file
world <- read_shape("gis_files/TM_WORLD_BORDERS_SIMPL-0.3.shp")

#map with all individuals, colored by genus
all_map <- tm_shape(world, bbox = sbbox) + 
  tm_polygons() + 
  tm_shape(coord_points) + 
  tm_dots(col = "genus", size = .2) +
  tm_layout(legend.position = c("LEFT", "TOP"))

####maps for each genus, colored by species. Genera include: Acanthoxyla, Argosarchus, Clitarchus, Niveaphasma, and Tectarchus####
#acanthoxyla
acan_points <- subset(coord_points, genus == "acanthoxyla")
acan_points$species <- droplevels(acan_points$species)
acan_map <- tm_shape(world, bbox = sbbox) + 
  tm_polygons() + 
  tm_shape(acan_points) + 
  tm_dots(col = "species", size = .2) +
  tm_layout(legend.position = c("LEFT", "TOP"),  title = acan_points$genus)

#argosarchus
argo_points <- subset(coord_points, genus == "argosarchus")
argo_points$species <- droplevels(argo_points$species)
argo_map <- tm_shape(world, bbox = sbbox) + 
  tm_polygons() + 
  tm_shape(argo_points) + 
  tm_dots(col = "species", size = .2) +
  tm_layout(legend.position = c("LEFT", "TOP"),  title = argo_points$genus)

#clitarchus
clit_points <- subset(coord_points, genus == "clitarchus")
clit_points$species <- droplevels(clit_points$species)
clit_map <- tm_shape(world, bbox = sbbox) + 
  tm_polygons() + 
  tm_shape(clit_points) + 
  tm_dots(col = "species", size = .2) +
  tm_layout(legend.position = c("LEFT", "TOP"),  title = clit_points$genus)

#niveaphasma
nive_points <- subset(coord_points, genus == "niveaphasma")
nive_points$species <- droplevels(nive_points$species)
nive_map <- tm_shape(world, bbox = sbbox) + 
  tm_polygons() + 
  tm_shape(nive_points) + 
  tm_dots(col = "species", size = .2) +
  tm_layout(legend.position = c("LEFT", "TOP"),  title = nive_points$genus)


