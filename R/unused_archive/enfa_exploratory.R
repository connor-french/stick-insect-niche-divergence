library(raster)
library(sf)
library(adehabitatHS)
library(tidyverse)
library(readxl)
library(ggspatial)
source("/Users/connorfrench/Dropbox/Old_Mac/Science/Stick-Insect-Project/R/min_convex_poly.R")
source("/Users/connorfrench/Dropbox/Old_Mac/Science/Stick-Insect-Project/R/presence_absence_raster_function.R")

#read in localities and filter for a single species
locs <- read_xlsx("/Users/connorfrench/Dropbox/Old_Mac/Science/Stick-Insect-Project/Original-spreadsheets/all species New_6-14-19.xlsx") %>% 
  janitor::clean_names() %>% 
  filter(genus == "niveaphasma", species == "annulata", reproductive_mode == "sexual") 

#convert to a SPDF
coordinates(locs) <- ~longitude + latitude

#assign the CRS as wgs84
crs(locs) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

###plot the localities ----
#get a new zealand base map
nz_map <- rnaturalearth::ne_countries(country = "new zealand", returnclass = "sf")

ggplot() +
  geom_sf(data = nz_map) +
  geom_point(data = as.data.frame(locs, xy = TRUE), aes(x = longitude, y = latitude))


#read in the chelsa climate data
nz_env <- "/Users/connorfrench/Dropbox/Old_Mac/climate-data/chelsa_30sec_NewZealand/chelsa_bioclims_NZ.tif"
env <- stack(nz_env)

#make a polygon for the background environment
background_poly <- min_conv_poly(locs) %>% 
  rgeos::gBuffer(width = 0.5)

#crop environment by the polygon
background_env <- crop(env, background_poly) 
crs(background_env) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#convert background poly to sf for plotting
background_poly_sf <- st_as_sf(background_poly)
st_crs(background_poly_sf) <- 4326

#plot background env't just to make sure everything looks right
ggplot() +
  ggspatial::layer_spatial(background_env[[2]]) +
  ggspatial::geom_spatial_point(data = as.data.frame(locs, xy = TRUE), aes(x = longitude, y = latitude)) +
  geom_sf(data = background_poly_sf, fill = NA) 

#extract env't for localities
env_locs <- raster::extract(env, locs, na.rm = FALSE) %>% 
  as_tibble() %>% 
  bind_cols(as.data.frame(locs, xy = TRUE))

#check localities for NA
env_na <- env_locs %>% 
  filter(is.na(chelsa_bioclims_NZ.5)) %>% 
  select(longitude, latitude)

#plot na localities
ggplot() +
  ggspatial::layer_spatial(background_env[[2]]) +
  ggspatial::geom_spatial_point(data = env_na, aes(x = longitude, y = latitude)) +
  geom_sf(data = background_poly_sf, fill = NA) 


###get presence-absence raster and convert to vector ----
#assign mask raster
mask_raster <- background_env[[1]]

#reduce locs data frame to just coordinates
locs_coords <- locs %>% as.data.frame(xy = TRUE) %>% select(longitude, latitude)

#generate presence-absence raster
presence_absence <- presence_absence_raster(mask_raster, locs_coords)

#convert to vector of 0s and 1s
locs_pr <- na.omit(values(presence_absence)) %>% as.vector()

###conduct environmental pca ----
env_df <- as.data.frame(background_env) %>% na.omit()

env_pca <- dudi.pca(env_df, scannf = FALSE, nf = 6)

###perform enfa analysis ----
enfa_obj <- adehabitatHS::enfa(env_pca, locs_pr, scannf = FALSE, nf = 6)

hist(enfa_obj)
scatter(enfa_obj, clabel = 0, Acol = "grey60", Ucol = "red")




