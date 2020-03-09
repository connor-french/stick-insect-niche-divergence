### The purpose of this script is to convert hdf files into merged GeoTiffs;
### one for each month
library(gdalUtils)
library(raster)
library(tidyverse)

hdf_dir <- "/Volumes/Linux-Shari/nz_nasa"

hdf_files <- list.files(hdf_dir, pattern = ".hdf", full.names = TRUE)

# this is the list of files that I used to download from the internet.
# the file path contains info on the month and year the data is from, so I'm 
download_list <- read_table(paste0(hdf_dir, "/download.txt"), 
                            col_names = "file")
download_vector <- download_list$file

# convert hdf files to geotiffs. This writes them to the disk (i.e. the tif won't be assigned to an object)
# the data I'm interested in is the Enhanced Vegetation Index (EVI). 
# This is the 2nd raster in each hdf dataset, so I'm outputting the second slot (sds[2])
purrr::map(download_vector, function(x) {
  prefix <- stringr::str_extract(x, pattern = "MOD13A3\\.A(.*)") %>% 
    stringr::str_remove(".hdf")
  sds <- gdalUtils::get_subdatasets(paste0(hdf_dir, "/", prefix, ".hdf"))
  date <- stringr::str_extract(x, pattern = "\\d\\d\\d\\d\\.\\d\\d\\.\\d\\d") %>% 
    stringr::str_replace_all("\\.", "_")
  name <- paste0(prefix, "_", date, ".tif")
  gdalUtils::gdal_translate(src_dataset = sds[2], dst_dataset = paste0(hdf_dir, "/", name))
}
)

### reproject and merge tifs into monthly rasters
### note: I made a folder for this output called "monthly_rasters"

tif_list <- list.files(hdf_dir, pattern = ".tif")

tif_df <- tibble(
  name = str_split_fixed(tif_list, pattern = "_", n = 2)[,1],
  month = str_split_fixed(tif_list, pattern = "_", n = 2)[,2] 
)

tif_split <- split(tif_df, tif_df$month)

#chelsa raster for reprojection purposes
r_projection <- raster("/Users/connorfrench/Dropbox/Old_Mac/Science/Stick-Insect-Project/climate/CHELSA_bio10_1.tif")

# this loop takes a while
# I'm reprojecting each tile before merging and writing to a new file
purrr::map(tif_split, function(x) {
  r1 <- raster(
    paste0(hdf_dir, "/", str_c(x[1,]$name, x[1,]$month, sep = "_"))
  )
  
  r1_repro <- projectRaster(r1, r_projection)
  
  r2 <- raster(
    paste0(hdf_dir, "/", str_c(x[2,]$name, x[2,]$month, sep = "_"))
  )
  
  r2_repro <- projectRaster(r2, r_projection)
  
  r3 <- raster(
    paste0(hdf_dir, "/", str_c(x[3,]$name, x[3,]$month, sep = "_"))
  )
  
  r3_repro <- projectRaster(r3, r_projection)
  
  r4 <- raster(
    paste0(hdf_dir, "/", str_c(x[4,]$name, x[4,]$month, sep = "_"))
  )
  
  r4_repro <- projectRaster(r4, r_projection)
  
  r5 <- raster(
    paste0(hdf_dir, "/", str_c(x[5,]$name, x[5,]$month, sep = "_"))
  )
  
  r5_repro <- projectRaster(r5, r_projection)
  
  r_combo <- raster::merge(r1_repro, 
                           r2_repro, 
                           r3_repro, 
                           r4_repro,
                           r5_repro)
  
  r_cropped <- crop(r_combo, r_projection)
  
  writeRaster(r_cropped, paste0(hdf_dir, "/monthly_rasters/evi_", x[1,]$month))
  
})

r_t <- raster("/Volumes/Linux-Shari/nz_nasa/monthly_rasters/evi_2000_02_01.tif")
r_t2 <- raster("/Volumes/Linux-Shari/nz_nasa/monthly_rasters/evi_2001_02_01.tif")

