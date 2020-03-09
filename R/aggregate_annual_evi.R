### Aggregate monthly EVI rasters to annual averages
require(raster)
require(tidyverse)

raster_path <- "/Volumes/Linux-Shari/nz_nasa/monthly_rasters"

raster_files <- list.files(raster_path, full.names = TRUE)

raster_df <- tibble(full_files = raster_files, 
                    year = str_extract(raster_files, pattern = "\\d\\d\\d\\d") %>% 
                      as.factor())

yearly_split <- split(raster_df, raster_df$year)

yearly_avg <- purrr::map(yearly_split,
  function(x) {
      r_stack <- raster::stack(x$full_files)
      r_yearly_avg <- raster::calc(r_stack, mean)
      names(r_yearly_avg) <- paste0("evi_annual_", unique(x$year))
      r_yearly_avg
    }
  )

# write to files
output_path <- "/Volumes/Linux-Shari/nz_nasa/yearly_rasters/"
purrr::map(yearly_avg, 
           function(x){
             writeRaster(x, filename = paste0(output_path, names(x), ".asc"), format = "ascii")
           }
)  





