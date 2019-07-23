###testing the moving window aggregation method
library(raster)
library(grainchanger)

nz_temp <- raster("https://github.com/connor-french/stick-insect-niche-divergence/blob/master/CHELSA_bio10_1.tif?raw=true")

nz_large <- aggregate(nz_temp, fact = 12) #Aggregate to make a "response" raster. Just for demonstration purposes.

#Error in .local(x, row, nrows, ...) : validRow(x, row) is not TRUE
nz_agg <- grainchanger::winmove_agg(g = nz_large, 
                                    dat = nz_temp,
                                    d = 5,
                                    type = "rectangle",
                                    fun = "mean")
plot(nz_agg)
