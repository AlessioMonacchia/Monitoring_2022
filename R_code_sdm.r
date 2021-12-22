install.packages("sdm")
library(sdm)
library(raster)
library(rgdal)
file <- system.file("external/species.shp", package = "sdm")
file
species <- shapefile(file)
plot(species, pch = 19, col = "red")
