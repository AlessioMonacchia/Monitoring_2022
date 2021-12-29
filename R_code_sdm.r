# R code fpr species distribution modelling, namely the distibution of individuals in space
install.packages("sdm") # we are going to use data inside the package(no need to set working directory)
library(sdm)
library(raster)
library(rgdal) # OSGeo is open space library, contributing to GDAL library
# system.file function is showing all the files in a certain package, in this case we use it for the sdm package
# in computer there is a "external" folder which was created downloading the sdm package
# the file we are looking for is the one with species presence and absence inside the external folder
file <- system.file("external/species.shp", package = "sdm")
file # like the list function, will tell the path where the file is stored
# .shp data, shape file data, shapefile function to import such data into R
# it is not a raster file, it is made of point (at certain coordinates) in space
species <- shapefile(file)
plot(species, pch = 19, col = "red")
