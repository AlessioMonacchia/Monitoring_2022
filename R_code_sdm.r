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

# 10/1
library(sdm)
library(raster)  #predictors, environmental variables used to predict where species are going to be located in space
library(rgdal)   #species, vectors/arrays of x and y points = coordinates in space
library(c("sdm", "raster", "rgdal"))
# OSGeo community
# species data
file <- system.file("external/species.shp", package="sdm")
file
# shx is linking every single point to the arrays, linking spatial data to environmental variables (?)
species <- shapefile(file)  # shapefile() used to import..
species$Occurrence
# how many occurrences are there?
species[species$Occurrence == 1, ]
# features = 94 means we have 94 points with occurrences = species present
species[species$Occurrence == 0, ]
presences <- species[species$Occurrence == 1, ]
absences <- species[species$Occurrence == 0, ]

#plot
plot(species, pch = 19) # we have an area, inside we have the points where the species were recorded
# now what we do is we will plot only the prences of the species 
plot(presences, pch = 19, col = "blue")
# we have 94 points instead of 200 as in the previous plot. 
# now let's plot also the absences with the presences but distinguishing
# hpw to add additional points to a previous plot? with points()
plot(presences, pch = 19, col = "blue")
points(absences, pch = 19, col = "red")
# let's look at the predictors, assigning the path of the data to an object 
path <- system.file("external", package = "sdm")
# list in R the files inside the folder path
lst <- list.files(path = path, pattern = "asc", full.names = T)
lst
predictors <- stack(lst)
predictors
color <- colorRampPalette(c("blue", "orange", "red", "yellow"))(100)
plot(predictors, col = color)
# plot predictor with species presence
plot(predictors$elevation, col = color)
points(presences, pch = 19)

plot(predictors$vegetation, col = color)
points(presences, pch = 19)

plot(predictors$precipitation, col = color)
points(presences, pch = 19)
