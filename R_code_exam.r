# R code for assessment of ecosystems state in the Trasimeno lake area (IT) based on temperature, land cover and lake area time series analysis over the 
# years (2001-2022).
# Data was downloaded from: https://eos.com/find-satellite/

setwd("~/Trasimeno") # set the working directory, we use the folder where the downloaded images are present, the images where downloaded from the Earth 
# Observing System (EOS) website

# recalling all the packages we are going to use by the library function
library(raster)
library(rgdal)
library(viridis)
library(ggplot2)
library(RStoolbox)
library(patchwork)

# import the downloaded images in R with the brick function
list <- list.files(pattern = "L1TP") # create a list containing all the images to import
tras <- lapply(list, brick) # import with brick and lapply

# the imported images have slightly different dimension, I need to crop all to the same size 
ext <- c(254115, 274845, 4770945, 4790085) # create an extent objext with the size of the smallest of the images
trasc <- lapply(tras, crop, ext) # crop all the files

# change names of the files in the list and all the names of the bands in the files
names(trasc) <- c(2001, 2008, 2015, 2022) 
names(trasc$`2001`) <- c("b1", "b2", "b3", "b4", "b5", "b6")
names(trasc$`2008`) <- c("b1", "b2", "b3", "b4", "b5", "b6")
names(trasc$`2015`) <- c("b1", "b2", "b3", "b4", "b5", "b6")
names(trasc$`2022`) <- c("b1", "b2", "b3", "b4", "b5", "b6")

# plot all the images in natural colors with plotRGB
par(mfrow=c(2,2)) # placing all the plots in a same window with par function
plotRGB(trasc$`2001`, r=1, g=2, b=3, stretch="lin")
plotRGB(trasc$`2008`, r=1, g=2, b=3, stretch="lin")
plotRGB(trasc$`2015`, r=1, g=2, b=3, stretch="lin")
plotRGB(trasc$`2022`, r=1, g=2, b=3, stretch="lin")
dev.off()



### TEMPERATURE analysis by using the thermal bands: first separate the thermal bands (infrared) from each of the RasterBricks, we assign them to an object 
# with nomenclature corresponding to the relative year.

# Since the values differ, originating from different Landsat satellites, it is necessary to normalize them in order to perform a comparison.
thermal01 <- (trasc$`2001`$b6/maxValue(trasc$`2001`$b6))
thermal08 <- (trasc$`2008`$b6/maxValue(trasc$`2008`$b6))
thermal15 <- (trasc$`2015`$b6/maxValue(trasc$`2015`$b6))
thermal22 <- (trasc$`2022`$b6/maxValue(trasc$`2022`$b6))

# create and plot a stack including only the thermal bands from the different years
thermal_stack <- stack(thermal01, thermal08, thermal15, thermal22) # we create a single stack containing the cropped and normalized thermal images.
plot(thermal_stack)

# compare the thermal Rasters from different years with plotRGB and with the pairs() function, which creates a matrix of scatterplots.
plotRGB(thermal_stack, r=1, g=2, b=4, stretch="lin")
names(thermal_stack) <- c("2001", "2008", "2015","2022") # I decided to rename the layers with the corresponding years.
pairs(thermal_stack) # NOTE: temperature values are representative of a single day and therefore are not effectively representing the actual temperatures 
# of the period, which estimation would require an average of the entire month of August.

# calculate and plot the difference between the thermal bands values of the years 2022 and 2001
dif_thermal <- thermal_stack$X2022-thermal_stack$X2001
cl <- colorRampPalette(c("blue", "white", "red"))(100)
plot(dif_thermal, col=cl)



### NDVI analysis: First we calculate the NDVI for each year by applying the formula NDVI = (NIR-RED)/(NIR+RED), the NIR band is the number 4 whereas 
# the visible red band is the number 1.
NDVI01 <- (trasc$`2001`$b4 - trasc$`2001`$b1) / (trasc$`2001`$b4 + trasc$`2001`$b1)
NDVI08 <- (trasc$`2008`$b4 - trasc$`2008`$b1) / (trasc$`2008`$b4 + trasc$`2008`$b1)
NDVI15 <- (trasc$`2015`$b4 - trasc$`2015`$b1) / (trasc$`2015`$b4 + trasc$`2015`$b1)
NDVI22 <- (trasc$`2022`$b4 - trasc$`2022`$b1) / (trasc$`2022`$b4 + trasc$`2022`$b1)
plot(NDVI22)
list_NDVI <- c(NDVI01, NDVI08, NDVI15, NDVI22) # create a list of the NDVI Rasters

# now we apply the unsuperclass function from the RStoolbox package to perform an unsupervised classification which we are going to use to differentiate the
# land cover types
NDVI_class <- lapply(list_NDVI, unsuperClass, nClasses=3) # we use 3 classes as by empirical analysis it appeared to be the most suitable classification 
# approach, to simplify coding we use the lapply function.

# now we need to export the frequencies of pixels ordered according to the 3 identified classes
x <- NULL # create an empty vector to be used for creating a matrix in the for loop for storing output values
for(i in 1:4) {  # calculating the frequencies of each clusterized raster brick by for loop
  frequenc <- freq(NDVI_class[[i]]$map)
  frequenc <- frequenc[-4,] # escluding the NA values which are identified as a 4th class, so will always be present in the 4th row
  x <- rbind(x, frequenc)
}

# we create a dataframe including the information needed: cover, year and area (count)
land_cover <- as.data.frame(x) # transforming the matrix into a dataframe suitable for analysis and plotting 

# we do not know which land cover where assigned to which values by the software, so we need to plot each classified image and manually tell the software
# the right values-land cover combinations.
plot(NDVI_class[[1]]$map)
land_cover$cover <- c("lake","urban/agr","forest","urban/agr","lake","forest","lake","urban/agr","forest","forest","lake","urban/agr") # manually assign 
# each land cover type to the corresponding classification value.
land_cover$year <- c(rep(2001, 3) , rep(2008, 3) , rep(2015, 3), rep(2022, 3)) # add column containing the year
total_pixels <- sum(land_cover$count[1:3]) # calculate the total amount of pixels
land_cover$count <- 370*land_cover$count/total_pixels # calculating the area in km^2 using the known total area (370m^2)

# we create a stacked barchart to illustrate the land cover types evolution through the years
p <- ggplot(land_cover, aes(x=year, y=count, fill=cover)) +
  geom_bar(position="stack", stat="identity") +
  ggtitle("Land cover evolution in the Trasimeno lake area")
p + ylab("area (km^2)")



### WATER BODIES: the EOS websites suggests a band combination which is effective in distinguishing land from water, it uses the blue (3), NIR (4) and  
# shortwave infrared (5) bands to play the trick
# I want to perform the same quantitative analysis and plotting as with NDVI and land cover type, but this time specifically on the lake extent
water01 <- stack(trasc$`2001`$b4, trasc$`2001`$b5, trasc$`2001`$b3)
water08 <- stack(trasc$`2008`$b4, trasc$`2008`$b5, trasc$`2008`$b3)
water15 <- stack(trasc$`2015`$b4, trasc$`2015`$b5, trasc$`2015`$b3)
water22 <- stack(trasc$`2022`$b4, trasc$`2022`$b5, trasc$`2022`$b3)
water_list <- c(water01, water08, water15, water22)

# performing unsupervised classification to obtain estimates of water cover, I used 3 classes 
water_class <- lapply(water_list, unsuperClass, nClasses = 3) # dividing raster brick cell values into 3 categories
y <- NULL # create an empty vector to be used for creating a matrix in the for loop for storing output values
for(i in 1:4) {  # calculating the frequencies of each clusterized Raster Brick using a for loop to facilitate the calculations
  frequenc <- freq(water_class[[i]]$map)
  y <- rbind(y, frequenc)
}

plot(water_class[[3]]$map) # plot each classified raster image to understand which value (class) the software assigned to water bodies in each image
water_array <- y[c(1,7,10,15),] # select only the water bodies values (note: this value might change depending on the classes assignment made by the software)

# calculate water cover extent
# I multiple the water cover% for the known extension in Km^2 of the area under study, the latter information was taken at the moment I downloaded the images
water_area <- 370*water_array[,2]/total_pixels

# create a barplot for the water cover evolution in time with ggplot and geom_bar
year <- c(2001, 2008, 2015, 2022)
water_data <- data.frame(water_area, year)

w <- ggplot(water_data, aes(x=year, y=water_area, fill=year)) +
  geom_bar(stat="identity", fill = "blue") +
  ggtitle("Trasimeno lake area evolution")
w + ylab("area (km^2)")


### DROUGHT/HEAT WAVES VULNERABILITY: here we create a PCA analysis whose results can be used as a proxy of land vulnerability to drought and heat waves
# precipitation data was not found, so I created a raster whose values are within the precipitation range found in literature for the study area.
# Only data for the year 2022 were considered. In particular the model was fed with temperature, vegetation (NDVI), water cover and precipitation.

# we create a RasterLayer with the correct parameters for the precipitation data.
r <- raster(ncol=691, nrow=638, xmn=254115, xmx=274845, ymn=4770945, ymx=4790085) # creating a raster with same extent and dimensions as the analysed images
projection(r) <- "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs " # set the coordinate reference system (CRS) (define the projection).
values(r) <- runif(ncell(r), min=700, max=900) # set the values, we assign randomly values comprised between 700 and 900 with the runiff function.

# Prepare the factors to include in the PCA analysis.
temperature <- thermal22n
vegetation <- NDVI22
lake <- water22
precipitation <- r/maxValue(r)
PCA_factors <- stack(temperature, vegetation, lake, precipitation)
PCA_drought <- rasterPCA(PCA_factors) # performing the RasterPCA on the stacked PCAfactors
drought <- PCA_drought$map$PC1/maxValue(PCA_drought$map$PC1) # we normalize values to improve visual representation

# plot the initial natural colors image of the lake area with the image representing drought vulnerability and export in pdf format making use of Viridis
# package for color blind friendly pallette and of patchwork package for easily cobining the plots into a single image.
pdf("drought.pdf")
p1 <- ggRGB(trasc$`2022`, r=1, g=2, b=3, stretch="lin") +
  ggtitle("Trasimeno lake area in natural colors (20/7/2022)")
p2 <- ggplot() +
  geom_raster(drought, mapping = aes(x=x, y=y, fill=PC1)) +
  scale_fill_viridis(option="plasma") +
  ggtitle("Drought vulnerability Trasimeno lake area")
p1+p2
dev.off()

### FINE :) ###
