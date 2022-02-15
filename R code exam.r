## Exam Project Alessio Monacchia

setwd("~/Dropbox/Alessio/Project")
library(ggplot2)
library(raster)
library(gridExtra)
library(RStoolbox)
library(corrplot)
library(viridis)

list <- list.files(pattern = "L1T") # create list of satellite images from Adamello glacier area. The images make use of false colors in the form of the Normalized Difference Snow Index (NDSI) which uses bands B2 (green) and B5 (short wave infrared) in the formula: (B2-B5)/(B2+B5). NDSI is a suitable index to detect snow cover and clearly distinguish it from other types of surface reflectances, including clouds.
list_rast <- lapply(list, raster) # import raster images included in list
list_brick <- lapply(list, brick) # import same images as Raster brick objects

# Cropping both the raster layers list and the raster bricks list to the size of the smallest image
ext <- c(0, 764, 0, 699) 
list_rastcr <- lapply(list_rast, crop, ext)
list_brickcr <- lapply(list_brick, crop, ext)

plotRGB(list_brick[[8]], r=1, g=2, b=3, stretch = "Lin") # plotting in RGB a single Raster Brick from the Raster brick list previously created, visualize original image

Ada_brick <- brick(list_rastcr) # create raster brick from the cropped raster layers list to be used for the time series plot
cl <- colorRampPalette(c("blue", "orange"))(100) # create a color blind friendly palette for the time series plot
plot(Ada_brick, col=cl, main="Adamello snow cover 1986-2020", cex.main=2) # plot TEMPORAL EVOLUTION
plotRGB(Ada_brick, r=8, g=4, b=1, stretch = "lin") # comparison of snow cover in three years 1986-2001-2020 by plotting in plotRGB the Raster layers corresponding to the years 1986, 2001 and 2020

Snowdif <- Ada_brick[[1]] - Ada_brick[[8]] # creating a new raster layer calculated by subtracting the 2020 layer to the 1986 layer, it highlights the areas where the snow cover has decreased/increased
p3 <- ggplot() + # plot calculated difference, use Viridis package for colors
  geom_raster(Snowdif, mapping = aes(x=x, y=y, fill=layer)) +
  scale_fill_viridis(option="viridis") +
  ggtitle("Snow cover difference 1986-2020") + 
  labs(fill='Difference')

mytheme <- theme( # set of features to improve plot main title and axis titles
  plot.title = element_text(family = "Helvetica", face = "bold", size = (25)),
  axis.title = element_text(family = "Helvetica", size = (20), colour = "steelblue4"),
  axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (20)))

print(p3 + mynamestheme) # improved snow cover difference ggplot

# Procedure for quantitative ESTIMATION OF SNOW LAND COVER
Ada_class <- lapply(list_rastcr, unsuperClass, nClasses = 2) # dividing raster brick cell values into 2 classes, one will relate to the snow cover, the rest will cluster around the non-snow cover
plot(Ada_class[[1]]$map) # visualize the one of the "clustered" images

y <- NULL # create an empty vector to be used for creating a matrix for storing output values (frequencies) of the for loop
for(i in 1:8) {  # calculating the frequencies of each clustered raster layer = for each year
  frequenc <- freq(Ada_class[[i]]$map)
  y <- rbind(y, frequenc)
}

area <- sum(y[1:2,2]) # calculate total area 

percentages <- y[, 2]/area*100 # create a vector containing the % of snow land cover, calculate the % of snow cover by dividing the frequencies for the area and multiplying by 100
percentages <- percentages[-c(2, 3, 5, 7, 10, 11, 14, 16)] # removing rows not containing the snow cover %, it is possible as from the temporal evolution plot it is clear that the snow cover extent is lower than the rest of the land cover types for all the years
year <- c("1986", "1991", "1997", "2001", "2006", "2011", "2016", "2020") # create a vector assigning the years corresponding to the snow cover %

dataset <- data.frame(percentages, year) # create a dataset containing snow cover % and year

# here I create a  barchart of the dataset just created, it describes the temporal evolution of % of ice cover from 1986 to 2020 at intervals of approximately 5 years
p1 <- ggplot(dataset, aes(x=year, y=percentages)) + 
  geom_bar(stat="identity", fill = "blue") 
p1

print(p1 + mynamestheme + labs(title = "Barplot Adamello snow cover 1986-2020", y = "Snow cover (%)", x = "Year")) # Include theme to ggplot

# correlation
dataset$year <- as.numeric(dataset$year) # need to change the year column into a numeric one in order to run a correlation analysis with the %
cor.test(dataset$year, dataset$percentages) # the correlation test 

model <- lm(percentages~year, data = dataset) # create a linear model to describe the correlation between time and percentages
summary(model)

p2 <- ggplot(corr_set, aes(x = year, y = percentages)) + # plot the linear model, visualize the % variation through time with fitted linear model
  geom_point() +
  stat_smooth(method = "lm", col = "red")
print(p2 + mynamestheme + labs(title = "Linear model plot", y = "Snow cover (%)", x = "Year"))

f <- as.data.frame(c(2030, 2040)) # create a vector for predict function as to predict the snow cover values for 2030 and 2040
colnames(f) <- "year" 
prediction <- predict(model, newdata = f) # results of prediction using fitted model for 2030 and 2040
