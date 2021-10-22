# This is my first code in this github

# input data
# costanza data on stream
water<-c(100,200,300,400,500)
# marta data on fishes genomes
fishes<-c(10,50,60,100,200)
#plot diversity of fishes on the y against the water on the x
plot(water, fishes)

#third lecture
#build a data frame/table in R with data.frame function, also assigning it a name
streams <- data.frame(water, fishes)
streams
#view function will show thw table in a different manner
View(streams)

#fourth lecture, importing and exporting data, setting the working directory (do not put the lab directory in the desktop, but put into the disc C, shorter path)
setwd("C:/lab/")
#data.frame to build a data set from R
streams<-data.frame(water, fishes)
streams
#export the table
write.table(streams, file = "my_first_table.txt")
#import data into R
read.table("my_first_table.txt")
#let's assign it an object inside R
ducciotable <- read.table("my_first_table.txt")
#some descriptive stat
summary(ducciotable)
#nota: in caso di outliers nei valori, la mediana è più effettiva perchè non ne prenderà conto, mentre la media si, quindi meno affidabile in questo caso.
#what if we want the descriptive stat con summary ma solo per una delle due variabili? 
summary(ducciotable$fishes)
#make a histogram
hist(ducciotable$fishes)
hist(ducciotable$water)
