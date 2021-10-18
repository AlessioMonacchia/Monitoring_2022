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
