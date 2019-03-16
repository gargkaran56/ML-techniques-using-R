install.packages('ggplot')
#fetching the data
library(datasets)
#gives first six rows of data
head(iris)
#ggplot used for graphs
library(ggplot2)
#geom_point() means giving scattered plot,aes stands for axis
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

#reproduceability ,so that same data is used for traing and testing
set.seed(20)
#making cluster using all rows and only 3 & 4 column
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
#showing the data
irisCluster

table(irisCluster$cluster, iris$Species)

irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) + geom_point()

setwd("C:/Users/Anirban Chakraborty/Desktop/New folder")
#exporting the results 
x <- irisCluster$cluster
write.csv(x, file="irisclusterresults.csv")


