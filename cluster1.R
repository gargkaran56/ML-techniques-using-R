setwd("/Users/karangarg/Documents/My Tableau Repository")
install.packages('rattle')
library(rattle)
View(wine)

data(wine, package="rattle")
head(wine)

df <- scale(wine[-1])

# plot of the total within-groups sums of squares against the number of clusters in a K-means solution can be helpful. 
# A bend in the graph can suggest the appropriate number of clusters. The graph can be produced by the following function.
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(df)


install.packages("NbClust")
library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")

table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

set.seed(1234)
fit.km <- kmeans(df, 3, nstart=25)
fit.km$size
fit.km$centers
aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)
