# Install pre-requisite packages
install.packages('stats')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('ggfortify')

# load required packages
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
# Unsupervised learning -hence converting to unlabelled variables
View(iris)
mydata <- select(iris,c(1,2,3,4))

# WSS plot to choose maximum number of clusters
wssplot <- function(data,nc=15,seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centers = i)$withinss)}
  plot(1:nc,wss,type = "b",xlab = "number of clusters",
       ylab="within groups sum of squares")
}
# Spotting the kink in the curve in oder to choose the optimum
wssplot(mydata)

# K-means cluster
KM <- kmeans(mydata,2)

# Evaluating cluster Analysis

# Cluster plot
autoplot(KM,mydata,frame=TRUE)

# Cluster centres
KM$centers
