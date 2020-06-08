
# Set working Directory

setwd("D:\\from E\\ML")

# Read Csv file

utilities <- read.csv("utilities.csv",header = T,sep = ",")
str(utilities)

# Scatter plot
plot(Fuel_Cost ~ Sales,utilities)
with(utilities,text(Fuel_Cost~Sales,labels=Company,pos=4,cex=.4))

# NOTE : for cluster Analysis we need to have quantitative Data
z <- utilities[,-c(1,1)] # removing company column
# Normalization 
m <- apply(z,2,mean)
s <- apply(z,2,sd)
z <- scale(z,m,s)

# Calculate Euclidean distance
distance <- dist(z)
print(distance,digits=3)

# Cluster Dendrogram with complete Linkage
hc.c <- hclust(distance)
plot(hc.c)
plot(hc.c,labels = utilities$Company)
plot(hc.c,hang = -1)

# Cluster Dendrogram with Average Linkage
hc.a <- hclust(distance,method='average')
plot(hc.a,hang=-1)

# Cluster Membership
member.c <- cutree(hc.c,3)
member.a <- cutree(hc.a,3)
table(member.c,member.a)

# Cluster Means
aggregate(z,list(member.c),mean)
aggregate(utilities[,-c(1,1)],list(member.c),mean)

# Silhouette Plot
library(cluster)
plot(silhouette(cutree(hc.c,3),distance))

# Scree Plot
wss <- (nrow(z)-1)*sum(apply(z,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(z, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

# K-Means Clustering
kc <- kmeans(z,3)
kc

kc$cluster  # you get clusters
kc$centers  # you get averages

plot(Sales~D.Demand,utilities,col=kc$center)
