
# Setworking directory
setwd("D:/All/github/R/15.Social_Network_and_Text_mining")

# load library
library(tm)

# Read Text file
tweets <- readLines("Tweets.txt")

# Build Corpus
corpus <- Corpus(VectorSource(tweets))

# Create term document matrix
tdm <- TermDocumentMatrix(corpus,control = list(minWordLength=c(1,Inf)))

t <- removeSparseTerms(tdm,sparse = 0.98)

# Convert t into matrix

m <- as.matrix(t)

# Plot frequent terms

freq <- rowSums(m)

freq <- subset(freq,freq>=50)

barplot(freq,las=2,col = rainbow(25))


# Hierachical word/tweet clustering using dendogram
distance <- dist(scale(m))
print(distance,digits = 2)
print(distance,digits=2)

hc <- hclust(distance,method = "ward.D")
plot(hc)

plot(hc,hang=1)
rect.hclust(hc,k=10)

# Non-hierarchical k-means clustering of words/tweets
# transpose m
m1 <- t(m)

set.seed(222)

k <- 12
kc <- kmeans(m1,k)
kc

