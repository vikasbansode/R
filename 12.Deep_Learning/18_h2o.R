
install.packages('h2o')
library(h2o)
setwd("D:\\from E\\Deep_Learning")

download.file("https://gist.githubusercontent.com/tiangechen/b68782efa49a16edaf07dc2cdaa855ea/raw/0c794a9717f18b094eabab2cd6a6b9a226903577/movies.csv",destfile = "movies.csv")


library(h2o)
h2o.init()


ausPath = system.file("extdata", "australia.csv", package="h2o")
australia.hex = h2o.uploadFile(path = ausPath)
summary(australia.hex)
pca_model=h2o.prcomp(training_frame = australia.hex,
                     k = 8,
                     transform = "STANDARDIZE")
summary(pca_model)
barplot(as.numeric(pca_model@model$importance[2,]),
        main="Pca model",
        xlab="Pca component",
        ylab="Proportion of Variance")



movies <- read.csv("movies.csv",header = TRUE,sep = ",")
head(movies)

model <- h2o.deeplearning(2:3,training_frame = as.h2o(movies),
                          hidden = c(2),
                          autoencoder = T,
                          activation = "Tanh")
summary(model)

features=h2o.deepfeatures(model,
                          as.h2o(movies),
                          layer=1)
d=as.matrix(features[1:10,])
labels=as.vector(movies[1:10,2])
plot(d,pch=17)
text(d,labels,pos=3)



