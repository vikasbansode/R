library(mongolite)
library(datasets)

data("iris")

names(iris)[1:4] = gsub("[.]","",names(iris)[1:4])
head(iris)

# connect mongo DB collection

c=mongo(collection = "iris",db='MyIrisDemo')

# Insert into collection

c$insert(iris)

# Check for collection

c$distinct("Species")

# Check for Aggregation

c$aggregate()

# Count

c$count()

c$info()

