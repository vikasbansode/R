if(!require("plumber")) install.packages("plumber")
if(!require("jsonlite")) install.packages("jsonlite")
library(plumber)
library(jsonlite)

pr <- plumb("plumber-api.R")

swaggerFile <- pr$swaggerFile()
swaggerFile$info$title <- "plumberCarsService"
swaggerFile$info$description <- "Returns the Probability of a vehicle fitted with a manual transmission"
swaggerFile$info$version <- "1.0.0"
swagger <- toJSON(swaggerFile,pretty = TRUE,auto_unbox = TRUE)
cat(swagger,file = "plumber-swagger.json",append = FALSE)

pr$run(port=8000)
