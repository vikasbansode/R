library(plumber)

setwd("D:\\from E\\ML\\6.Deployment")
p <- plumb("plumber.R")

p$run(port = 8000)

