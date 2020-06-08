
# plumber.R

setwd("D:\\from E\\ML\\6.Deployment")
mymodel <- readRDS("slr.rds")

#* Returns the predicted prices
#* @param area the area
#* @post /PredictthePrice

function(area){
  newdata <- data.frame(area=as.numeric(area))
  predict(mymodel,newdata,type="response")
}

setwd("D:\\from E\\ML")

d <- read.csv("slr_test_areas.csv",header = T,sep = ",")
p <- predict(mymodel,d)
d

#* Returns the predicted area Prices
#* @post /PredictwithNewData

function(area){
  #newdata <- data.frame(d) # New data is not in data.frame then create df
  p <- predict(mymodel,d)
  d['prices'] <- p
  data.frame(d)
}


