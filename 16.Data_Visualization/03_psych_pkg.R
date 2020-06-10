
setwd("D:\\from E\\ML")

data <- read.csv("vehicle.csv",header = T,sep = ",")

# Multiple plots

library(psych)

pairs.panels(data[2:5])

# Scatter plots
library(ggplot2)

qplot(Mileage,fm,data = data)
qplot(Mileage,fm,data = data,color=State)


# Multiple plots
data("mpg")

qplot(displ,hwy,data = mpg,facets = drv~.)
qplot(hwy,data = mpg,facets = drv~., binwidth=1)


# Motion Chart

library(googleVis)
data("Fruits")

mychart <- gvisMotionChart(Fruits,idvar = "Fruit","Year")
plot(mychart)


# Geo Map
data("Population")
head(Population)
w <- gvisGeoMap(Population,locationvar = 'Country',numvar = 'Population')
plot(w)


