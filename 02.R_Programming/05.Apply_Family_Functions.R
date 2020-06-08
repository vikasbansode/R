# Set working Directory

setwd("D:\\from E\\R")

Stockdata <- read.csv("StockExample.csv",header = T,sep = ",")
Stockdata

# Calculate the mean price of each stock

apply(Stockdata[,-1],MARGIN = 2,FUN = mean,na.rm=TRUE)


# OR

colMeans(Stockdata[,-1],na.rm = TRUE)

# Find the Maximum Stock price, for each stock
apply(Stockdata[,-1],MARGIN = 2,FUN = max,na.rm=TRUE)
apply(Stockdata[,-1],MARGIN = 2,FUN = quantile,probs=c(0.2,0.8),na.rm=TRUE)

# Create a plot of each column using line

apply(Stockdata[,-1],MARGIN = 2,FUN = plot,type="l")

apply(Stockdata[,-1],MARGIN = 2,FUN = plot,type="l",
      main="stock",ylab="Price",xlab="Day")


# Calculating sum of each stock price column

apply(Stockdata[,-1],MARGIN = 2,FUN=sum,na.rm=TRUE)

rowSums(Stockdata[,-1],na.rm = TRUE)


# Using the Apply family of Functions in R
# apply()
# lapply()
# sapply()
# tapply()
# mapply()

data <- matrix(c(1:10,21:30),nrow = 5,ncol = 4)
data

# Apply function : can be used to apply a function to a matrix

apply(data,1,mean)

# lapply function : is similar to apply, but it takes a list as an input,
# returns a list as the output

data <- list(x = 1:5,y=6:10,z=11:15)
data

lapply(data,FUN = median)

# Sapply function : is the same as lapply, but it returns a vector insted of list.

sapply(data,FUN = median)

# tapply function : splits the array based on specified data, usually factor levels and then
# applies the function to it.

library(datasets)
tapply(mtcars$wt,mtcars$cyl,mean)

unique(mtcars$cyl)

# mapply function : is a multivariate version of sapply. It will apply the specified
# Function to the first element of each argument first, followed by the second element, and so on.

x < 1:5
b <- 6:10

mapply(sum,x,b)

# It adds 1 to 6, 2 to 7 and so on.