######## Single Varialbe Linger Regression #######

# Set working Directory

setwd("D:\\from E\\ML")

# 1. Read Data

data <- read.csv("homeprices.csv",header = T,sep = ",")

# 2. visualize data in Scatter price

plot(data,main="Home Prices",xlab='area sq ft',ylab='price(US$)',pch=1)
abline(reg = reg,col='red')

# 3. Build a model

reg <- lm(price ~.,data = data)
reg
summary(reg)

reg$coefficients
reg$residuals

# 4. Predict a Model

predict(reg, data.frame(area = 3300))

# Read test Data

d <- read.csv("areas.csv",header = T,sep = ",")
head(d)

# use  model to predict new data

p <- predict(reg,d)
d['prices'] <- p
d

# plot predicted values

plot(data,main="Home Prices",xlab='area sq ft',ylab='price(US$)',pch=1)
lines(d$area,p,type='l',col='blue')

# Save predicted values in csv file

write.csv(d,"prediction.csv",row.names = FALSE)



