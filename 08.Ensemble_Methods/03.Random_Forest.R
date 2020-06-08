# install packages
install.packages('stats')
install.packages('dplyr')
install.packages('randomForest')
# load packages
library(stats)
library(dplyr)
library(randomForest)
#load data into mydata object
mydata <- iris
#inspect base data
View(mydata)

# Varialbe selection
str(mydata)
#split Data in Training and Testing
# A vector that has random sample of training values
ind <- sample(2,nrow(mydata),replace = TRUE,prob = c(0.7,0.3))

# Training Data
training <- mydata[ind==1,]

# Testing Data
testing <- mydata[ind==2,]

# Build RandomForest model
rfm <- randomForest(Species~.,data = training)

# Evaluating Model Accuracy
pred <- predict(rfm,testing)
testing$pred <-pred
View(testing)

# Building confusion Matrix

cfm <- table(testing$Species,testing$pred)
cfm

Classification_Accuracy <- sum(diag(cfm))/sum(cfm)
Classification_Accuracy

1-sum(diag(cfm))/sum(cfm)
