#################### SUPPORT VECTOR MACHINE ##################

# 1. Set working Directory

setwd("D:\\from E\\ML")

# 2. Read Data

data("iris")
str(iris)

# 3. Do Exploratory Data Analysis here

library(ggplot2)
qplot(Petal.Length,Petal.Width,data = iris,color=Species)

# 4. Build Model Support vector machine
library(e1071)
mymodel <- svm(Species~.,data = iris,kernel='radial')
# Change Kernal type liner,polynomial,sigmoid, and default is radial
summary(mymodel)
plot(mymodel,data = iris,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3,Sepal.Length=4))

# Confusion Matrix
pred <- predict(mymodel,iris)
tab <- table(Predicated=pred,Actual=iris$Species)
tab 
# Misclassification Error

1-sum(diag(tab))/sum(tab)

# radial is best all of others.

######################################################
##################### TUNNING MODEL ##################
###############################################################
# Tunning also called hyperparameter optimization and it helps#
# to select the best model                                    #
###############################################################

set.seed(123)
tmodel <- tune(svm,Species~.,data = iris,
     ranges = list(epsilon=seq(0,1,0.1),cost=2^(2:7)))
plot(tmodel)
summary(tmodel)

# Choosing our best model
mymodel <- tmodel$best.model
summary(mymodel)
plot(mymodel,data = iris,
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width=3,Sepal.Length=4))

# Confusion Matrix
pred <- predict(mymodel,iris)
tab <- table(Predicated=pred,Actual=iris$Species)
tab 
# Misclassification Error

1-sum(diag(tab))/sum(tab)

#### Now model looks good, as we have only 2 misclassfication 
#### and misclassifcation error is very less compared to previous models

