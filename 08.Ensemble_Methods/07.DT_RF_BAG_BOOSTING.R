setwd("D:\\from E\\ML")

library(tree)
install.packages('tree')

data <- read.csv("binary.csv",header = T,sep = ",")
str(data)

data$admit <- ifelse(data$admit ==0,"No","Yes")
data$admit <- as.factor(data$admit)

set.seed(2)

train <- sample(1:nrow(data),200)
data.test <- data[-train,]
data.train <- data[train,]

tree.data <- tree(admit~gre+gpa+rank,data = data.train)
tree.pred <- predict(tree.data,data.test[,-1],type = 'class')
table(tree.pred,data.test$admit)

# To improve the performance use cross validation

set.seed(2)
cv.data <- cv.tree(tree.data,FUN = prune.misclass)
plot(cv.data$size,cv.data$dev,type="b")
prune.data <- prune.misclass(tree.data,best = 3)
tree.pred <- predict(prune.data,data.test,type = 'class')
table(tree.pred,data.test$admit)
plot(prune.data);text(prune.data,pretty = 0)

# Regression Tree
library(MASS)
set.seed(1)
train <- sample(1:nrow(Boston),nrow(Boston)*0.6)
tree.boston <- tree(medv~.,Boston,subset = train)
yhat <- predict(tree.boston,newdata = Boston[-train,])
boston.test <- Boston[-train,"medv"]
mean((yhat-boston.test)^2)

cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')
prun.boston <- prune.tree(tree.boston,best = 7)
yhat <- predict(prun.boston,newdata = Boston[-train,])
mean((yhat-boston.test)^2)

# Random Forest and Bagging
library(randomForest)
set.seed(1)
bag.boston <- randomForest(medv~.,data = Boston,subset = train,mtry=4,importance=TRUE)
yhat.bag <- predict(bag.boston,newdata = Boston[-train,])
mean((yhat.bag-boston.test)^2)
varImpPlot(bag.boston)

# Boosting
library(gbm)
set.seed(1)
boost.boston <- gbm(medv~.,data = Boston[train,],distribution = "gaussian",
                    n.trees = 5000,interaction.depth = 4)

yhat.boost <- predict(boost.boston,newdata = Boston[-train,],n.trees = 5000)
mean((yhat.boost-boston.test)^2)

boost.boston <- gbm(medv~.,data = Boston[train,],distribution = "gaussian",
                    n.trees = 5000,interaction.depth = 4,shrinkage = 0.2)

yhat.boost <- predict(boost.boston,newdata = Boston[-train,],n.trees = 5000)
mean((yhat.boost-boston.test)^2)
