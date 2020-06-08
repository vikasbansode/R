
# Read Data

data <- read.csv("CTG.csv",header = T,sep = ",")

#Explore Data
str(data)

# convert int variable to factor

data$NSP <- as.factor(data$NSP)
table(data$NSP)

# Data Partition

set.seed(123)
ind <- sample(2,nrow(data),replace = TRUE,prob = c(0.7,0.3))
train <- data[ind==1,]
test <- data[ind==2,]

# Random Forest developed by aggregating trees.
# can be used for classification or regression.
# Avoid overfitting.
# can deal with large number of features.
# Helps with feature selection based on importance.
# User-friendly:Only 2 free parameters
  # 1. Trees -ntree,default 500
  # 2. variables randomly sampled as candidates at each split -mtry,
  # default is sq.root(p) for classification & p/3 for regression

# STEPS
# 1. Draw ntree bootstrap samples.
# 2. For each bootstrap sample, grow un-pruned tree by choosing best split based on a random sample of 
# mtry predictors at each node
# 3. Predict new data using majority votes for classification and average for regression based on ntree trees.

# Create RandomForest model
library(randomForest)
set.seed(222)
rf <- randomForest(NSP~.,data = train,ntree=300,mtry=8,importance=TRUE,proximity=TRUE)
print(rf)
attributes(rf)

rf$confusion

#Prediction & confusion matrix - train Data
library(caret)
p1 <- predict(rf,train)
head(p1)
head(train$NSP)

confusionMatrix(p1,train$NSP)


# Out of Bag(OOB) Error
# 1. For each bootstrap iteration and related tree,prediction error using data not in bootstrap sample
#(also called out of bag or OOB data) is estimated.
#1. Classification: Accuracy
#2. Regression:R-square & RMSE

#Prediction & Confusion matrix - Test Data
p2 <- predict(rf,test)
confusionMatrix(p2,test$NSP)

# Error rate of Random Forest
plot(rf)

# Tune mtry
t <- tuneRF(train[,-22],train[,22],
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 300,
       trace = TRUE,
       improve = 0.05)
# No of nodes for the trees
hist(treesize(rf),main = "No.of Nodes for the trees",col='green')

#Variable Importance
varImpPlot(rf,sort = T,
           n.var = 10,
           main = 'Top 10-variable importance')
importance(rf)

varUsed(rf)

# partial Dependence Plot
partialPlot(rf,train,ASTV,"2")

# Extract Single Tree
getTree(rf,1,labelVar = TRUE)

# Multi-dimensional scaling plot of Proxmity Matrix
MDSplot(rf,train$NSP)
