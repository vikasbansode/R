setwd("D:\\from E\\ML")

# Read Data
data <- read.csv("Cardiotocographic.csv",header = TRUE,sep = ",")
str(data)

# Convert int to factor
data$NSPF <- factor(data$NSP)

# Data Partition into Training and Testing datasets
set.seed(1234)
pd <- sample(2,nrow(data),replace = TRUE,prob = c(0.8,0.2))
train <- data[pd==1,]
test <- data[pd==2,]

# Decision Tree with Party
library(party)
tree <- ctree(NSPF~LB+AC+FM,data = train,controls=ctree_control(mincriterion = 0.99,minsplit = 500))
tree
plot(tree)

# Predict
predict(tree,test,type='prob')
predict(tree,test)

# Decision tree with rpart
library(rpart)
tree1 <- rpart(NSPF~LB+AC+FM,train)
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(tree1,extra=1)

# Prediction
predict(tree1,test)

# Misclassification error for 'train' data
tab <- table(predict(tree),train$NSPF)
print(tab)
1-sum(diag(tab))/sum(tab)

# Misclassification error for 'test' data
tab1 <- table(predict(tree),test$NSPF)
print(tab1)
1-sum(diag(tab))/sum(tab)
      