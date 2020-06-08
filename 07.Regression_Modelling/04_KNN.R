
# import libraries

library(caret)
library(pROC)
library(mlbench)


# Read Data
data("BostonHousing")
data <- BostonHousing
str(data)

# Data Partition
set.seed(1234)
ind <- sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
training <- data[ind==1,]
test <- data[ind==2,]

# KNN Model

trControl <- trainControl(method = 'repeatedcv',
                          number = 10,
                          repeats = 3
                          )

set.seed(333)
fit <- train(medv~.,
             data = training,
             tuneGrid=expand.grid(k=1:70),
             method='knn',
             metric='Rsquared',
             trControl=trControl,
             preProc=c('center','scale'))

# Model Performance
fit
plot(fit)
varImp(fit)

# Prediction
pred <- predict(fit,newdata = test)
RMSE(pred,test$medv)
plot(pred~test$medv)
