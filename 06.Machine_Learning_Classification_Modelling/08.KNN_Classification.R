# 1.Set working Directory

setwd("D:\\from E\\ML")

# 2.Import pre-requisite libraries
library(caret)
library(pROC)
library(mlbench)

# 3.Read Data

data <- read.csv("binary.csv",header = TRUE,sep = ",")
str(data)

# 4.Convert admit to factor

data$admit[data$admit ==0] <- 'No'
data$admit[data$admit ==1] <- 'Yes'
data$admit <- factor(data$admit)

# 5.Data Partition

set.seed(1234)
ind <- sample(2,nrow(data),replace = TRUE,prob = c(0.7,0.3))
training <- data[ind==1,]
test <- data[ind==2,]

# 6.Build a Modle KNN

trControl <- trainControl(method = 'repeatedcv',
                          number = 10,
                          repeats = 3,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary
                          )
set.seed(222)

fit <- train(admit~.,data = training,method='knn',tuneLength=20,
            trControl=trControl,preProc=c("center","scale"),
            metric='ROC',
            tuneGrid=expand.grid(k=1:60))
# 7.Model Performance

fit
plot(fit)
varImp(fit)

# 8.Prediction

pred <- predict(fit,newdata = test)

# 9. Confusion Matrix

confusionMatrix(pred,test$admit)

# 10. Further improvement use Class imbalance
