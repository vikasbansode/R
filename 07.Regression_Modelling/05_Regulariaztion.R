################## Regularization ################################
### 1. Ridge L1
### 2. Lasso L2
### 3. ElasticNet



# Libaries need
library(caret)
library(glmnet)
library(mlbench)
library(psych)

# Read Data
data("BostonHousing")
data <- BostonHousing
str(data)

pairs.panels(data[c(-4,-14)],cex=2)

###########################################################################################################
#                         Problem:
#      Collinearity leads to Overfitting.
#  Solution:
# 1. Ridge Regression - Shrinks coefficients to non-zero values to prevent overfit,but keep all variables.
# 2. Lasso Regression - Shrinks regression coefficients, with some shrunk to zero. Thus, it also helps with
#   feature selection.
# 3. Elastic Net Regression - Mix of ridge and lasso
##########################################################################################################

# SSE(ridge) = sum(y-y_pred)^2 + lambda*sum of bita^2
# SSE(lasso) = sum(y-y_pred)^2 + lambda*sum of abs(bita)
# SSE(EN) = sum(y-y_pred)^2 + lambda[(1-alpha)*sum(bita^2+alpha*sum(abs(bita)))]
# when alpha = 0 Elastic model reduces to Ridge
# when alpha = 1 Elastic Model Reduces to LASSO

# Data Partition
set.seed(222)
ind <- sample(2,nrow(data),replace = T,prob = c(0.7,0.3))
train <- data[ind==1,]
test <- data[ind==2,]

# Custom Control Parameters
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter = T)
# Linear Model
set.seed(1234)
lm <- train(medv~.,
            train,
            method='lm',
            trControl=custom)

# Results
lm$results
lm
summary(lm)
plot(lm$finalModel)

# Ridge Regression -> try to shrink the coefficients but keep all variables in the model
set.seed(1234)
ridge <- train(medv~.,
               train,
               method='glmnet',
               tuneGrid = expand.grid(alpha=0,lambda=seq(0.0001,1,length=5)),
               trControl=custom)
# It found the best value of lambda is 0.5, so lambda here is hyperparameter and estimated using cross validation
# that we have specified it is basically strength of the penalty of the coefficient. as we increase the lambda
# we are increasing the penalty and as we decrease the lambda we are decreasing the penalty.

# Plot Results
plot(ridge)
ridge
plot(ridge$finalModel,xvar = 'lambda',label = T)
plot(ridge$finalModel,xvar = 'dev',label = T)
plot(varImp(ridge,scale = F))
plot(varImp(ridge,scale = T))

# Lasso Regression -> It does both shrinkage as well as feature selection
set.seed(1234)
lasso <- train(medv ~.,
               train,
               method = 'glmnet',
               tuneGrid=expand.grid(alpha=1,
                                    lambda=seq(0.0001,0.2,length=5)),
               trControl=custom)

# Plot results
plot(lasso)
lasso
plot(lasso$finalModel,xvar = 'lambda',label=T)
plot(lasso$finalModel,xvar = 'dev',label=T)
plot(varImp(lasso,scale = F))

# Elastic Net Regression
set.seed(1234)
en <- train(medv~.,
            train,
            method='glmnet',
            tuneGrid=expand.grid(alpha=1,lambda=seq(0.0001,0.2,length=5)),
            trControl=custom)

# Plot Results
plot(en)
plot(en$finalModel,xvar = 'lambda',label=T)
plot(en$finalModel,xvar = 'dev',label=T)
plot(varImp(en))

# Compare Models
model_list <- list(linearModel=lm,Ridge=ridge,Lasso=lasso,ElasticNet=en)
res <- resamples(model_list)
summary(res)
bwplot(res)
xyplot(res,metric = 'RMSE')

# Best Model
en$bestTune
best <- en$finalModel
coef(best,s=en$bestTune$lambda)

# Save final Model for Later Use
saveRDS(en,"final_model.rds")

fm <- readRDS("final_model.rds")
print(fm)

# Prediction

p1 <- predict(fm,train)
sqrt(mean((train$medv-p1)^2)) # RMSE

p2 <- predict(fm,test)
sqrt(mean((test$medv-p2)^2)) # RMSE
