################### eXtreme Gradient Boosting ###############
#   Why eXtreme Gradient Boosting?                          #
# -> 1. Popular in machine Learning Challenges              #
#    2. Fast and Accurate                                   #  
#    3. It can handle missing values                        #
# NOTE : xgboost requires numeric matrix for its input.     #  
#############################################################

# Set working Directory

setwd("D://from E//ML")

# get working Directory

getwd()

# load pre-requisite packages

library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)

# Read Data

data <- read.csv("binary.csv",header = TRUE,sep = ",")
str(data)

# Convert into factor variables
data$rank <- as.factor(data$rank)

# Data Partition
set.seed(1234)
ind <- sample(2,nrow(data),replace = T,prob = c(0.8,0.2))
train <- data[ind==1,]
test <- data[ind==2,]

# Create Matrix - One-Hot Encoding for factor variables - train Data

trainm <- sparse.model.matrix(admit ~.-1,data = train)
head(trainm)

train_Label <- train[,"admit"]
train_matrix <- xgb.DMatrix(data = as.matrix(trainm),
                            label=train_Label)

# Create Matrix - One-Hot Encoding for factor variables - test Data

testm <- sparse.model.matrix(admit ~.-1,data = test)
head(test)

test_Label <- test[,"admit"]
test_matrix <- xgb.DMatrix(data = as.matrix(testm),
                            label=test_Label)
# Parameters
nc <- length(unique(train_Label))
xgb_params <-list("objective" = "multi:softprob",
                  "eval_metric" = "mlogloss",
                  "num_class"=nc)

watch_list <- list(train=train_matrix,test=test_matrix)

# Build eXtreme Gradient Boosting Model
bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = 100,
                       watchlist = watch_list,
                       eta=0.05)

# Training & Test  Error plot
e <- data.frame(bst_model$evaluation_log)
plot(e$iter,e$train_mlogloss,col='blue')
lines(e$iter,e$test_mlogloss,col='red')

min(e$test_mlogloss)
e[e$test_mlogloss == 0.597642,]

# Feature Importance 
imp <- xgb.importance(colnames(train_matrix),bst_model)
print(imp)
xgb.plot.importance(imp)

# Prediction & Confusion matrix - test Data
p <- predict(bst_model,newdata = test_matrix)

head(p)

pred <- matrix(p,nrow = nc,ncol = length(p)/nc) %>%
  t() %>% 
  data.frame() %>%
  mutate(label=test_Label,max_prob=max.col(.,"last")-1)

head(pred)

table(Predicted=pred$max_prob,Actual=pred$label)

####################################################################
################### More XGboost Parameters#########################
####################################################################

bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = 104,
                       watchlist = watch_list,
                       eta=0.01,
                       max.depth=3,
                       gamma=0,
                       subsample=1,
                       colsample_bytree=1,
                       missing=NA,
                       seed=333)

# Training & Test  Error plot
e <- data.frame(bst_model$evaluation_log)
plot(e$iter,e$train_mlogloss,col='blue')
lines(e$iter,e$test_mlogloss,col='red')

min(e$test_mlogloss)
e[e$test_mlogloss ==  0.625217,]

# Feature Importance 
imp <- xgb.importance(colnames(train_matrix),bst_model)
print(imp)
xgb.plot.importance(imp)

# Prediction & Confusion matrix - test Data
p <- predict(bst_model,newdata = test_matrix)

head(p)

pred <- matrix(p,nrow = nc,ncol = length(p)/nc) %>%
  t() %>% 
  data.frame() %>%
  mutate(label=test_Label,max_prob=max.col(.,"last")-1)

head(pred)

table(Predicted=pred$max_prob,Actual=pred$label)

53/75  # it is good improvement with 104 iteration compared to previous one
45+7
52/75  # it is not good with 1000 iteration
