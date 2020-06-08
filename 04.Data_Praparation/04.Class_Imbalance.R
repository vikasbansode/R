
# Set working Directory

Setwd("D:\\from E\\ML")

# Get working Directory

getwd()

# Read Data

data <- read.csv("binary.csv",header = TRUE,sep = ",")
str(data)

# convert amdit to factor variables

data$admit <- as.factor(data$admit)
summary(data)
prop.table(table(data$admit))
barplot(prop.table(table(data$admit)),col = rainbow(2),ylim=c(0,0.7),main = "Class Distribution")

####################################################################################################################################
# We see 2/3 of data belong to 0 and 1/3 belong to 1 so there is a big difference in amount of data                                #
# available for the two classes so that's why we say class imbalance because 1 class data is much lower and 0 class is higher      #
# when we develop prediction  model based on such data what will happen is so that predictive model will be dominated              #  
# by contribution from data in 0 class.                                                                                            #
# Accuracy of the model will be better when predicting 0 class vs 1 class but there are situation where we may be more interested  #
# in may be predicting 1 more accuratly compared to 0.So this is called class imbalance problem.                                   #
####################################################################################################################################
 
# Data Partition

set.seed(123)
ind <- sample(2,nrow(data),replace = TRUE,prob = c(0.7,0.3))
train <- data[ind ==1,]
test <- data[ind==2,]

# Data for Developing Predictive Model

table(train$admit)
prop.table(table(train$admit))
summary(train)

# Predictive Model (Random Forest)

library(randomForest)
rftrain <- randomForest(admit~.,data = train)

# Predictive Model Evaluation with test data
library(caret)
library(e1071)
confusionMatrix(predict(rftrain,test),test$admit,positive = '1')

#          Reference
#Prediction  0  1
#          0 70 22
#          1 15  8

# Reference is Actual values
# Prediction is prediction
# Model predicted 70 applicants not admited and 8 are admited so this is correct prediction
# 21 and 14 are incorrect prediction
# Diagnal are correct prediction and off-diagnal are incorrect prediction

70+8    # Dianal 
78/115  # 78/ total no of test i.e. 115 This will give accuracy
# Accuracy : 0.6783
# No Information Rate : 0.7391 --> This is the largetst proportion of the observed class.
70+15
85/115
# based on above information model is not good for prediction
# Sensitivity : 0.26667
22+8
8/30
## Sensitivity : 0.26667
70+15
70/85 # Specificity : 0.82353
# we can see here sensitivity is very low and specificity is high and one of the reason of this wide difference is that 
# we have few datapoints for class 1 but much more for class 0. and 0 is dominating the model
# if you want predict 1 correctly then, 30% sensitivity is too bad, its very low. what can be done to improve the situation.
# one of the way is we do this by going for oversampling

#############################################################################################################################

install.packages('ROSE')
library(ROSE)
# ROSE -> RANDOMLY OVER SAMPLING EXAMPLES
188*2
over <- ovun.sample(admit~.,data = train,method = "over",N=372)$data
table(over$admit)
summary(over)

# Predictive Model (Random Forest on Over)

library(randomForest)
rfover <- randomForest(admit~.,data = over)

#Predictive Model Evaluation with test Data

confusionMatrix(predict(rfover,test),test$admit,positive = '1')


# lets do Under sampling

under <- ovun.sample(admit~.,data=train,method = "under",N=194)$data
97*2
table(under$admit)

# Predictive Model (Random Forest on under)

rfunder <- randomForest(admit~.,data = under)

#Predictive Model Evaluation with test Data

confusionMatrix(predict(rfunder,test),test$admit,positive = '1')

# try both under and Over sampling

both <- ovun.sample(admit~.,data = train,method = 'both',p=0.5,seed = 222,N=285)$data
# N=285 because out train data has 285 observation
table(both$admit)

#Predictive Model on both
rfboth <- randomForest(admit~.,data = both)

#Predictive Model Evaluation with Test Data
confusionMatrix(predict(rfboth,test),test$admit,positive = '1')


rose <- ROSE(admit~.,data = train,N=500,seed = 111)$data # This is creating synthetic data
table(rose$admit)
summary(rose)

rfrose <- randomForest(admit~.,data = rose)
confusionMatrix(predict(rfrose,test),test$admit,positive = '1')
