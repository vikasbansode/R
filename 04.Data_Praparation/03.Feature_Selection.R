# Feature Selection

# import pre-requsite libraries
library(Boruta)
library(mlbench)
library(caret)
library(randomForest)

# Read Data
data('Sonar')
str(Sonar)

# Feature Selection
set.seed(111)
boruta <- Boruta(Class ~.,data = Sonar,doTrace=2,maxRuns=500)
print(boruta)
plot(boruta,las=2,cex.axis=0.7)
plotImpHistory(boruta)

# Tentative Fix
bor <- TentativeRoughFix(boruta)
print(bor)
attStats(boruta)

# Data Partition
set.seed(222)
ind <- sample(2,nrow(Sonar),replace = T,prob = c(0.6,0.4))
train <- Sonar[ind==1,]
test <- Sonar[ind==2,]

# Random Forest Model
set.seed(333)
rf60 <- randomForest(Class~.,data = train)
rf60

getNonRejectedFormula(boruta)
rf41 <- randomForest(Class ~ V1 + V2 + V4 + V5 + V8 + V9 + V10 + V11 + V12 + V13 + 
                       V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + 
                       V26 + V27 + V28 + V29 + V30 + V31 + V32 + V35 + V36 + V37 + 
                       V39 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + V51 + V52 + 
                       V54 + V59,data = train)

getConfirmedFormula(boruta)
rf33 <- randomForest(Class ~ V1 + V4 + V5 + V9 + V10 + V11 + V12 + V13 + V15 + V16 + 
                       V17 + V18 + V19 + V20 + V21 + V22 + V23 + V26 + V27 + V28 + 
                       V31 + V35 + V36 + V37 + V43 + V44 + V45 + V46 + V47 + V48 + 
                       V49 + V51 + V52,data = train)

# Prediction & Confusion - Test
p <- predict(rf33,test)
p
confusionMatrix(p,test$Class)


