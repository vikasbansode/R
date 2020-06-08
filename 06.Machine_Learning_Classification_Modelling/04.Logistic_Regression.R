#Read data file

mydata <- read.csv("binary.csv",header = T,sep = ",")

# Explore

str(mydata)

#Convert admit variable into factor
mydata$admit <- as.factor(mydata$admit)
mydata$rank <- as.factor(mydata$rank)
str(mydata)

# Two-way table of factor variables and make sure we don't have any cell with zero value

xtabs(~admit+rank,data = mydata)

# Data Partition - train (80%) & test (20%)
set.seed(1234)
ind <- sample(2,nrow(mydata),replace = T,prob = c(0.8,0.2))
train <- mydata[ind==1,]
test <- mydata[ind==2,]

# logistic Regression Model
mymodel <- glm(admit ~ gpa+rank,data = train,family = 'binomial')
summary(mymodel)

# Prediction
p1 <- predict(mymodel,train,type = 'response')
head(p1)
head(train)

# how to calculate predictions or p1 manually
 y <-  -4.7270 + (1.3735*3.61)+(1*-1.1645)
exp(y)/(1+exp(y)) 

# Misclassification error - train data
pred1 <- ifelse(p1>0.5,1,0)
tab1 <- table(Predicted=pred1,Actual=train$admit)
1-sum(diag(tab1))/sum(tab1)

# Misclassification error - test data
p2 <- predict(mymodel,test,type = 'response')
pred2 <- ifelse(p2>0.5,1,0)
tab2 <- table(Predicted=pred2,Actual=test$admit)

1-sum(diag(tab2))/sum(tab2)

# Goodness -of-fit test
with(mymodel,pchisq(null.deviance - deviance,df.null-df.residual,lower.tail = F))
