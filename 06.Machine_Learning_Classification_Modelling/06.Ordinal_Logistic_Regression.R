
# Set working Directory

setwd("D:\\from E\\ML")

# 1. Read Data

data <- read.csv("Cardiotocographic.csv",header = TRUE,sep = ",")
str(data)

# 2. Convert NSP integer variables to Ordinal Variable

data$NSP <- as.ordered(data$NSP)
data$Tendency <- as.factor(data$Tendency)
str(data)
summary(data)
xtabs(~NSP+Tendency,data)

# 3. Data Partition

ind <- sample(2,nrow(data),replace = TRUE,prob = c(0.8,0.2))
train <- data[ind==1,]
test <- data[ind==2,]

# 4. Ordinal Logistic Regression Or Proportional Odds Logistic Regression

library(MASS)
model <- polr(NSP~ .-Max-LB-MSTV-Nmax-Nzeros-Median-FM,train,Hess = TRUE)
summary(model)

# 5. P-value Calculation

(ctable <- coef(summary(model)))
p <- pnorm(abs(ctable[,"t value"]),lower.tail = FALSE) * 2
(ctable <- cbind(ctable,"P value" = p))

# 6. Prediction

pred <- predict(model,train)
print(pred,digits = 3)

# 7. Confusion Matrix & Error for Training Data
(tab <- table(pred,train$NSP))
1-sum(diag(tab))/sum(tab)

# 8. Confusion Matrix & Error for Testing Data
pred1 <- predict(model,test)

(tab1 <- table(pred1,test$NSP))
1-sum(diag(tab1))/sum(tab1)

# 9. End

0.1347725 - 0.131068

# There is no major significant difference in training and testing data
# Hence model looks good.
