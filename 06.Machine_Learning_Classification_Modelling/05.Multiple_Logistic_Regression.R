
# Read Data
mydata <- read.csv("Cardiotocographic.csv",header = T,sep = ",")
str(mydata)

# Convert NSP in to factor
mydata$NSPF <- factor(mydata$NSP)
mydata$out <- relevel(mydata$NSPF,ref = "1")

#Develop Multinomial Logistic Regression Model
library(nnet)
mymodel <- multinom(out~LB+AC+FM,data = mydata)
summary(mymodel)

#Manual calculation
#ln <- intercept + LB - AC + FM 

#Predict

predict(mymodel,mydata[c(3,100,400),],type = 'prob')

# Misclassification error
cm <- table(predict(mymodel),mydata$NSPF)
print(cm)
1-sum(diag(cm))/sum(cm)

# z-tailed z test
z <- summary(mymodel)$coefficients/summary(mymodel)$standard.errors
p <- (1 - pnorm(abs(z),0,1)) * 2
p
