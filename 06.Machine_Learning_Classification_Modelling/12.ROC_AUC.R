getwd()

# Read Data file
binary <- read.csv("binary.csv",header = T,sep = ",")
str(binary)

# Logistic Regression Model

library(nnet)

mymodel <- multinom(admit~.,data = binary)

# misclassification Rate

p <- predict(mymodel,binary)
tab <- table(p,binary$admit)
tab

1-sum(diag(tab))/sum(tab)

table(binary$admit)

# Model Performance Evaluation
library(ROCR)
pred <- predict(mymodel,binary,type = 'prob')
head(pred)
head(binary)

hist(pred)

pred <- prediction(pred,binary$admit)
eval <- performance(pred,'acc')
plot(eval)
abline(h=0.71,v=0.45)

# Identify Best values
max <- which.max(slot(eval,"y.values")[[1]])
max
acc <- slot(eval,"y.values")[[1]][max]
acc
cut <- slot(eval,"x.values")[[1]][max]
cut
print(c(Accuracy=acc,Cutoff=cut))

# Reciever Operating Characteristic (ROC) curve

pred <- prediction(pred,binary$admit)
roc <- performance(pred,"tpr","fpr")

plot(roc,colorize=T,main="ROC Curve",ylab="sensitivity",
     xlab="1-Specificity")
abline(a=0,b=1)

# if model is performing worse then curve will be the under the line
# if it is good then it will above the line


# Area Under the Curve (AUC) curve

auc <- performance(pred,"auc")
auc <- unlist(slot(auc,"y.values" ))
auc <- round(auc,4)
legend(.6,.2,auc,title="AUC",cex=1.2)
