require(gbm)
require(MASS)#package with the boston housing dataset

#separating training and test data
train=sample(1:506,size=374)


# Build a Model
Boston.boost=gbm(medv ~ . ,data = Boston[train,],distribution = "gaussian",n.trees = 10000,
                 shrinkage = 0.01, interaction.depth = 4)
Boston.boost

#Summary gives a table of Variable Importance and a plot of Variable Importance
summary(Boston.boost)

# Plotting the Partial Dependence Plot

#Plot of Response variable with lstat variable
plot(Boston.boost,i="lstat") 
#Inverse relation with lstat variable

plot(Boston.boost,i="rm") 
#as the average number of rooms increases the the price increases

cor(Boston$lstat,Boston$medv)#negetive correlation coeff-r

cor(Boston$rm,Boston$medv)#positive correlation coeff-r

# Prediction on Test Set
n.trees = seq(from=100 ,to=10000, by=100) #no of trees-a vector of 100 values 

#Generating a Prediction matrix for each Tree
predmatrix<-predict(Boston.boost,Boston[-train,],n.trees = n.trees)
dim(predmatrix) #dimentions of the Prediction Matrix

#Calculating The Mean squared Test Error
test.error<-with(Boston[-train,],apply( (predmatrix-medv)^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged

#Plotting the test error vs number of trees

plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

#adding the RandomForests Minimum Error line trained on same data and similar parameters
abline(h = min(test.error),col="red") #test.err is the test error of a Random forest fitted on same data
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)
