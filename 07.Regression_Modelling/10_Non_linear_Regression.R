rm(list = ls())

# Set working Directory

setwd("D:\\Economatrics")

# Polynomial Regression is a special case of Linear Regression
# where the relationship between x and Y is modeled usisng a Polynomial,
# rather than a  line.
# It can be used when the relationship between X and Y is non-linear,although
# this is still considered to be a special case of Multiple Linear Regression

# Read Data

LungCapData <- read.csv("LungCapData2.csv",header = T,sep = ",")
attach(LungCapData)
summary(LungCapData)

# make a plot of LungCap vs. Height

plot(Height,LungCap,main = "Polynomial Regression",las=1)

# now, let's fit a linear Regression
model1 <- lm(LungCap ~ Height)

# and add the line to the plot... make it thick and red...
abline(model1,lwd=3,col="red")

# We can also use Residual plots to help with assessing linearity and checking
# other model assumptions

# first, the WRONG WAY ....
model2 <- lm(LungCap~Height+Height^2)
summary(model2)

# first, the RIGTH WAY
model3 <- lm(LungCap~Height+I(Height^2))
summary(model3)

# Alternative ways to include Height:
# 1: Create a new variable called Height^2 and include
# this variable into the model

#Or, Create Height^2, and then include this in model...it's the same!
HeightSquare <- Height^2
model3again <- lm(LungCap~Height+HeightSquare)
summary(model3again)

# Or, use the "poly" command...it's the same!
model3againagain <- lm(LungCap~poly(Height,degree = 2,raw = T))
summary(model3againagain)

# now,let's add this to the plot, using a thick blue line
lines(smooth.spline(Height,predict(model3)),col='blue',lwd=3)

# test if the model including Height^2 i signif. better than one without
# using the partial F-Test
anova(model2,model3)

# from the F-test we can see such a small p-value we will reject the null
# hypothesis and conclude that we have evidence to believe the model
# including height squared provides a Statistically significantly better fit
# than the model without Height squared.

# try fitting a model that includes Height^3 as well
model4 <- lm(LungCap ~Height+I(Height^2)+I(Height^3))
summary(model4)

# Now,let's add this model to the plot, using a thick dashed green line
lines(smooth.spline(Height,predict(model4)),col="green",lwd=3,lty=3)

# and, let's add a lenged to clarify the lines
legend(46,15,legend = c("model1:linear","model3:poly x^2","model:poly"),
                        col=c("red","blue","green"),lty=c(1,1,3),lwd=3,bty="n",cex=0.9)
