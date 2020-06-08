
######################## Multiple Linear Regression ############
# 1. very first thing to do in this study is scatter plot
# as scatter plot will confirm whether or not there is linear 
# relation between two variables or among the other variables.
# 2. Multiple Linear Regresssion equation.
# y = b0 + b1x1 + b2x2 + .......+bnxn
#
#  b0 is Y-intercept and b1,b2,b3,bn is slop
##



# Set working Directory

setwd("D:\\from E\\ML")

# Read Data

vehicle <- read.csv("vehicle.csv",header = T,sep = ",")

head(vehicle)

pairs(vehicle[3:5])

# Multiple Linear Regression

results <- lm(lc ~ Mileage+lh,vehicle)
results

summary(results)

# Variable significance last column propability or p-value which
# tells you whether variable in the model is really contributing
# significantly or not.
# star indicates how significant variable in the model whether it is playing
# significant role or not
# 1. If it is 3 star means it is very highly significant
# 2. if it is 2 star, slightly less than 3
# 3. if it is 1 star, slightly less than 2 but still significant
# 4. if there is no star it means variable is not really playing
# significant role in the model.
# 5. if it is not playing any significant role, the natural thing will
# be to delete that variable for further analysis.
# 
# significant code : if you have a 3 star means significance level is
# 0.001 now how do you interprete this 0.001.
# suppose p-value is 0.001 
# if you do (1-p) in this case it will become (1-0.001)=0.999
# 99.9% confidence. it is very high level of confidence.
# default value is 95 % anything more than 95 % percent is really good.
# and anything less than 95% percent is not really that good.

# Final model exluding mileage variable.

results <- lm(lc ~ lh,vehicle)
results

summary(results)



reduced <- lm(lc ~ lh,vehicle)
full <- lm(lc ~ Mileage+lh,vehicle)

anova(reduced,full)

# Prediction
predict(results,data.frame(lh=10),interval = 'confidence')
