# What is multicollinearity?
# -> Moderate to high intercorrelations among independent variables

# What problems multicollinearity creates?
# ->1. If two independent variables contain essentially same information to a large extent, one gains little by
# using both in the regression model.
# 2. Multicollinearity leads to unstable estimates as it tends to increase the variances of regression coefficients.

# How do we assess presence or absence of multicollinearity?
# -> One way is to obtain Variance Inflation Factor (VIF)
# 1. VIF > 10 indicates presence of multicollinearity
# 2. VIF < 10 we do not have multicollinearity problem?

# What is the solution?
# --> One way is to keep only one of the two independent variables that are highly correlated in the regression model.

install.packages("faraway")
library(faraway)

data("divusa")
head(divusa)
?divusa

# Read Data
mydata <- data.frame(divusa[,-1])
head(mydata)

round(cor(mydata),2)

mymodel <- lm(divorce~.,mydata)
mymodel
summary(mymodel)

# Check if we have multicollinearity problem or not
vif(mymodel)

# We see all values are less than 5, for sever multicollinearity problem the values should have more than 10.
# but here we see values are smaller than 5, and much smaller than 10.
# We can safly conclude that we are not having multicollinearity problem as far as this dataset is concerned.