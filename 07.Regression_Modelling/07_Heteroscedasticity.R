# Heteroscedasticity

# Set working Directory
setwd("D:\\from E\\ML")

# get working Directory

getwd()

# Install pre-requisite library
install.packages('AER')

# Import library
library(AER)

# Read Data
hp_data <- read.csv("houseprices.csv",header = TRUE,sep = ",")
summary(hp_data)

# Build Regression Model

reg1 <- lm(price~lotsize+sqrft+bdrms,data = hp_data)
reg1_sm <- summary(reg1)
print(reg1_sm)

# How woud you investigate whether heteroscedasticity is a problem
# in this regression.

# Residual Plots
par(mfrow = c(1,3))
plot(reg1$residuals~hp_data$lotsize,pch=16)
plot(reg1$residuals~hp_data$sqrft,pch=16)
plot(reg1$residuals~hp_data$bdrms,pch=16)
par(mfrow=c(1,1))

# 1. hpi = alpha + alpha1.Xi+alpha2.X2+alpha3.X3+Ui --> Esitmated by OLS
# 2. Predicted_Ui^2 = coef0+coef1.X1+coef2.X2+coef3.X2+Vi --> R-square
# 3. LM=R-Square = 88.R-square
# H0 : no heteroscadacity
# H1 : coefi not equal to 0, i=1,2 or 3 the heteroscadicity
# Reject H0 p-value < alpha =0.01

# White test

bptest(reg1)
bptest(reg1,~lotsize+I(lotsize^2)+sqrft+I(sqrft^2)+bdrms+I(bdrms^2),data = hp_data)

# White standard Errors
coeftest(reg1,vcov=vcovHC(reg1,type = 'HC1'))

#
install.packages("lmtest")
library(lmtest)
# Step 1: Install package lmtest
# Step 2 : Run a suitable regression model
# step 3: bptest(model1)
# H0: There is constant variance or homoscedasticity in residual.
# (P-value is greater than 0.05: Accept H0) it means there is no Heteroscedasticity problem in this dataset
