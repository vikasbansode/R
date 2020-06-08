library(car)
cars <- mtcars
head(cars)
fit <- lm(mpg~disp+hp+wt+drat,data = cars)


# Outliers

outlierTest(fit) # Bonferonni p-value for most extreme obs
qqPlot(fit,main="QQPLOT") #qq plot for studentized resid
leveragePlots(fit)  # leverage plots

# Fixing outlier -> remove outliers or normalize data



#Non-normality

# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(fit)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)

# Non-constant Error Variance

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)
#
bptest(fit)
# plot studentized residuals vs. fitted values
spreadLevelPlot(fit)

# To fix the heteroskedasticity -> use box cox transforamtion

# Multi-collinearity
# Evaluate Collinearity
vif(fit) # variance inflation factors
sqrt(vif(fit)) > 2 # problem?

# To fix the multi-collinearity problem use remove variables

# Nonlinearity
# Evaluate Nonlinearity
# component + residual plot
crPlots(fit)
# Ceres plots
ceresPlots(fit)

# to fix the non-linearity problem

# Non-independence of Errors
# Test for Autocorrelated Errors
durbinWatsonTest(fit) # DW statistics is less than 2.0 hence we can 
# conclude there is no autocorrelation
durbinWatsonTest(fit, max.lag = 4)

acf(fit$residuals)

library(lmtest)
dwtest(fit) # DW value is < 2.0 hence we accept Ha, can conclude there is
# no autocorrelation in this model
bgtest(fit) # used for higher autocorrelation, p-value is very less hence 
# we can conclude there is no autocorrelation.
bgtest(fit,order = 2)




install.packages("sandwich")
library(sandwich)

coeftest(fit, vcov = vcovHC(fit, type = "HC1"))

