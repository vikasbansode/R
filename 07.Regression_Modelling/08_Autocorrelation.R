############# AUTOCORRELATION ##############
# Autocorrelation is a measure of the correlation between the lagged values of 
# a time series. For example, r1 is the autocorrelation between yt and yt-1;
# similarly, r2 is the autocorrelation between yt and yt-2. 
# This can be summarized in the following formula:
# rk = sumT(superscript) t=k+1 (subscript)(yt-y_pred)(yt-k-y_pred)/sum t(suprscript) t=1(subscript)(yt-y_pred)^2
# In the preceding formula, T is the length of the time series


# set working directory

setwd("D:\\from E\\ML")

# Import Packages
library(car)
library(astsa) #To use lag1.plot()
library(orcutt)

# Read Data 
sale <- read.table("Autocorrelation.csv",header = T,sep = ",")
attach(sale)
names(sale)
sale

#Understanding Autocorrelation
Model=lm(Sales~Adcost,data=sale)
plot(Model)

e=Model$residuals
#Telling R to residuals are in time order.
e=ts(e) 
e
plot(e)

#Autocorrelatiofunction
Auto=acf(e,xlim=c(0,10))

Auto$acf
lag1.plot(e,1)

#Durbin Watson test for autocorrelation
DWT=durbinWatsonTest(lm(Sales~Adcost))#Two sides
DWT

PositiveDWT=durbinWatsonTest(lm(Sales~Adcost,data=sale),alternative="positive")
PositiveDWT

NegativeDWT=durbinWatsonTest(lm(Sales~Adcost,data=sale),alternative="negative")
NegativeDWT

#Cochrane-Orcutt Method
Model=lm(Sales~Adcost,data=sale)
CO=cochrane.orcutt(Model)
# it calculates the original beta values,not transformed beta values(b/(1-rho))
CO
