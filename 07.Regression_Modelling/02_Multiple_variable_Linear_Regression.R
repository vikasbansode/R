rm(list = ls())

# 1. Set working directory

setwd("D:\\from E\\ML")

# 2. Read Data
df <- read.csv("2homeprices.csv",header = T,sep = ",")
df

# 3. Preprocess data as we see df have missing values


median_bedrooms <- floor(median(df$bedrooms,na.rm = TRUE))
median_bedrooms
df[is.na(df)] <- median_bedrooms
df

# build the model
names(df)

mlr <- lm(price~area+bedrooms+age,data = df)
mlr
summary(mlr)

mlr$coefficients
mlr$residuals

# predict the model

predict(mlr,data.frame("area"=3000,"bedrooms"=3,"age"=40))

# 