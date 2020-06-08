########################################################
#   Create, Test and Save a Logistic Regression Model  #
########################################################

# Use Logistic regression equation of vehicle transmission
# in the data set mtcars to estimate the probability of 
# a vehicle beging fitted with a manual transmission
# based on horsepower (hp) and (wt)
?mtcars
summary(mtcars)

# Create glm model with mtcars dataset

carsModel <- glm(formula = am~hp+wt,data = mtcars,family = binomial)

# Produce a prediction function that can use the model
manualTransmission <- function(hp,wt){
  newdata <-data.frame(hp=hp,wt=wt)
  predict(carsModel,newdata,type='response')
}

# test function locally by printing results
print(manualTransmission(120,2.8))

carsModel <- glm(formula = am ~ hp+wt,data = mtcars,family = binomial)
saveRDS(carsModel,'cars-model.rds')
