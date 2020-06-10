# Grammar of Graphics

# Data
# Aesthetics
# Geometry
# Facets
# Statistics
# Coordinates
# Themes


# library
library(dplyr)
library(ggplot2)
library(ggExtra)

# Set working directory

setwd("D:\\from E\\ML")

# Read Data

vehicle <- read.csv("vehicle.csv",header = T,sep = ",")
str(vehicle)

veh <- vehicle %>%
  filter(State == 'CA' | State == 'TX' | State == 'FL' )


# Using all 7 elements of grammar of graphics

veh %>%   #----------------------------------------------------># 1. Data
  ggplot(aes(x =lh,y =lc )) + #--------------------------------># 2. Aesthetics
  geom_point() + #---------------------------------------------># 3. Geometry/ Geometric shape
  facet_wrap(~ State) + #--------------------------------------># 4. Facets
  ggtitle("Labor hours vs labor cost",subtitle = "Source: vehicle.csv") +
  geom_smooth(method = "lm",col = 'red') + #-------------------># 5. Statistics : used linear method
  scale_x_continuous("Labor Hours",limits = c(0,40)) +
  scale_y_continuous("Labor Cost in $", limits = c(0,3000)) +#-># 6. Coordinates
  theme_bw()#--------------------------------------------------># 7. Theme
  

