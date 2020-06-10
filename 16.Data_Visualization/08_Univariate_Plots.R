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

# Bar plot

vehicle %>%
  ggplot(aes(x=State)) +
  geom_bar() +
  coord_flip()


veh %>%
  ggplot(aes(x=State,fill=State)) +
  geom_bar()

veh %>%
  filter(fm == 1 | fm == 2) %>%
  ggplot(aes(x=State,fill=State)) +
  geom_bar() +
  facet_wrap(.~fm) +
  ggtitle('Failure in Top 3 States',
          'Failure months 1 & 2')

# Pie Chart

veh %>%
  ggplot(aes(x = State,fill = State)) +
  geom_bar(width = 1) +
  coord_polar(theta = 'x')

# Histogram

veh %>%
  ggplot(aes(x = Mileage,fill = State)) +
  geom_histogram(bindwidth = 5000,
                 color = 'blue',
                 alpha = 0.8) +
  facet_wrap(~State) +
  ggtitle('Failure mileage distribution',
          'Top 3 States')


veh %>%
  ggplot(aes(x = Mileage,fill = State)) +
  geom_histogram(bindwidth = 5000,
                 color = 'blue',
                 alpha = 0.8,
                 aes(y = ..density..)) +
  facet_wrap(~State) +
  ggtitle('Failure mileage distribution',
          'Top 3 States') +
  scale_fill_brewer(palette = 'Set1') +
  geom_density(alpha = 0.2)


veh %>% filter(fm == 2 | fm == 2) %>%
  ggplot(aes(x = Mileage,fill = State)) +
  geom_histogram(bindwidth = 2000,
                 color = 'blue',
                 alpha = 0.8,
                 aes(y = ..density..)) +
  facet_grid(State ~ fm) +
  facet_wrap(~State) +
  ggtitle('Failure mileage distribution',
          'Top 3 States') +
  scale_fill_brewer(palette = 'Set1') 

# Denstity

veh %>% filter(fm == 2 | fm == 2) %>%
  ggplot(aes(x = Mileage,fill = State)) +
  geom_density(bindwidth = 2000,
                 color = 'blue',
                 alpha = 0.8,
                 aes(y = ..density..)) +
  facet_grid(State ~ fm) +
  facet_wrap(~State) +
  ggtitle('Failure mileage distribution',
          'Top 3 States') +
  scale_fill_brewer(palette = 'Set1') 
