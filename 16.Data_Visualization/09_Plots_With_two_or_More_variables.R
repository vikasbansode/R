
# Plots with 2+ Variables
# Box plot
# Violin chart
# Dot plot
# Scatter plot
# 2D Density plot
# Polar Plot
# Marginal Plot


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

# Box plot

veh %>%
  ggplot(aes(x = State, y = Mileage, fill = State)) +
  geom_boxplot() +
  scale_fill_manual(values = c('grey','grey','pink'),
                    guide= F)


veh %>% filter(fm == 1 | fm == 2) %>%
  ggplot(aes(x = interaction(State,fm), y = Mileage)) +
  geom_boxplot()


# Violin plot

veh %>% ggplot(aes(x = State,
                   y = Mileage,
                   fill = State)) +
  geom_violin(adjust = 0.5)


# Dot plot

veh_avg <- vehicle %>%
  group_by(State) %>%
  summarise(avg = mean(Mileage))

veh_avg %>% ggplot(aes(x = avg,y = State)) +
  geom_point()


veh_avg %>% ggplot(aes(x = avg,y = reorder(State,avg))) +
  geom_point()


veh_avg %>% ggplot(aes(x = avg,y = reorder(State,avg))) +
  geom_point(color = 'red') +
  scale_y_discrete('State') +
  theme_bw()


# Scatter plot
veh %>% ggplot(aes(x = fm,
                   y = Mileage,
                   color = State)) +
  geom_point() +
  stat_smooth() +
  facet_wrap(~State)

# 2D density plot

veh %>%
  ggplot(aes(x = lh, y=lc)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  stat_density2d(aes(fill = ..density..),
                 geom = 'raster',
                 contour = F)

# Polar Plot

veh %>%
  ggplot(aes(x = Mileage, fill = State)) +
  geom_histogram(binwidth = 5000,
                 color = 'blue',
                 alpha = 0.8) +
  coord_polar() +
  facet_wrap(~State)
 

# Marginal plot

p <- veh %>% ggplot(aes(x = lh, y = lc )) +
        geom_point() +
        facet_wrap(~ State) +
        ggtitle("Labor hours vs Labor cost",
                subtitle = "Source: Vehicle.csv") +
        geom_smooth(method = 'lm',
                    col = 'red') +
        scale_x_continuous("Labor Hours",
                           limits = c(0,40)) +
        scale_y_continuous("Labor Cost in $",
                           limits = c(0,3000)) +
        theme_bw()
        

ggMarginal(p, type="histogram")
ggMarginal(p, type="boxplot")
ggMarginal(p, type="density",fill='pink')



