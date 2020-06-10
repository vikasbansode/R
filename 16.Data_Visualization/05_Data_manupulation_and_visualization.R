# Load libraries

library(dplyr)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
library(openintro)
library(fiftystater)
library(colorplaner)


# Set working directory

setwd("D:\\from E\\ML")

# Read Data

vehicle <- read.csv("vehicle.csv",header = T,sep = ",")

car <- as_data_frame(vehicle)
car

# Filter
car %>%
  filter(State == 'CA' | State == 'TX' | State == 'FL')

car %>%
  filter(State == 'CA', Mileage > 1000)

# Arrange

car %>%
  filter(State == 'CA' | State == 'TX' | State == 'FL') %>%
  arrange(desc(Mileage))

# Summarise

car %>%
  group_by(State) %>%
  summarise(Avg_lc = mean(lc),
            sd_lc = sd(lc),
            max_lc = max(lc),
            min_lc = min(lc),
            sum_lc = sum(lc),
            median_lc = median(lc),
            total = n()
            ) %>%
  arrange(desc(Avg_lc))

# Mutate

car %>%
  group_by(State) %>%
  mutate(cph = sum(lc)/sum(lh)) %>%
  summarise(Avg_cph = mean(cph),
            Avg_mileage = mean(Mileage)) %>%
  arrange(desc(Avg_cph))



# Visualization
# Histogram
car %>%
  filter(State == 'CA' | State == 'TX' | State == 'FL') %>%
  ggplot(aes(x=lc)) +
  geom_histogram(alpha =0.8,color='darkblue') +
  ggtitle('Labor cost in Top 3 States') +
  facet_wrap(~State)

# Density
car %>%
  filter(State == 'CA' | State == 'TX' | State == 'FL') %>%
  ggplot(aes(x=lc,fill=State)) +
  geom_density(alpha=0.5,color = 'darkblue') +
  ggtitle('Labor cost in Top 3 States')


# Scatter plot

car %>%
  filter(State == 'CA' | State == 'TX' | State == 'FL') %>%
  ggplot(aes(x=lh,y=lc)) +
  geom_point(alpha=0.5,color = 'darkblue') +
  geom_smooth() # with smooth line


car %>%
  filter(State == 'CA' | State == 'TX' | State == 'FL') %>%
  ggplot(aes(x=lh,y=lc)) +
  geom_point(alpha=0.5,color = 'darkblue') +
  geom_smooth(se=0)  # without smooth line


car %>%
  filter(State == 'CA' | State == 'TX' | State == 'FL') %>%
  ggplot(aes(x=lh,y=lc,col=State)) + # color by state
  geom_point(alpha=0.5,color = 'darkblue') + 
  geom_smooth(se=0)  # without smooth line
 

car %>%
  filter(State == 'CA' | State == 'TX' | State == 'FL') %>%
  ggplot(aes(x=lh,y=lc,col = State)) +
  geom_point(alpha=0.5,color = 'darkblue') +
  geom_smooth(se=0) + # without smooth line
  facet_wrap(~State) # Seperate plot

# Bar plot

new <- car %>%
          group_by(State) %>%
          mutate(cph = sum(lc)/sum(lh)) %>%
          summarise(Avg_cph = mean(cph),
                    Avg_mileage = mean(Mileage)) %>%
          arrange(desc(Avg_cph))

ggplot(new,aes(x=State,y=Avg_cph)) +
  geom_col()
  

ggplot(new,aes(x=State,y=Avg_cph)) +
  geom_col() +
  coord_flip()

ggplot(new,aes(x=State,y=Avg_cph,fill=State)) +
  geom_col() +
  coord_flip()

ggplot(new,aes(x=State,y=Avg_cph,fill=State)) +
  geom_col() +
  coord_flip() +
  ggtitle('Cost per hour in 50 Statet')


# Box plot

car %>%
  group_by(State) %>%
  filter(n() > 40) %>% # more than 40 car failure in the state
  ggplot(aes(x=State,y=Mileage,col=State)) +
  geom_boxplot()

# Map-1
# Data Preparation
new <- car %>%
  group_by(State) %>%
  summarise(total = n(),
            Avg_mileage = mean(Mileage))

colnames(new) <- c('region','value','mileage')
new
new$region <- abbr2state(new$region) # abbr2State from pkg opern intro abbrevation convert into full name

# for ex: AL to Alaska

new$region <- tolower(new$region)
new <- new[-1,]

state_choropleth(new,
                 title = 'Car Failures in US',
                 legend = 'Number of Failures')

# Map - 2

p <- ggplot(new,aes(map_id = region)) +
      geom_map(aes(fill=mileage),map = fifty_states) +
      expand_limits(x=fifty_states$long,y=fifty_states$lat) +
      coord_map() +
      scale_x_continuous(breaks = NULL) +
      scale_y_continuous(breaks = NULL) +
      labs(x="",y="") +
      theme(legend.position = "bottom",
            panel.background = element_blank())

p + fifty_states_inset_boxes()
p + aes(fill2 = value ) + scale_fill_colorplane() +
  theme(legend.position = 'right') +
  ggtitle('Geo Map for Failures and Average Mileage ')
