library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(gganimate)
library(magrittr)

setwd("D:\\from E\\ML")
# Data 

data <- read.csv("latestdata.csv",header = T,sep = ",")
head(data$date_confirmation)

# Handling dates
datanew <- data %>%
  mutate(date_confirmation = dmy(date_confirmation))

head(datanew$date_confirmation)

# Animated time series plot

datanew %>% group_by(date_confirmation) %>%
  summarise(count = n()) %>%
  mutate(cuml = cumsum(count)) %>%
  ggplot(aes(x = date_confirmation,y = cuml)) +
  geom_line(color = 'red') +
  geom_point(size = 1.5) +
  theme_bw() +
  ggtitle('Daily Cumulative Cases') +
  transition_reveal(cuml)

# solid Animated time series plot

datanew %>% group_by(date_confirmation) %>%
  summarise(count = n()) %>%
  mutate(cuml = cumsum(count)) %>%
  ggplot(aes(x = date_confirmation,y = cuml)) +
  geom_line(color = 'red') +
  geom_point(size = 1.5) +
  geom_area(fill = 'red') +
  theme_bw() +
  ggtitle('Daily Cumulative Cases') +
  transition_reveal(cuml)
