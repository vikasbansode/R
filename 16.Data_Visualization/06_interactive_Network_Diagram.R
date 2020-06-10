# Set working directory

setwd("D:\\from E\\R")

# Read Data

networkdata <- read.csv("networkdata2.csv",header = T,sep = ",")

# Interactive network diagram using networkD3

library(networkD3)

simpleNetwork(networkdata,fontSize = 16,
              nodeColour = 'black')
            
simpleNetwork(networkdata,fontSize = 16,
              nodeColour = 'black',
              zoom = T)

simpleNetwork(networkdata,fontSize = 16,
              nodeColour = 'black',
              zoom = T,
              opacity = 0.1)

simpleNetwork(networkdata,fontSize = 16,
              nodeColour = 'black',
              zoom = T,
              opacity = 0.9)
