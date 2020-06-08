install.packages("vosonSML")

library(magrittr)
library(vosonSML)

apikey <- "AIzaSyAY4bOpji2WXeaYG51cmrF7gwQiX1L9sF0"

myYoutubeVideoIs <- GetYoutubeVideoIDs(
  c("5eDqRysaico","dJclNIN-TPo"))

youtubeData <- Authenticate("youtube",apikey) %>%
  Collect(videoIDs = myYoutubeVideoIs,maxComment =1000)

str(youtubeData)
setwd("D:\\from E\\Textmining")


activityNetwork <- youtubeData %>% Create("activity") %>%
  AddText(youtubeData)

activityGraph <- activityNetwork %>% Graph()

actorGraph <- youtubeData %>%
  Create("actor") %>% AddText(youtubeData) %>% Graph()



write.csv(youtubeData,file = "ytb.csv",row.names = F)

# Read YouTube data file
data <- read.csv(file.choose(),header = T)
str(data)
# data <- data[data$ReplyToAnotherUser != FALSE,]
# y <- data.frame(data$User, data$ReplyToAnotherUser) # This columns not pulled in data as we don't have permission for others account
# Create User Network
library(igraph)
net <- graph.data.frame(y,directed = T)
net <- simplify(net)
V(net)
E(net)
V(net)$label <- v(net)$name
v(net)$degree <- degree(net) # degree is basically number of connections

# Historgram of node degree
hist(V(net)$degree,
     col = 'green',
     main = 'Histogram of the Node Degree',
     ylab = 'Frequency',
     xlab = 'Degree of Vertices')

# Network diagram

plot(net,
     vertex.size = 0.2 * V(net)$degree,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.02*V(net)$degree)


# Sentiment Analysis

library(syuzhet)
comments <- iconv(data$Comment,to='utf8')

# Obtain sentiment scores

s <- get_nrc_sentiment(comments)
head(s)
s$neutral <- ifelse(s$negative+s$positive==0,1,0)
head(s)
comments[1]
comments[2]
comments[3]

# Bar plot

barplot(100*colSums(s)/sum(s),las=2,col=rainbow(10),ylab='Percentage',
        main='Sentiment scores for Youtube comments')
