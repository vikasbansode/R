# Go to https://apps.twitter.com
# Register Application Program Interface (API) using your Twiter account

# Replace xxx with your infor

api_key <- 'p1GQstu0ZhPl9VrnFhDPuSYXm'
api_secret <- 'ZfQamjD8sRbZazFcWsLqzoj8hZ28ZGpzvC5RbcaKefwqk4iE5G'
access_token <- '1245330728541458432-J6juov9njmzGi0QhtM7ldqgMlsaiDl'
access_token_secret <- 'AifL0XUb4HZ7yAEzUVvGnGfeZAIpkMtUMHrLPVAWslGUA'

# Load library

library(twitteR)
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# Getting tweets

tweets <- searchTwitter('$aapl',n=10,lang = 'en')
tweets

# Save into dataframe
tweetsdf <- twListToDF(tweets)

# Export tweets data
write.csv(tweetsdf,"apple.csv",row.names = F)

# Read file

apple <- read.csv("apple.csv",header = T,sep = ",")


# Trends Locations

trend <- availableTrendLocations()
head(trend)

# Getting trends
world <- getTrends(1)
world


# User Timeline
t <- getUser('MittRomney')
t

