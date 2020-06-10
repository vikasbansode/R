# Read Data
apple <- read.csv(file.choose(),header = T)
str(apple)

# Build corpus
library(tm)
library(tmap)
library(wordcloud)
library(stringr)
library(tm)
library(SnowballC)

corpus <- iconv(apple$text,to="utf-8")
corpus <- Corpus(VectorSource(corpus))

inspect(corpus[1:5])

# Clean Text
# convert upper to lower
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])

# remove punctuation
corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])

# remove Stopwords

Cleanset <- tm_map(corpus,removeWords,stopwords('english'))
inspect(Cleanset[1:5])

# Remove URL

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)

Cleanset <- tm_map(Cleanset,content_transformer(removeURL))

# remove common words so that they will not  over shadow

Cleanset <- tm_map(Cleanset,removeWords,c('aapl','apple','appl'))

# Combine words like stock and stocks both are create same frequency

Cleanset <- tm_map(Cleanset,gsub,pattern='stocks',replacement='stock')

# Remove whitespace

Cleanset <- tm_map(Cleanset,stripWhitespace)

# Term Document matrix
# NOTE : Text Data like twitts is a unstructured data and to do further analysis we have to convert this into
# structure data into rows and columns, that is achieved by doing Term Document matrix

tdm <- TermDocumentMatrix(Cleanset)
tdm

tdm <- as.matrix(tdm)
tdm[1:10,1:5]

# Bar plot
w <- rowSums(tdm)
# w <- subset(w,w>=3)

barplot(w,
        las=2,
        col = rainbow(52))

# word cloud
library(wordcloud)
w <- sort(rowSums(tdm),decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),freq = w,max.words = 150,random.order = F,min.freq = 5,colors = brewer.pal(8,'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.7)

# word cloud 2
install.packages('wordcloud2')
library(wordcloud2)

w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')

head(w)

wordcloud2(w,
           size =0.3,
           shape = 'circle')

wordcloud2(w,
           size =0.3,
           shape = 'star')


wordcloud2(w,
           size =0.3,
           shape = 'star',
           rotateRatio = 0.5,
           minSize = 1)

wordcloud2(w,
           size =0.3,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)

# Letter cloud

letterCloud(w,
            word = "A",
            size=2)


letterCloud(w,
            word = "a",
            size=1)



# Sentiment Analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

apple <- read.csv(file.choose(),header = T)
tweets <- iconv(apple$text,to='utf-8')

s <- get_nrc_sentiment(tweets)
head(s)
tweets[4]

get_nrc_sentiment('COVID19Pandemic')

# COVID19Pandemic has score of 1 fear and sadness hence word is negative

get_nrc_sentiment('SARSCoV2')

# Bar plot
barplot(colSums(s),
        las=2,
        col=rainbow(10),
        ylab='Count',
        main = 'Sentiment Scores for Apple Tweets')

# you can import file before earning and after earnings report to check sentiment Analysis.
