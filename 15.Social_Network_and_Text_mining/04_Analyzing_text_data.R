library(tm)
library(SnowballC)
install.packages("pdftools")
library(pdftools)
files <- list.files(pattern = "pdf$")
files
options(header=FALSE,stringsAsFactors = FALSE,FileEncoding='latin1')
setwd("D:\\All\\github\\R\\15.Social_Network_and_Text_mining")
download.file("http://textfiles.com/news/092793.txt",destfile = "RAI2009.txt")

library(tm)
corp <- Corpus(URISource(files),
               readerControl = list(reader = readPDF))

# Getting pdf data to text

library(pdftools)
library(tidyverse)

UC_text <- pdf_text('xgboost.pdf') %>%
  readr::read_lines()
  
writUC_text

write.csv(UC_text,"UC.txt")

# Read data


text <- pdf_text('xgboost.pdf') %>%
  readr::read_lines()
corpus <- VCorpus(VectorSource(text))
text

inspect(corpus)

# Clean-up text file

corpus <- tm_map(corpus,tolower) 
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,removeNumbers)
cleanset <- tm_map(corpus,removeWords,stopwords('english'))
cleanset <- tm_map(cleanset,removeWords,c('fig','per'))
cleanset <- tm_map(cleanset,gsub,pattern="claims",replacement="claim")
cleanset <- tm_map(corpus,stripWhitespace)

inspect(cleanset)

# Needed for new tw 0.6.0

cleanset <- tm_map(cleanset,PlainTextDocument)

# Build term document matrix

dtm <- TermDocumentMatrix(cleanset,control = list(minwordLength=c(1,Inf)))

# Inspect frequent words

findFreqTerms(dtm,lowfreq = 2)

# Bar plot

termFrequency <- rowSums(as.matrix(dtm))
termFrequency <- subset(termFrequency,termFrequency>=15)

library(ggplot2)
barplot(termFrequency,las=2,col=rainbow(20))

# Word cloud
library(wordcloud)
m <- as.matrix(dtm)

# Calculate the frequency of words and sort it descendingly by frequency

wordFreq <- sort(rowSums(m),decreasing = TRUE)

# Word cloud
set.seed(375)

grayLevels <- gray((wordFreq+10)/(max(wordFreq)+10))

# With gray levels

wordcloud(words = names(wordFreq),freq = wordFreq,max.words = 100,min.freq = 5,random.order = F,colors = grayLevels )

# With COlors

wordcloud(words = names(wordFreq),freq = wordFreq,max.words = 100,min.freq = 5,random.order = F,colors = rainbow(20))

# Different type of colors

wordcloud(words = names(wordFreq),freq = wordFreq,max.words = 100,min.freq = 5,random.order = F,colors = brewer.pal(6,"Dark2"))

wordcloud(words = names(wordFreq),scale=c(6,.1),freq = wordFreq,max.words = 100,min.freq = 5,random.order = F,colors = brewer.pal(6,"Dark2"))

# Roating words

wordcloud(words = names(wordFreq),rot.per=0.2,scale=c(6,.1),freq = wordFreq,max.words = 100,min.freq = 5,random.order = F,colors = brewer.pal(6,"Dark2"))



