setwd("D:\\All\\R\\textm")
dput(list.files())

readLines("pl.txt")
str(readLines("pl.txt"))

text <- paste(readLines("pl.txt"),collapse = " ")  # Read Text file
text2 <- gsub(pattern = "\\W",replace = " ",text)  # remove punctuation mark
text2 <- gsub(pattern = "\\d",replace=" ",text2)   # Remove digits
text2 <- tolower(text2) # conver to lower case

library(tm)
text2 <- removeWords(text2,stopwords()) # Remove stopwords
text2 <- gsub(pattern = "\\b[A-z]\\b{1}",replacement = " ",text2) # removing works/letter with any specific lenght in this case letter 1.
text2 <- stripWhitespace(text2)

library(stringr)
library(wordcloud)

textbag <- str_split(text2,pattern = "\\s+") # split string to individual term
class(textbag)

textbag <- unlist(textbag)
str(textbag)
textbag

# Read Positive negative terms

poswords <- scan('positive-words.txt',what = 'character',comment.char =";")
str(poswords)
negwords <- scan('negative-words.txt',what = 'character',comment.char =";")
str(negwords)

# Matching with Positive terms
match(textbag,poswords)
!is.na(match(textbag,poswords))
sum(!is.na(match(textbag,poswords)))

# Matching with negative terms

match(textbag,negwords)
!is.na(match(textbag,negwords))
sum(!is.na(match(textbag,negwords)))

# getting score 

score <- (sum(!is.na(match(textbag,poswords))) - sum(!is.na(match(textbag,negwords))))

mean(score)
sd(score)
var(score)
hist(score)

# plot words

wordcloud(textbag)
wordcloud(textbag,min.freq = 4)
wordcloud(textbag,min.freq = 4,random.order = FALSE)
wordcloud(textbag,min.freq = 4,random.order = FALSE,scale = c(3,0.5))
wordcloud(textbag,min.freq = 4,random.order = FALSE,scale = c(3,0.5),colors = rainbow(3))

