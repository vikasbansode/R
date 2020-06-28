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
text2 <- gsub(pattern = "\\b[A-z]\\b{1}",replacement = " ",text2) # removing words/letter with any specific lenght in this case letter 1.
text2 <- stripWhitespace(text2)


# Basic str_split

a <- c("Hello","world","name","is","R.")
a

b <- paste(a,collapse = " ")
b

str_split(b,pattern = "\\s+")

str(str_split(b,pattern = "\\s+"))
