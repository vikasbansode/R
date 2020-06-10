
# Choose a directory

file.choose()

# create a path

folder <- "D:\\All\\R\\textm\\corpus"
folder

# List files

list.files(path = folder)

# list files by pattern

list.files(path = folder,pattern = "*.txt")

# create variabl for pattern files

filelist <- list.files(path = folder,pattern = "*.txt")
filelist

# Seperate the files with path

paste(folder,"\\",filelist)
paste(folder,"\\",filelist,sep = "")

# it is useful when you have lots of documents for Analysis

filelist <- paste(folder,"\\",filelist,sep = "")
filelist

# Use readlines function to read text file

a <- lapply(filelist,FUN = readLines)

# Collapsing into one character element

corpus <- lapply(a,FUN = paste,collapse=" ")
corpus

# cleaning Corpus

corpus2 <- gsub(pattern = "\\W",replace = " ",corpus)  # remove punctuation mark
corpus2 <- gsub(pattern = "\\d",replace=" ",corpus2)   # Remove digits
corpus2 <- tolower(corpus2) # conver to lower case

library(tm)
library(stringr)
corpus2 <- removeWords(corpus2,stopwords()) # Remove stopwords
corpus2 <- gsub(pattern = "\\b[A-z]\\b{1}",replacement = " ",corpus2) # removing works/letter with any specific lenght in this case letter 1.
corpus2 <- stripWhitespace(corpus2)
corpus2 <- str_trim(corpus2)
corpus2

# Visualization
library(wordcloud)
wordcloud(corpus2)
wordcloud(corpus2,min.freq = 4)
wordcloud(corpus2,min.freq = 4,random.order = FALSE)
wordcloud(corpus2,min.freq = 4,random.order = FALSE,col=rainbow(3))

comparison.cloud(corpus2)

corpus3 <- Corpus(VectorSource(corpus2))
corpus3
x11()

# Create a Term Documentry Matrix
tdm <- TermDocumentMatrix(corpus3)
tdm

# Convert tdm into matrix

m <- as.matrix(tdm)

colnames(m)

# name meaningful columnnames
colnames(m) <- c("CR","JUVY","TOT")
m

# Do a comparison

comparison.cloud(m)


