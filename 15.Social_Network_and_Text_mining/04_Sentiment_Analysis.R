# Read positive and negative terms

setwd("D:\\All\\R\\textm")

opinion.lexicon.pos <- scan('positive-words.txt',what = 'character',comment.char =";")
opinion.lexicon.neg <- scan('negative-words.txt',what = 'character',comment.char =";")

# Sentiment Analysis

jj <- str_split(corpus2,pattern = "\\s+")

# Positve corpus score

lapply(jj,function(x){sum(!is.na(match(x,opinion.lexicon.pos)))})

# negative corpus score

lapply(jj,function(x){sum(!is.na(match(x,opinion.lexicon.neg)))})


# Final Corpus score

lapply(jj,function(x){sum(!is.na(match(x,opinion.lexicon.pos)))-
    sum(!is.na(match(x,opinion.lexicon.neg)))})


# Unlist

unlist(lapply(jj,function(x){sum(!is.na(match(x,opinion.lexicon.pos)))-
    sum(!is.na(match(x,opinion.lexicon.neg)))})
)

# calculate Sentiment Score

SentimentScore <- unlist(lapply(jj,function(x){sum(!is.na(match(x,opinion.lexicon.pos)))-
    sum(!is.na(match(x,opinion.lexicon.neg)))})
)

mean(SentimentScore)
sd(SentimentScore)
hist(SentimentScore)
