
# Set working Directory

setwd("D:\\from E\\ML")

# import libraries
library(arules)

# Read Data
mydata <- read.csv("Cosmetics.csv",header = T,sep = ",")

# Creat rules on mydata
rules <- apriori(mydata)
summary(rules)

# Support: % of cases that include both 'if' and 'then' items.
# Confidence : % of case with 'if' that also have 'then' items.
# lift = confidencel(% of cases with 'then' items)

# Reduce to smaller number of rules

rules <- apriori(mydata,parameter = list(minlen=2,maxlen=3,supp=.7))
inspect(rules)

# Finding interesting rules -1

rules <- apriori(mydata,parameter = list(minlen=2,maxlen=3,conf=.7),
                 appearance = list(rhs=c("Foundation=Yes"),default='lhs'))
inspect(rules)

# Visualizing rules
library(arulesViz)
plot(rules)
plot(rules,method = 'grouped')
plot(rules,method = 'graph',control = list(type='items'))

# Finding interesting rules-2
rules <- apriori(mydata,parameter = list(minlen=2, maxlen=5,supp=.1, conf=.5),
                 appearance=list(rhs=c("Foundation=Yes"),lhs=c("Bag=Yes", "Blush=Yes", "Nail.Polish=Yes", "Brushes=Yes",
                "Concealer=Yes", "Eyebrow.Pencils=Yes", "Bronzer=Yes", "Lip.liner=Yes", "Mascara=Yes", "Eye.shadow=Yes","Lip.Gloss=Yes", 
                "Lipstick=Yes", "Eyeliner=Yes"),default="none"))
quality(rules)<-round(quality(rules),digits=3)
rules.sorted <- sort(rules, by="lift")
inspect(rules)

# Finding redundancy
redundant <- is.redundant(rules, measure="confidence")
which(redundant)
rules.pruned <- rules[!redundant]
rules.pruned <- sort(rules.pruned, by="lift")
inspect(rules.pruned)
