# Set working Directory
setwd("D:\\from E\\ML")

# import library
library(arules)

# Read Data

data("Groceries")
groceries <- Groceries
summary(Groceries)

inspect(groceries[1:5])

itemFrequency(groceries[,1:3])

# I will call item to a product purchased in each cart. And item set, 
# is products occurring together
# In association analysis there are 3 main concepts to work with:
# 1. Support: The fraction of which our item set occurs in our data set.
# 2. Confidence: probability that a rule is correct for a new transaction
# with items on the left.
# 3. Lift: The ratio by which by the confidence of a rule exceeds the expected confidence.

# Data visualization
## Items at least with 10% support.
itemFrequencyPlot(groceries, support = 0.1)
## Frequency Plot for the top 20 items
itemFrequencyPlot(groceries, topN = 20)
## It is also possible to visualize the entire Sparse Matrix:
image(groceries[1:5])
image(sample(groceries, 100))
# Building the Model
# We are now ready to mine some rules! + 
# We set the minimum support to 0.006. + 
# We set the minimum confidence of 0.25. + 
# We set the minimum to 2 items.
apriori(groceries)#Mine frequent itemsets

# # Get the rules
rules <- apriori(groceries, parameter = list(supp = 0.006,
                                             conf = 0.25,minlen = 2))
summary(rules)
# Evaluating model performance
# The size of the rule is calculated as the total of
# both Left-hand side (LHS) and right-hand side of the rule (RHS).

inspect(rules[1:5])

# Sorting the set of the association Rule by lift.
inspect(sort(rules, by ="lift")[1:5])

# Targeting Items
# I am going to subset to see if a particular item is purchased along with 
# others.
# There are two types of targets we might be interested in that 
# are illustrated with some examples: + What are customers likely to
# buy after buying "blueberries" + What are customers likely to buy before
# buying "whole milk"" + What are customers likely to buy if they purchase 
# "yogurt" and "blueberries"?

berryrules <- subset(rules, items %in% "berries")
inspect((berryrules))

# Interesting, it seems people buy berries to consume mostly for breakfast time.
# Mixing them with milk (maybe as a shake) or with yoghurt or cream.
# Now, the second question can also be answered

milkrules<-apriori(data=groceries, parameter=list(supp=0.001,conf = 0.08),
                   appearance =list(default="lhs",rhs="whole milk"),
                   control = list(verbose=F))
milkrules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(milkrules[1:5])

## and mixing queries.
rules2 <- subset(rules, items %in% c("berries", "yogurt"))
inspect(rules2[1:10])

#Except for the cat-food item, the other products seem to be purchased also for breakfast time.

# Visualization
library("devtools")
library(arulesViz)
plot(rules,method="graph",interactive=TRUE,shading=NA)

# Save rules

# I will, finally, convert the rules as data frame in order to view it 
# in the R studio viewer and will save in a csv for extensive analysing 
# purposes.

rules_df <- as(rules, "data.frame")
write(rules, file = "groceryrules.csv",
      sep = ",",
      quote =TRUE,
      row.names = FALSE)
