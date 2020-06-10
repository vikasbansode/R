
# Network Diagram
library(igraph)

y <- data.frame(data$first,data$second)
y.network <- graph.data.frame(y,directed = T)

V(y.network)
E(y.network)
degree(y.network)
plot(y.network)



bad.vs <- V(y.network)[degree(y.network) < 20]
y.network <- delete.vertices(y.network,bad.vs)
plot(y.network)


# interative Pivot tables for R

rpivotTable::