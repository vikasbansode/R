# Social Network Analysis
library(igraph)
g <- graph(c(1,2,2,3,3,4,4,1))
g <- graph(c(1,2,2,3,3,4,4,1),directed = F)
g <- graph(c(1,2,2,3,3,4,4,1),directed = F,n=7)

plot(g,
     vertex.color = 'green',
     vertex.size = 40,
     edge.color = 'red')

g[]

g1 <- graph(c("Amy","Ram","Ram","Li","Li","Amy","Amy","Li","Kate","Li"))

plot(g1,
     vertex.color = 'green',
     vertex.size = 40,
     edge.color = 'red')

# Network measures
# Degree
degree(g1,mode = 'all')
degree(g1,mode = 'in')
degree(g1,mode = 'out')

# Diameter
diameter(g1,directed = F,weights = NA)
edge_density(g1,loops = F)
ecount(g1)/(vcount(g1)*(vcount(g1)-1))
reciprocity(g1)
closeness(g1,mode = 'all',weights = NA)
betweenness(g1,directed = T,weights = NA)
edge_betweenness(g1,directed = T,weights = NA)

# Read Data File
#D:\from E\R
data <- read.csv(file.choose(),header = T)
# Example
# - Twitter
# - University website
# - Facebook friends
# - Email networks
# - Facebook likes

y <- data.frame(data$first,data$second)

# Create network
net <- graph.data.frame(y,directed = T)
V(net)
E(net)

V(net)$label <- V(net)$name
V(net)$label
V(net)$degree <- degree(net)
V(net)$degree


# Histogram of node degree

hist(V(net)$degree,
     col='green',
     main = "histogram of Node Degree",
     ylab= "Frequency",
     xlab = 'Degree of Vertices')

# Network Diagram
set.seed(222)
plot(net,
     vertex.color='gree',
     vertex.size=2,
     vertex.label.dist=1.5,
     edge.arrow.size=0.1,
     vertex.label.cex=0.8)

plot(net,
     vertex.color='gree',
     vertex.size=2,
     edge.arrow.size=0.1,
     vertex.label.cex=0.8)

# Highlighting degree & layouts
plot(net,
     vertex.color=rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size=0.1,
     layout=layout.fruchterman.reingold)


plot(net,
     vertex.color=rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size=0.1,
     layout=layout.graphopt)


plot(net,
     vertex.color=rainbow(52),
     vertex.size = V(net)$degree*0.4,
     edge.arrow.size=0.1,
     layout=layout.kamada.kawai)

# Hub and authorities
hs <- hub_score(net)$vector
as <- authority.score(net)$vector
par(mfrow=c(1,2))
set.seed(123)
plot(net,
     vertex.size=hs*30,
     main="hubs",
     vertex.color=rainbow(52),
     edge.arrow.size=0.1,
     layout=layout.kamada.kawai)

plot(net,
     vertex.size=as*30,
     main="Authorities",
     vertex.color=rainbow(52),
     edge.arrow.size=0.1,
     layout=layout.kamada.kawai)
par(mfrow=c(1,1))

# Community detection
net <- graph.data.frame(y,directed = F)
cnet <- cluster_edge_betweenness(net)
plot(cnet,
     net,
     vertex.size=10,
     vertex.label.cex=0.8)
