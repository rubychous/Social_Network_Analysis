# Deep Analysis Digital Media and Social Network
getwd()
setwd("C:/Users/LUYI/Downloads/arenas-email")
ctrl+L > clean the console
rm(list=ls()) > clean all the variables

# load package
library(igraph)

# convert data from tsv to csv
email<-read.delim("out.arenas-email") 
write.table(email, file = "DMSN.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")
# or use write.csv function to get the same result
# write.csv(email,"DMSN.csv",row.names=FALSE)
# the reason why I cannot pass email to igraph object is because it needs to be at least 2 columns
# but the TSV file contains 2 values in one column

# load csv file 
email<-read.csv("DMSN.csv",header=FALSE,stringsAsFactors=FALSE)

# pass email object to igraph
nemail<-graph.data.frame(email,directed=FALSE)

# create ER random network
er<-sample_gnm(n=1133, m=5451)

# Plot degree distribution
pdf("1.1_ER_DegreeDistribution.pdf")
hist(deg_er, breaks=1:vcount(er)-1,xlab="Number of Degrees",ylab="Count",main="Degree Distribution",xlim=c(0,24))
axis(1,xaxp=c(0,24,16))
dev.off()

### check basic info.
V(nemail)
E(nemail)
average.path.length(nemail)
diameter(nemail)
count_triangles(nemail)

V(er)
E(er)
average.path.length(er)
diameter(er)
count_triangles(er)

# or you can use summary(nemail), summary(er)

### build up attributes
# start from degree and degree distribution
V(nemail)$degree<-degree(nemail)
# extract info
V(nemail)[degree==71]

degree_distribution(nemail)

V(er)$degree<-degree(er)
degree_distribution(er)

# then clustering coefficient in global level
nemail$GC<-transitivity(nemail)
er$GC<-transitivity(er)

# clustering coefficient in local level
V(nemail)$local_cluster<-transitivity(nemail,type="local")
V(er)$local_cluster<-transitivity(er,type="local")

# set graph attribute
nemail$Average_Path<-average.path.length(nemail)
er$Average_Path<-average.path.length(er)
nemail$Diameter<-diameter(nemail)
er$Diameter<-diameter(er)
nemail$Average_Degree<-mean(degree(nemail))
er$Average_Degree<-mean(degree(er))

# betweenness attribute (set vertex attribute)
V(nemail)$Betweenness<-betweenness(nemail, directed = FALSE,normalized = TRUE)
V(er)$Betweenness<-betweenness(er, directed = FALSE,normalized = TRUE)

# get the max betweenness from vertex attribute
V(nemail)[which.max(Betweenness)]

# get vertex attriburw
get.vertex.attribute(nemail,"Betweenness")

# modularity, community detection

V(nemail)$edge_b_membership<-nemail_edge_betweenness$membership
V(er)$edge_b_membership<-er_edge_betweenness$membership

# plot in igraph
http://igraph.org/r/doc/plot.common.html

er_edge_betweenness<-cluster_edge_betweenness(er,directed=F)
nemail_edge_betweenness<-cluster_edge_betweenness(nemail,directed=F)


# Plot modularity
plot(nemail_edge_betweenness, nemail,vertex.size=2, vertex.label=NA,layout=layout.fruchterman.reingold(nemail, niter=10000),edge.color="grey",edge.width=0.01,vertex.color="blue",vertex.shape ="circle")
plot(er_edge_betweenness, er,vertex.size=2, vertex.label=NA,layout=layout.fruchterman.reingold(er, niter=10000),edge.color="grey",edge.width=0.01,vertex.color="blue",vertex.shape ="circle")
