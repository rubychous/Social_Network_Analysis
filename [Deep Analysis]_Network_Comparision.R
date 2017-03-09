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
# or you can use write.csv function to get the same result
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


# igraph tutorial on youtube

# Video one
# Ref: https://www.youtube.com/watch?v=1gDQg5abHsE

library(igraph)
test_case=25
graph=edge_list[[test_case]]; colnames(graph)<-c("from","to","weight");
swap_idx=which(graph[,3]<0); to=graph[swap_idx,1];
graph[swap_idx,1]=graph[swap_idx,2]; graph[swap_idx,2]=to;
graph[,3]=abs(graph[,3]) #get the absolute value of the graph
lasso_graph<-graph.data.frame(graph[,1:2],directed=TRUE)
E(lasso_graph)$color<-"grey";
E(lasso_graph)$color[as.logical(graph[,5])]<-"red"
V(lasso_graph)$color<-"grey"
V(lasso_graph)[degree(lasso_graph,mode="all")>=4]$color<-"yellow"
tkplot(lasso_graph)

# view the detections by confidence
# view(graph[order(graph[,4]),])
# view the detection by weight
# view(graph[order(graph[,3]),])
# graph analysis
hist(log(graph[,3]),100)
scc<-cluster(lasso_graph,"strong")
v_idx=which(scc$csize>1)
V(lasso_graph)[scc$membership==v_idx[2]]
unique(as.vector(graph[,1:2]))[scc$membership==v_idx[2]]
# find the edges with these vertices
V(lasso_graph)[degree(lasso_graph)>5]
# might help in the pruning of graphs
a=cbind(graph,log(graph[,4]+0.1)*graph[,3])
a=a[order(a[,6]),]

# Video 2
# Ref: https://www.youtube.com/watch?v=ZJfsihTfKwY
links=(Bali)
nodes=(BaliAttr)
links=as.matrix(links) # coerces the data as a matrix
NET=graph.adjacency(links,mode="undirected",weighted=NULL)
E(NET) # Network size by edges or links
V(NET) # Network size by verticies or nodes
plot(NET,edge.arrow.size=.4,vertex.label=NA)
mean_distance(NET, directed=F) # Calculate average distance
diameter(NET,directed=F,weights=NA) # Calculate diameter
centr_degree(NET,mode="in",normalized=T)
edge_density(NET,loop=F) # Calculate density
degree(NET) # Calculate degree of each node
mean(degree(NET)) # Calculate mean degree
net.sym<-as.undirected(NET,mode="collapse",edge.attr.comb=list(weight="sum","ignore"))

cliques(net.sym) # list of cliques
sapply(cliques(net.sym),length) # clique sizes
largest_cliques(net.sym) # cliques with max number of nodes
vcol<-rep("grey80",vcount(net.sym))
vcol[unlist(largest_cliques(net.sym))]<-"gold"
plot(as.undirected(net.sym),vertex.label=V(net.sym)$name,vertex.color=vcol)
ceb<-cluster_edge_betweenness(NET) # Calculate community partition 
dendplot(ceb,mode="hclust")
plot(ceb,NET)

# Video 3
# Ref: https://www.youtube.com/watch?v=TGICEEL-OYI
library(networkD3)
# plot
simpleNetwork(networkData)

# Video 4
# Ref: https://www.youtube.com/watch?v=isBm5RTslow
library(igraph)
myfirstnetwork<-graph(edges=c("A","B","B","C","C","A","D","C"),directed=FALSE)
plot(myfirstnetwork)

png(filename="myfirstnetwork.png")
plot(myfirstnetwork)
title(main="Ruby's Family Network")
dev.off()

# Video 5
# Ref: https://www.youtube.com/watch?v=ADzGdovNNE0
# Code: https://github.com/abhik1368/dsdht/blob/master/Networks/basic%20_networks.R

# Video 6
# Ref: https://www.youtube.com/watch?v=b_qwazJWsZE
library(igraph)
library(rgl)
library(igraph)

g<-graph.formula(1-5,1-7,2-9,2-4,3-5,4-5,4-6,4-7)
V(g) # vertex sequence
E(g) # edge sequence
str(g)
plot(g)

rglplot(g) # 3D plot

g1<-make_lattice(c(5,5,5)) # make 
coords<-layout_with_fr(g1,dim=3)
rgplot(g1,layout=coords)

magact96<-read.delim("http://sna.stanford.edu/sna_R_labs/data/mag_act96.txt",na.strings="na",check.names=FALSE)
write.table(magact96,file="magact96.csv",sep=",")

mg96<-read.csv("filepath")
magattrib <- magact96[,1:4]
g_96<-as.matrix(mg96[,-(1:4)])
row.names(g_96)<-mg96[,1]
View(mg9) # to view the column in R directly

ig_96<-graph.incidence(g_96,mode=c("all"))
plot(ig_96)

# A shorthand to access edge or vertex attribute works
# similarly to R's handling of lists and dataframes
# dataframe$variablename or list$sublist$object

V(ig96)$color[1:1295]=rgb(red=0,green=1,blue=0,alpha=.5)
V(ig96)$color[1]
V(ig96)$color[2]
V(ig96)$color[120]
