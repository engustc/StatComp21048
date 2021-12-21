#' @title The shortest path  
#' @description The shortest path of a given graph
#' @param c graph' node vector
#' @return The shortest path of a given graph
#' @export
MSP<-function(c)
{
  library(igraph)
  library(plyr)
  g<-erdos.renyi.game(100,0.4,directed=T)
  degree<-degree(g,mode="all",normalized=T)
  plot(table(degree),type="h")
  g<-erdos.renyi.game(100,0.4,directed=T)
  m<-gsize(g)
  n<-vcount(g)
  l<-mean_distance(g)
  c<-transitivity(g)
  degree<-degree(g,mode="all",normalized=T)
  table(degree)
  plot(table(degree),type="h")
  degree.distribution(g)
  closeness(g,mode="in")
  order(closeness(g,mode="in"))
  betweenness(g,normalized=T)
  edge.betweenness(g)
  evcent(g,scale = F)$vector
  page.rank(g)$vector
  g<-erdos.renyi.game(100,0.4,directed=T)
  data_m<-as_data_frame(g, what="edges")
  data_m2<-as_adjacency_matrix(g)
  g2<-graph_from_data_frame(data_m, directed =F, vertices = NULL)
  plot(g2,layout=layout.grid)
  g3<-graph.adjacency(data_m2,mode="undirected")
  plot(g3,layout=layout.grid)
  g1<-graph.lattice(c(6,6,1),directed=T,mutual = T)
  V(g1)$name<-c
  V(g1)$color<-
    V(g1)$size<-12
  pa<-get.all.shortest.paths(g1,which(V(g1)$name==11),which(V(g1)$name==66))$res[[1]]
  V(g1)[pa]$color<-"green"
  E(g1,path=pa)$color<-"red"
  plot(g1,layout=layout.grid)
  random<-erdos.renyi.game(50,0.2,directed=T)
  V(random)$color<-"pink"
  V(random)$size<-10
  E(random)$color<-"grey"
  pa<-get.all.shortest.paths(random,1,20)$res[[1]]
  E(random,path=pa)$color<-"red"
  plot(random,layout=layout.fruchterman.reingold)
}
```