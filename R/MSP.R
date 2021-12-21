#' @title The minimum spanning tree 
#' @description The minimum spanning tree of a given graph
#' @param vn graph' node number
#' @return The minimum spanning tree of a given graph
#' @export
MST<-function(vn)
{
  library(igraph)
  rownames<-letters[1:vn]
  colnames<-letters[1:vn]
  adjacency_matrix=matrix(0,vn,vn,byrow=T,dimnames=list(rownames,colnames))            
  adjacency_matrix_k=matrix(0,vn,vn,byrow=T,dimnames=list(rownames,colnames))         
  adjacency_matrix_p=matrix(0,vn,vn,byrow=T,dimnames=list(rownames,colnames))          
  link <- function(x,y,w){
    adjacency_matrix[x,y]<<-w
    adjacency_matrix[y,x]<<-w
  }
  link_k <- function(x,y,w){
    adjacency_matrix_k[x,y]<<-w
    adjacency_matrix_k[y,x]<<-w
  }
  link_p <- function(x,y,w){
    adjacency_matrix_p[x,y]<<-w
    adjacency_matrix_p[y,x]<<-w
  }
  a=1;b=2;c=3;d=4;e=5;f=6;g=7;h=8;i=9;j=10;k=11;l=12
  link(a,b,2);link(b,c,3);link(c,d,1)
  link(a,e,3);link(b,f,1);link(c,g,2);link(d,h,2)
  link(e,f,4);link(f,g,3);link(g,h,3)
  link(e,i,4);link(f,j,2);link(g,k,4);link(g,l,3)
  link(i,j,3);link(j,k,3);link(k,l,1)
  print(adjacency_matrix)
  myg=graph.adjacency(adjacency_matrix,mode = "undirected",weighted = TRUE)  
  myl <-layout.reingold.tilford(myg)
  myl[a,]=c(0,2);myl[b,]=c(1,2);myl[c,]=c(2,2);myl[d,]=c(3,2)
  myl[e,]=c(0,1);myl[f,]=c(1,1);myl[g,]=c(2,1);myl[h,]=c(3,1)
  myl[i,]=c(0,0);myl[j,]=c(1,0);myl[k,]=c(2,0);myl[l,]=c(3,0)
  E(myg)$width=E(myg)$weight
  plot.igraph(myg,layout = myl,edge.label = E(myg)$weight)
  Sys.sleep(1.5)
  visited <- c(0*(1:vn))
  Prim <- function(){
    for(o in 1:vn-1 ){
      for(i in order(E(myg)$weight)){
        i=as.numeric(i)
        v1<-as.numeric(charToRaw(get.edgelist(myg)[i,1]))-as.numeric(charToRaw('a'))+1
        v2<-as.numeric(charToRaw(get.edgelist(myg)[i,2]))-as.numeric(charToRaw('a'))+1
        if(visited[v1]==1&&visited[v2]==0){
          link_p(v1,v2,E(myg)[i]$weight)
          visited[v2]=1
          E(myg)[i]$color="red"
          plot.igraph(myg,layout=myl,main="Prim",edge.label = E(myg)$weight)
          Sys.sleep(1)
          break
        }
        if(visited[v2]==1&&visited[v1]==0){
          link_p(v1,v2,E(myg)[i]$weight)
          visited[v1]=1
          E(myg)[i]$color="red"
          plot.igraph(myg,layout=myl,main="Prim",edge.label = E(myg)$weight)
          Sys.sleep(1)
          break
        }
      }
    }}
  visited[a]=1
  Prim()
  myg_p=graph.adjacency(adjacency_matrix_p,mode = "undirected",weighted = TRUE)  
  E(myg_p)$width=E(myg_p)$weight
  plot.igraph(myg_p,layout=myl,main="Prim",edge.label = E(myg_p)$weight)
  print("Adjacency Matrix for Prim:")
  print(adjacency_matrix_p)
  Sys.sleep(1.5)
  plot.igraph(myg,layout = myl,edge.label = E(myg)$weight)
  Sys.sleep(1.5)
  myset<-c(0*(1:vn))
  setcnt<-0
  same_set=matrix(0,vn,vn,byrow=T)
  bindset <- function(x,y){
    same_set[x,y]<<-1
    same_set[y,x]<<-1
    for(i in 1:vn){
      i=as.numeric(i)
      if(same_set[y,i]==1&&i!=x&&i!=y&&same_set[x,i]==0) bindset(i,x)
      if(same_set[x,i]==1&&i!=y&&i!=x&&same_set[y,i]==0) bindset(i,y)
    }
  }
  for(i in 1:vn) same_set[i,i]=1
  Kruskal <- function(){
    cnt<-0
    for(i in order(E(myg)$weight)){
      i=as.numeric(i)
      v1<-as.numeric(charToRaw(get.edgelist(myg)[i,1]))-as.numeric(charToRaw('a'))+1
      v2<-as.numeric(charToRaw(get.edgelist(myg)[i,2]))-as.numeric(charToRaw('a'))+1
      if(myset[v1]==0&&myset[v2]==0){
        setcnt<<-setcnt+1
        myset[v1]<<-setcnt
        myset[v2]<<-setcnt
        link_k(v1,v2,E(myg)[i]$weight)
        E(myg)[i]$color="red"
        plot.igraph(myg,layout=myl,main="Kruskal",edge.label = E(myg)$weight)
        #Sys.sleep(1)
        cnt=cnt+1
        if(cnt==vn-1) break
      }
      else{ if(myset[v1]==0||myset[v2]==0){
        if(myset[v2]==0) myset[v2]=myset[v1]
        else myset[v1]=myset[v2]
        link_k(v1,v2,E(myg)[i]$weight)
        E(myg)[i]$color="red"
        plot.igraph(myg,layout=myl,main="Kruskal",edge.label = E(myg)$weight)
        Sys.sleep(1)
        cnt=cnt+1
        if(cnt==vn-1) break
      }
        else if(same_set[myset[v1],myset[v2]]==0){
          bindset(myset[v1],myset[v2])
          link_k(v1,v2,E(myg)[i]$weight)
          E(myg)[i]$color="red"
          plot.igraph(myg,layout=myl,main="Kruskal",edge.label = E(myg)$weight)
          Sys.sleep(1)
          cnt=cnt+1
          if(cnt==vn-1) break
        }
      }
    }
  }
  Kruskal()
  Sys.sleep(1)
  myg_k=graph.adjacency(adjacency_matrix_k,mode = "undirected",weighted = TRUE)  
  E(myg_k)$width=E(myg_k)$weight
  plot.igraph(myg_k,layout=myl,main="Kruskal",edge.label = E(myg_k)$weight)
  print("Adjacency Matrix for Kruskal:")
  print(adjacency_matrix_k)
  Sys.sleep(1.5)
}