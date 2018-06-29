library('igraph')
myfiles <- list.files(pattern="*circles")
count = 0
for (files in myfiles)
{
  if (length(readLines(files))>2)
    count=count+1
  else
    print(files)
    
}
print(paste("Number of Personalized Networks with 2 or more circles : ",toString(count)))

#First Node : 109327480479767108490
  id = 109327480479767108490
  dat=read.table("./gplus/109327480479767108490.edges",header=TRUE) 
  g=graph.data.frame(dat,directed=FALSE)
  
  # Need to add a new vertex and n new edges connecting the ego network to all edges 
  k = length(V(g))
  g = add.vertices(g,nv=1,name=id)
  ctr = 0
  aux = V(g)$name
  for (vertices in V(g))
  {

    ctr=ctr+1
    g = add_edges(g,c(id,aux[ctr]))
  }
plot(degree.distribution(g,mode="in"),type=1, main ="In-degree Histogram  of 109327480479767108490" )
deg1= degree(g, mode="in")
hist1 = hist(deg1, breaks = seq(from = 0, to = max(deg1) + 1, by = 1), freq = FALSE, main = "In-degree Histogram  of 109327480479767108490",xlab = "Degree")
deg1= degree(g, mode="out")
hist1 = hist(deg1, breaks = seq(from = 0, to = max(deg1) + 1, by = 1), freq = FALSE, main = "Out-degree Histogram  of 109327480479767108490",xlab = "Degree")



#Second node : 115625564993990145546
id = 115625564993990145546
dat=read.table("./gplus/115625564993990145546.edges",header=TRUE) 
g=graph.data.frame(dat,directed=FALSE)

# Need to add a new vertex and n new edges connecting the ego network to all edges 
k = length(V(g))
g = add.vertices(g,nv=1,name=id)
ctr = 0
aux = V(g)$name
for (vertices in V(g))
{
  
  ctr=ctr+1
  g = add_edges(g,c(id,aux[ctr]))
}

deg1= degree(g, mode="in")
hist1 = hist(deg1, breaks = seq(from = 0, to = max(deg1) + 1, by = 1), freq = FALSE, main = "In-degree Histogram  of 115625564993990145546",xlab = "Degree")
deg1= degree(g, mode="out")
hist1 = hist(deg1, breaks = seq(from = 0, to = max(deg1) + 1, by = 1), freq = FALSE, main = "Out-degree Histogram  of 115625564993990145546",xlab = "Degree")


#Third node : 101373961279443806744
id =  101373961279443806744
dat=read.table("./gplus/101373961279443806744.edges",header=TRUE) 
g=graph.data.frame(dat,directed=FALSE)

# Need to add a new vertex and n new edges connecting the ego network to all edges 
k = length(V(g))
g = add.vertices(g,nv=1,name=id)
ctr = 0
aux = V(g)$name
for (vertices in V(g))
{
  
  ctr=ctr+1
  g = add_edges(g,c(id,aux[ctr]))
}


deg1= degree(g, mode="in")
hist1 = hist(deg1, breaks = seq(from = 0, to = max(deg1) + 1, by = 1), freq = FALSE, main = "In-degree Histogram  of 101373961279443806744",xlab = "Degree")
deg1= degree(g, mode="out")
hist1 = hist(deg1, breaks = seq(from = 0, to = max(deg1) + 1, by = 1), freq = FALSE, main = "Out-degree Histogram  of 101373961279443806744",xlab = "Degree")




dev.off()
id = 109327480479767108490
dat=read.table("./gplus/109327480479767108490.edges",header=TRUE) 
g=graph.data.frame(dat,directed=FALSE)
g = add.vertices(g,nv=1,name=id)
ctr = 0
aux = V(g)$name
for (vertices in V(g))
{
  
  ctr=ctr+1
  g = add_edges(g,c(id,aux[ctr]))
}
walktrap_comm = walktrap.community(g)
print(modularity(walktrap_comm))

plot(walktrap_comm, g, asp = 9/16, vertex.label=NA , edge.color = "blue", layout=layout.fruchterman.reingold)


dev.off()
id = 115625564993990145546
dat=read.table("./gplus/115625564993990145546.edges",header=TRUE) 
g=graph.data.frame(dat,directed=FALSE)
g = add.vertices(g,nv=1,name=id)
ctr = 0
aux = V(g)$name
for (vertices in V(g))
{
  
  ctr=ctr+1
  g = add_edges(g,c(id,aux[ctr]))
}
walktrap_comm = walktrap.community(g)
print(modularity(walktrap_comm))

plot(walktrap_comm, g, vertex.label=NA , edge.color = "red", layout=layout.fruchterman.reingold)


id =  101373961279443806744
dev.off()
dat=read.table("./gplus/101373961279443806744.edges",header=TRUE) 
g=graph.data.frame(dat,directed=FALSE)
g = add.vertices(g,nv=1,name=id)
ctr = 0
aux = V(g)$name
for (vertices in V(g))
{
  
  ctr=ctr+1
  g = add_edges(g,c(id,aux[ctr]))
}
walktrap_comm = walktrap.community(g)
print(modularity(walktrap_comm))

plot(walktrap_comm, g, vertex.label=NA , edge.color = "black", layout=layout.fruchterman.reingold)
