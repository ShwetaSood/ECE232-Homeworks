library(igraph)
library(data.table)
setwd("C:\\Users\\karan\\Downloads")
rm(list=ls())getwd
edge_list <- fread("movie_edge_list1.txt",sep=" ")
# edge_list <- fread("imdb_edgelist.txt",sep=" ")
head(edge_list)
movies_mapping <- fread("movies_mapping.txt", sep='\t')
# movies_mapping <- fread("moviemap.txt", sep='\t')
movies_mapping
movie_rating <- fread("movie_rating.txt", sep='\t')
movie_rating
g <- read.graph("movie_edge_list1.txt",directed = FALSE, format=c("ncol"), weights='yes')
# g <- read.graph("imdb_edgelist.txt",directed = FALSE, format=c("ncol"), weights='yes')

core_movie = "Batman v Superman: Dawn of Justice (2016)"
#Batman v Superman: Dawn of Justice (2016)
#Mission: Impossible - Rogue Nation (2015)
#Minions (2015)

ind = movies_mapping$V1[movies_mapping$V2==core_movie]
list_neighbors <- neighbors(g,as.character(ind),mode="total")

# Question 9
sum = 0
count = 0
rating_list = numeric(0)
for (neighbor in list_neighbors){
  movie_name = movies_mapping$V2[movies_mapping$V1==neighbor]
  if(length(movie_rating$V3[movie_rating$V1==movie_name])){
    sum = sum + movie_rating$V3[movie_rating$V1==movie_name]
    rating_list = c(rating_list, movie_rating$V3[movie_rating$V1==movie_name])
    count = count + 1
  }
}
print(c("Average Rating of Neighbors", sum/count))
avg_1 = sum/count
# print(c("Rating of movie whose neighbors have been extracted",movie_rating$V3[movie_rating$V1==core_movie]))
hist(rating_list, main=paste('Distribution of average rating of movies in neighborhood'),col='blue')
# nrating_list = c(rating_list, rep(6.5,50))
# nrating<-c(rating_list, c(rep(5.2,60),rep(5.7,60),rep(6.2,60),rep(6.7,60),rep(7.2,60),rep(7.7,60),rep(8.2,60),rep(8.7,60)))

# Question 10
# list.files(path='.')
ind = movies_mapping$V1[movies_mapping$V2==core_movie]
list_neighbors <- neighbors(g,as.character(ind),mode="total")
load(file="final_comunities.rda")
core_node_membership <- membership(communities)[[as.character(ind)]]

sum = 0
count = 0
rating_list = numeric(0)
for(neighbor in list_neighbors){
  if(core_node_membership==membership(communities)[[as.character(neighbor)]]){
    movie_name = movies_mapping$V2[movies_mapping$V1==neighbor]
    if(length(movie_rating$V3[movie_rating$V1==movie_name])){
      sum = sum + movie_rating$V3[movie_rating$V1==movie_name]
      rating_list = c(rating_list, movie_rating$V3[movie_rating$V1==movie_name])
      count = count + 1
    }
  }
  }
  print(c("Average Rating of Neighbors in Same Community", sum/count))
  avg_2 = sum/count
  # print(c("Rating of movie whose neighbors have been extracted",movie_rating$V3[movie_rating$V1==core_movie]))
 
  # Question 11
  ind = movies_mapping$V1[movies_mapping$V2==core_movie]
  list_neighbors <- neighbors(g,as.character(ind),mode="total")
  subset = edge_list[which(edge_list$V1==ind | edge_list$V2==ind)]
  ordered_subset = subset[order(-subset$V3),]
  top_ordered_subset = ordered_subset[1:5,]
  neighbors_of_ind = numeric(0)
  for (row in 1:nrow(top_ordered_subset)){
    val = which(ordered_subset[row,1:2]!=ind)
    if(val[1]==2){
      print(as.character(ordered_subset[row, 2]))
      neighbors_of_ind = c(neighbors_of_ind, as.character(ordered_subset[row, 2]))
    }else{
      print(as.character(ordered_subset[row, 1]))
      neighbors_of_ind = c(neighbors_of_ind, as.character(ordered_subset[row, 1]))
    }
  }
  # for (row in 1:nrow(ordered_subset)) {
  #     if(ordered_subset[row,1:3]!=ind) {
  #         neighbors_of_ind = c(neighbors_of_ind, ordered_subset[row,"V2"])
  #     }else{
  #         neighbors_of_ind = c(neighbors_of_ind, ordered_subset[row,"V1"])
  #     }
  # }
  neighbors_of_ind
  for(neighbor in neighbors_of_ind){
    movie_name = movies_mapping$V2[movies_mapping$V1==neighbor]
    membership_of_neighbor = membership(communities)[[as.character(neighbor)]]
    print(c(movie_name, membership_of_neighbor))
  }
  print(top_ordered_subset)
  