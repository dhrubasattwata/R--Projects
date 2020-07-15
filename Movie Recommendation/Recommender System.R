# ---
# title: "Recommender System"
# author: "Dhrubasattwata Roy Choudhury"
# ---

### Import the packages
library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)
library(tidyverse)
library(caret)
library(lubridate)
library(scales)
library(ggthemes)
## Dataset

movies <- read.csv("C:/Users/Dhruba/Desktop/GitHub/R - GitHub/data/movies.csv",stringsAsFactors=FALSE)
ratings <- read.csv("C:/Users/Dhruba/Desktop/GitHub/R - GitHub/data/ratings.csv")

head(movies)
head(ratings)

## Data Pre- Processing

genres <- as.data.frame(movies$genres, stringsAsFactors=FALSE)

genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE)
colnames(genres2) <- c(1:10)
unique(genres2[,1])

genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western") # we have 18 genres in total

genre_matrix <- matrix(0,58099,18) #empty matrix, 9742=no of movies+1, 18=no of genres
genre_matrix[1,] <- genre_list #set first row to genre list
colnames(genre_matrix) <- genre_list #set column names to genre list

#iterate through matrix
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_matrix[1,] == genres2[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}

#convert into dataframe
genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[,c] <- as.integer(genre_matrix2[,c])
} #convert from characters to integers

#Create a matrix to search for a movie by genre:
years <- as.data.frame(movies$title, stringsAsFactors=FALSE)
library(data.table)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
years <- as.data.frame(substr(substrRight(substrRight(years$`movies$title`, 6),5),1,4))

search_matrix <- cbind(movies[,1], substr(movies[,2],1,nchar(movies[,2])-6), years, genre_matrix2)
colnames(search_matrix) <- c("movieId", "title", "year", genre_list)
head(search_matrix)
write.csv(search_matrix, "C:/Users/Dhruba/Desktop/GitHub/R - GitHub/data/search.csv")
#---------------------------------------------------------------
#---------------------------------------------------------------

# Example of search an Action movie produced in 1995:
subset(search_matrix, Action == 1 & year == 1995)$title

## Create a user profile
binaryratings <- ratings

for (i in 1:nrow(binaryratings)){
  if (binaryratings[i,3] > 3){
    binaryratings[i,3] <- 1
  }
  else{
    binaryratings[i,3] <- -1
  }
}

# convert binaryratings matrix to the correct format:
binaryratings2 <- dcast(binaryratings, movieId~userId, value.var = "rating", na.rm=FALSE)

for (i in 1:ncol(binaryratings2)){
  binaryratings2[which(is.na(binaryratings2[,i]) == TRUE),i] <- 0
}
binaryratings2 = binaryratings2[,-1] #remove movieIds col. Rows are movieIds, cols are userIds


#Remove rows that are not rated from movies dataset
movieIds <- length(unique(movies$movieId)) #10329
ratingmovieIds <- length(unique(ratings$movieId)) #10325
movies2 <- movies[-which((movies$movieId %in% ratings$movieId) == FALSE),]
rownames(movies2) <- NULL

#Remove rows that are not rated from genre_matrix2
genre_matrix3 <- genre_matrix2[-which((movies$movieId %in% ratings$movieId) == FALSE),]
rownames(genre_matrix3) <- NULL


### USER PROFILES: Calculate the dot product of the genre matrix and the ratings matrix and obtain the user profiles
result = matrix(0,18,668) # here, 668=no of users/raters, 18=no of genres
for (c in 1:ncol(binaryratings2)){
  for (i in 1:ncol(genre_matrix3)){
    result[i,c] <- sum((genre_matrix3[,i]) * (binaryratings2[,c])) #ratings per genre
  }
}

#Convert to Binary scale
for (c in 1:ncol(result)){
  for (i in 1:nrow(result)){
    if (result[i,c] < 0){
      result[i,c] <- 0
    }
    else {
      result[i,c] <- 1
    }
  }
}


### BUILD THE MODEL: User-Based Collaborative Filtering Approach Recommendation Engine

#Create ratings matrix. Rows = userId, Columns = movieId
ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) #remove userIds

# Method: UBCF
# Similarity Calculation Method: Cosine Similarity
# Nearest Neighbors: 30

#Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")

# Determine how similar the first four users are with each other
# create similarity matrix
similarity_users <- similarity(ratingmat[1:4, ], 
                               method = "cosine", 
                               which = "users")
as.matrix(similarity_users)
image(as.matrix(similarity_users), main = "User similarity")

# compute similarity between
# the first four movies
similarity_items <- similarity(ratingmat[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(similarity_items)
image(as.matrix(similarity_items), main = "Item similarity")

# Exploring values of ratings:
vector_ratings <- as.vector(ratingmat@data)
unique(vector_ratings) # what are unique values of ratings

table_ratings <- table(vector_ratings) # what is the count of each rating value
table_ratings


# Visualize the rating:
vector_ratings <- vector_ratings[vector_ratings != 0] # rating == 0 are NA values
vector_ratings <- factor(vector_ratings)

qplot(vector_ratings) + 
  ggtitle("Distribution of the ratings")

# Exploring viewings of movies:
views_per_movie <- colCounts(ratingmat) # count views for each movie

table_views <- data.frame(movie = names(views_per_movie),
                          views = views_per_movie) # create dataframe of views
table_views <- table_views[order(table_views$views, 
                                 decreasing = TRUE), ] # sort by number of views

ggplot(table_views[1:6, ], aes(x = movie, y = views)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(labels=subset(movies2, movies2$movieId == table_views$movie)$title) +
  ggtitle("Number of views of the top movies")

#Visualizing the matrix:
image(ratingmat[1:10, 1:15], main = "Heatmap of the first rows and columns")
image(ratingmat[rowCounts(ratingmat) > quantile(rowCounts(ratingmat), 0.99),
                colCounts(ratingmat) > quantile(colCounts(ratingmat), 0.99)], 
      main = "Heatmap of the top users and movies")

# Normalize the data
ratingmat_norm <- normalize(ratingmat)
image(ratingmat_norm[rowCounts(ratingmat_norm) > quantile(rowCounts(ratingmat_norm), 0.99),
                     colCounts(ratingmat_norm) > quantile(colCounts(ratingmat_norm), 0.99)], 
      main = "Heatmap of the top users and movies")


## THE MODEL
recommender_model <- Recommender(ratingmat_norm, 
                                 method = "UBCF", 
                                 param=list(method="Cosine",nn=30))

model_details <- getModel(recommender_model)
model_details$data

## For prediction using the model
recom <- predict(recommender_model, 
                 ratingmat[1], 
                 n=10) #Obtain top 10 recommendations for 1st user in dataset

recom

# Conversion to legible list
recom_list <- as(recom,                  "list") #convert recommenderlab object to readable list

#Obtain recommendations
recom_result <- matrix(0,10)
for (i in 1:10){
  recom_result[i] <- as.character(subset(movies, 
                                         movies$movieId == as.integer(recom_list[[1]][i]))$title)
}


## Predictions

user1 <- recom@items[[1]] # recommendation for the first user
movies_user1 <- recom@itemLabels[user1]
movies_user2 <- movies_user1
for (index in 1:10){
  movies_user2[index] <- as.character(subset(movies,
                                             movies$movieId == movies_user1[index])$title)
}
movies_user2

