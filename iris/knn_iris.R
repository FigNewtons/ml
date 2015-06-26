#       Basic Implementation of k-nearest neighbors
#           Algorithm on the Iris Data Set
#
#           FigNewtons - June 25, 2015

library(datasets)
library(dplyr)

# Load dataset just once
iris <- tbl_df(iris)

# Euclidean Metric
eu_distance <- function(x, y){
    sqrt(sum((x - y)^2))
}

# Square Metric (Manhattan Distance)
sq_distance <- function(x, y){
    sum(abs(x - y))
}

# Allow option of which metric to pick 
distances <- c(eu_distance, sq_distance)
names(distances) <- c("eu", "sq")

# Returns indices of the k-nearest neighbors to the testInstance
getNeighbors <- function(train.no, testInstance, dist, k){
    distance <- distances[[dist]]
    distances <- apply(train.no, 1, function(x){distance(x, testInstance)})
    head(order(distances), k)   
}

# Given a set of indices, returns the predicted species
# based off simple majority of neighbor's species
predict <- function(train, indices){
    spec <- group_by(train[indices, ], Species)
    counts <- summarize(spec, Freq = n())
    ans <- head(counts[which.max(counts$Freq), "Species"], 1)
    ans[[1]]
}


# ----- Main -----
# > source("knn_iris.R")
# > trials <- replicate(100, run())
# > mean(trials)

run <- function(k = 5, dist = "eu"){
    # Divide dataset into training and test sets (2:1)
    train <- sample_frac(iris, 2/3)
    test <- setdiff(iris, train)
    
    # Ignore the Species column
    train.no <- select(train, -Species)
    test.no <- select(test, -Species)
    
    predictions <- apply(test.no, 1, function(x){
        neighbors <- getNeighbors(train.no, x, dist, k)
        predict(train, neighbors) 
    })
    
    no.correct <- sum(predictions == test$Species)
    total <- nrow(test)
    
    percentage <- 100 * no.correct / total
    
    #sprintf("Percent correct: %.2f", percentage)
    percentage
}