# Unit Tests for knn_iris
# Run: test_file("test_knn_iris.R")  OR test_dir(".")

library(testthat)
source("knn_iris.R")

test_that("Correct Euclidean Distance",{
    
    x <- c(3, 4)
    y <- c(-1, 1)
    z <- 0
    
    d1 <- eu_distance(x, y)
    d2 <- eu_distance(x, z)
    
    expect_equal(d1, 5)
    expect_equal(d2, 5)
})

test_that("Correct Manhattan Distance", {
    
    x <- c(8, -19, 0, 1)
    y <- c(0, 1, 10, 3)
    z <- 0
    
    d1 <- sq_distance(x, y)
    d2 <- sq_distance(y, z)
    
    expect_equal(d1, 40)
    expect_equal(d2, sum(y))
})


test_that("Get Neighbors", {
    
    train.no <- matrix(1:100, nrow = 25, ncol = 4, byrow = TRUE)
    testInstance <- c(0,0,0,0)
    k = 5
    eu <- "eu"
    sq <- "sq"
    
    n1 <- getNeighbors(train.no, testInstance, eu, k)
    n2 <- getNeighbors(train.no, testInstance, sq, k)
    
    expect_identical(n1, 1:k)
    expect_identical(n2, 1:k)
})

test_that("Predict", {
    
    ind1 <- c(1, 6, 11, 16, 21)
    ind2 <- c(1, 5, 11, 15)
    
    train <- as.data.frame(matrix(1:100, nrow = 25, ncol = 4, byrow = TRUE))
    species <- rep(c("rose", "daisy", "tulip", "orchid", "sunflower"), 5)
    train$Species <- species
    
    p1 <- predict(train, ind1)
    p2 <- predict(train, ind2)
    
    # Should return factor "rose"
    rose <- "rose"
    
    expect_equal(p1, rose)
    expect_equal(p2, rose)
    
})

test_that("Run: Boundary Check", {
    percent <- run()
    expect_more_than(percent, 85)
})