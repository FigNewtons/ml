
library(testthat)
source("spam.R")


# Given path, returns email as text
getEmail <- function(path){
    con <- file(path, open = "rt", encoding = "latin1")
    email <- readLines(con)
    close(con)
    
    email
}

# Get email body from line #: start
getBody <- function(email, start){
    paste(email[start: length(email)], collapse = "\n")
}

test_that("Gets email body", {
    
    path <- "test_data/test_email"
    
    email <- getEmail(path)
    # Body starts at line 10 
    body <- getBody(email, 10)
    content <- getContent(path)
    
    expect_identical(content, body)
})

test_that("Combines email bodies into single character vector", {
              
    path <- "test_data"
    
    file.list <- dir(path)
    email.list <- file.list[which(file.list != "cmds")]
    number.emails <- length(email.list)
    
    body <- sapply(email.list, 
                   function(p){ getBody(getEmail(file.path(path, p)), 10)})

    content <- loadEmails(path)
    
    # Check no. of emails, their names, and the content
    expect_equal(length(content), number.emails)
    expect_identical(names(content), email.list)
    expect_identical(content, body)
})

test_that("Checks Accuracy of Term Document Matrix", {
    
    doc1 <- "the cat in the hat"
    doc2 <- "one fish two fish"
    doc3 <- "fox in socks"
    
    doc.vec <- c(doc1, doc2, doc3)
    names(doc.vec) <- c("doc1", "doc2", "doc3")

    tdm <- createTDM(doc.vec)
    term.matrix <- inspect(tdm)
    
    fish.count <- sum(term.matrix["fish", ])
    doc2.count <- sum(term.matrix[, 2])
    doc2.split <- unlist(strsplit(doc2, " "))
    total.count <- sum(rowSums(term.matrix))
    
    # Test Certain Attributes 
    expect_equal(fish.count, 2)
    expect_equal(doc2.count, length(doc2.split))
    expect_equal(total.count, length(rownames(term.matrix)) + 1)
})

test_that("Get term count", {
    
    path <- "test_data/test_email"
    
    # For some reason, the TDM removes capitalization
    wilco <- getTermCount(path, "wilco")
    empty <- getTermCount(path, "empty")
    
    expect_equal(wilco, 1)
    expect_equal(empty, 0)
    
})

test_that("Bayes Formula", {
    
    
})