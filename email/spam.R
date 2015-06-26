#       Basic Spam Classifier
#     (Naive Bayes Classifier)
#
# By Bayes Theorem:
#   p(spam | features) = p(features | spam) p(spam) / p(features)
#
# Since we making the naive assumption that features are independent:
#   p(features) = p(feature 1)p(feature 2)...p(feature N)

library(tm)
library(dplyr)

# Global Paths: Spam, Easy Ham, Hard Ham (harder to classify)
spam.path <- file.path("data", "spam")
spam2.path <- file.path("data", "spam_2")
easyham.path <- file.path("data", "easy_ham")
easyham2.path <- file.path("data", "easy_ham_2")
hardham.path <- file.path("data", "hard_ham")
hardham2.path <- file.path("data", "hard_ham_2")


# Given file path, returns email body
getContent <- function(path){
    # rt - "read text"
    con <- file(path, open = "rt", encoding = "latin1")
    text <- readLines(con)
    
    # Email body begins after 1st empty line
    body <- text[seq(which(text == "")[1] + 1, length(text), 1)]
    close(con)
    paste(body, collapse = "\n")
}

# Given directory path, returns character vector of email bodies
loadEmails <- function(mail.path){
    mail.list <- dir(mail.path)
    mail.list <- mail.list[which(mail.list != "cmds")]
    
    all.content <- sapply(mail.list, 
                          function(m){ getContent(file.path(mail.path, m))})
    all.content
}

# Creates Term-Document-Matrix
createTDM <- function(content.vec, minFreq = 2){
    corpus <- Corpus(VectorSource(content.vec))
    control <- list(stopwords = TRUE, removePunctuation = TRUE, 
                    removeNumbers = TRUE, minDocFreq = minFreq)
    tdm <- TermDocumentMatrix(corpus, control)
    
    tdm
}

# Converts TDM to data frame with columns:
# term, frequency, density, occurrence
createWordRank <- function(tdm){
    
    word.matrix <- as.matrix(tdm)
    word.counts <- rowSums(word.matrix)
    
    # Convert to data frame and clean up
    word.df <- as.data.frame(cbind(names(word.counts), as.numeric(word.counts)), 
                             stringsAsFactors = FALSE)
    names(word.df) <- c("terms", "frequency")
    word.df$frequency <- as.numeric(word.df$frequency)
    
    word.df <- tbl_df(word.df)
    
    number.docs <- ncol(word.matrix)
    number.words <- nrow(word.matrix)
    
    occur <- 
        sapply(1:number.words, function(index){
            length(which(word.matrix[index, ] > 0)) / number.docs})
    
    word.df <- 
        mutate(word.df, density=frequency/ sum(frequency), occurrence = occur)
}

# Given a filepath to an email and a term,
# returns the # of occurrences of that term in the email
getTermCount <- function(path, term){
    body <- getContent(path)
    tdm <- createTDM(body, minFreq = 1)
 
    word.counts <- rowSums(as.matrix(tdm))
    term.count <- word.counts[which(names(word.counts) == term)]
    
    return( ifelse(length(term.count) > 0, term.count, 0))
}

# Given an email path, a training set, the (naive) prior, and
# a constant c (which is the negligible probability we assign to
# words not found in our training set), we calculate the probability
# that the email matches our training data given the words present
computeBayes <- function(path, training.df, prior = 0.5, c = 1e-6){
    body <- getContent(path)    
    tdm <- createTDM(body)
    
    freq <- rowSums(as.matrix(tdm))
    
    word.matches <- intersect(names(freq), training.df$term)
    
    # Bayes Theorem  (If checking against spam: prior = p(spam))
    bayes <- if( length(word.matches) < 1){
        prior * c^(length(freq))
    }else{
        # Occurence = p(features| spam) \ p(features)
        prob.match <- training.df$occurrence[ match(word.matches, training.df$term)]
        
        prior * prob.match * c^( length(freq) - length(word.matches))
    }
    
    bayes
}

run <- function(){
    
    # Train using 500 easy_ham and spam emails
    spam.df <- loadEmails(spam.path) %>% createTDM %>% createWordRank
    easyham.df <- loadEmails(easyham.path) %>% createTDM %>% createWordRank
    
    hardham <- loadEmails(hardham.path)
    
    
    
}










