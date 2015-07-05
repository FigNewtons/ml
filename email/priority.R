#   Email Priority Ranker
#

library(lubridate)
library(tm)
library(dplyr)

# Global paths: Using the easyham set to train our ranker
easyham.path <- file.path("data", "easy_ham")

# Give file path, returns entire email content
getContent <- function(path){
    con <- file(path, open = "rt", encoding = "latin1")
    text <- readLines(con)
    close(con)
    
    return(text) 
}

# Given email content, return vector with: date, from, subject, message
parseEmail <- function(content){
    date <- getDate(content)
    from <- getFrom(content)
    subject <- getSubject(content)
    message <- getMessage(content)
    
    return(c(date, from, subject, message))
}

# Extracts date email was received and converts to POSIXct
getDate <- function(content){
    date.line <- content[grepl("^Date: ", content, perl = TRUE)][1]
    # Split along :, +, and -  The date will be second, following "Date: "
    date <- strsplit(date.line, "\\+|\\-|: ")[[1]][2] 
    # Remove leading and trailing whitespace
    date <- gsub("^\\s+|\\s+$", "", date) 
    date <- strtrim(date, 25)
    
    return(dmy_hms(date))
}

# Extracts sender's email from "From: " line in email content
getFrom <- function(content){
    from.line <- content[grepl("^From: ", content, perl = TRUE)]
    from.split <- strsplit(from.line, '[:<>]')[[1]]
    from <- from.split[!grepl("^\\s*$", from.split, perl = TRUE)]
    from.email <- from[grepl("@", from)][1]
    return(from.email)
}

# Extracts subject from email
getSubject <- function(content){
    subject.line <- content[grepl("^Subject: ", content, perl = TRUE)]
    
    if(length(subject.line) > 0){
        # Subject name will be 2nd, following "Subject: "
        subject <- strsplit(subject.line, "Subject: ")[[1]][2]
        return(subject)
    }else{
        return("")
    }
}

# Extract email body (identical to spam classifier code)
getMessage <- function(content){
    msg <- content[seq(which(content == "")[1] + 1, length(content), 1)]
    return(paste(msg, collapse = "\n"))
}

# Given directory path, returns all parsed emails as a data frame
loadEmails <- function(mail.path){
    mail.list <- dir(mail.path)
    mail.list <- mail.list[which(mail.list != "cmds")]
    
    parsed.mail <- sapply(mail.list, function(p){ 
        getContent( paste(mail.path, p, sep = "/")) %>% parseEmail})
    
    feature.df <- as.data.frame(t(parsed.mail))
    feature.df <- tbl_df(feature.df)
    names(feature.df) <- c("Date", "From.Email", "Subject", "Message")
    
}




run <- function(){
    
 
}