#   Email Priority Ranker
#

library(lubridate)
library(tm)
library(plyr)
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

# Extracts date email was received
getDate <- function(content){
    date.line <- content[grepl("^Date: ", content, perl = TRUE)][1]
    # Split along :, +, and -  The date will be second, following "Date: "
    date <- strsplit(date.line, "\\+|\\-|: ")[[1]][2] 
    # Remove leading and trailing whitespace
    date <- gsub("^\\s+|\\s+$", "", date) 
    date <- strtrim(date, 25)
    
    return(date)
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
createTable <- function(mail.path){
    mail.list <- dir(mail.path)
    mail.list <- mail.list[which(mail.list != "cmds")]
    
    parsed.mail <- sapply(mail.list, function(p){ 
        getContent( paste(mail.path, p, sep = "/")) %>% parseEmail})
    
    email.df <- as.data.frame(t(parsed.mail), stringsAsFactors = FALSE)
    email.df <- tbl_df(email.df)
    names(email.df) <- c("Date", "From.Email", "Subject", "Message")
    
    # Convert email, subject to lowercase and sort by date
    email.df <- email.df %>% 
        mutate(Date = dmy_hms(Date), From.Email = tolower(From.Email), 
               Subject = tolower(Subject)) %>% 
        arrange(Date)

    return(email.df)
}
 
# Find emails that are part of a thread (aka "re: "); returns sender and 
# corresponding subject
findThread <- function(email.df){
    response.threads <- strsplit(email.df$Subject, "re: ")
    
    # If the subject is a response thread, the first index should be empty
    # since we split by "re: "
    is.thread <- sapply(response.threads, function(subject){
        ifelse(subject[1] == "", TRUE, FALSE)
    })
    
    # Get response threads and paste subject line back together
    threads <- response.threads[is.thread] %>%
        sapply(function(t){paste(t[2:length(t)], collapse="re: ")})
    
    senders <- email.df$From.Email[is.thread]
    
    return(cbind(senders, threads))
}

# Takes the result of findThread (thread.matrix) and returns a df
# with sender email, mail frequency, and the corresponding log weight
emailThread <- function(thread.matrix){
    senders <- thread.matrix[ , 1]
    # Get counts on no. of threads for each email
    senders.freq <- table(senders)
    senders.matrix <- 
        cbind(names(senders.freq), senders.freq, log1p(senders.freq))
    
    senders.df <- data.frame(senders.matrix, stringsAsFactors = FALSE) 
    row.names(senders.df) <- 1:nrow(senders.df)
    names(senders.df) <- c("From.Email", "Freq", "Weight")
    senders.df <- mutate(senders.df, 
                         Freq = as.numeric(Freq), Weight = as.numeric(Weight))
    
    return(senders.df)
}

# For a given thread title, count no. of emails associated with it, the 
# time span in which they were sent, and assign a weight
getThreadCounts <- function(thread, email.df){
    thread.times <- email.df$Date[which(email.df$Subject == thread | 
                            email.df$Subject == paste("re:", thread))]
    
    freq <- length(thread.times)
    t.range <- range(thread.times)
    time.span <- as.numeric(difftime(t.range[2], t.range[1], units = 'secs'))
    
    if(freq < 2){
        return( c(NA, NA, NA))
    }else{
        trans.weight <- freq/ time.span
        log.trans.weight <- 10 + log(trans.weight, base = 10)
        return(c(freq, time.span, log.trans.weight))   
    }
}

# Get unique thread list
getThreads <- function(thread.matrix, email.df){
    threads <- unique(thread.matrix[ , 2])
    thread.counts <- lapply(threads, function(t){ getThreadCounts(t, email.df)})
    thread.matrix <- do.call(rbind, thread.counts)
    return(cbind(threads, thread.matrix))
}

# Get term counts from a vector of terms
getTermCounts <- function(term.vec, control){
    vec.corpus <- Corpus(VectorSource(term.vec))
    vec.tdm <- TermDocumentMatrix(vec.corpus, control = control)
    return(rowSums(as.matrix(vec.tdm)))
}

# We weight terms from threads -- the more active the thread, the higher
# weight we give to the corresponding keywords
weightTerms <- function(thread.weights){
    thread.terms <- 
        getTermCounts(thread.weights$Thread, control=list(stopwords=stopwords()))
    thread.terms <- names(thread.terms)                
    
    term.weights <- sapply(thread.terms, function(t){
        mean(thread.weights$Weight[grepl(t, thread.weights$Thread, fixed=TRUE)])
        })
    term.weights <- 
        data.frame(list(Term = names(term.weights), Weight = term.weights), 
                   stringsAsFactors = FALSE, row.names = 1:length(term.weights))
    
    return(term.weights)
}

# Weight a message based on all emails
weightMessage <- function(email.df){
    msg.terms <- getTermCounts(email.df$Message, 
                               control = list(stopwords = stopwords(),
                               removePunctuation = TRUE, removeNumbers = TRUE))
    msg.weights <- data.frame(list(Term = names(msg.terms), 
                                   Weight = log(msg.terms, base = 10)),
                              stringsAsFactors = FALSE,
                              row.names = 1:length(msg.terms))
    return(msg.weights)
}

# Does the weight lookup for a given search term
getWeights <- function(search.term, weight.df, term = TRUE){
    if(length(search.term) > 0){
        # The data frames have different column names
        if(term){
            term.match <- match(names(search.term), weight.df$Term)
        }else{
            term.match <- match(search.term, weight.df$Thread)
        }
        
        match.weights <- weight.df$Weight[which(!is.na(term.match))]
        if(length(match.weights) > 1){
            return(1)
        }
        else{
            return(mean(match.weights))
        }
    }else{
        return(1)
    }
}

# Return the rank of a new email
rankMessage <- function(msg, rankers){
    
    # Unpack data frames from the ranker
    from.weight <- rankers[1]
    senders.df <- rankers[2]
    thread.weights <- rankers[3]
    term.weights <- rankers[4]
    msg.weights <- rankers[5]
    
    from.freq <- which(from.weight$From.Email == msg[2])
    send.freq <- which(senders.df$From.Email == msg[2])
    
    # Get our features for ranking 
    from <- ifelse(length(from.freq) > 0, 
                   from.weight$Weight[length(from.freq)], 1)
    
    thread.from <- ifelse(length(send.freq) > 0, 
                   senders.df$Weight[length(send.freq)], 1)
    
    subj <- strsplit(tolower(msg[3]), "re: ")
    
    is.thread <- ifelse(subj[[1]][1] == "", TRUE, FALSE)
        
    if(is.thread){    
        activity <- getWeights(subj[[1]][2], thread.weights, term = FALSE )
    }else{
        activity <- 1
    }
    
    thread.terms <- getTermCounts(msg[3], control=list(stopwords = stopwords()))
    thread.terms.weights <- getWeights(thread.terms, term.weights)
    
    msg.terms <- getTermCounts(msg[4], control=list(stopwords = stopwords(),
                            removePunctuation = TRUE, removeNumbers = TRUE))
    msg.weights <- getWeights(msg.terms, msg.weights)
    
    # Compute rank
    rank <- prod(from, thread.from, activity, thread.terms.weights, msg.weights)
    
    return(c(msg[1], msg[2], msg[3], rank))
}


createRanker <- function(train.df){
    
    # 1st Feature - List of top senders (with frequency and log weights)
    from.weight <- ddply(train.df, .(From.Email), summarize, 
                         Freq = length(Subject)) %>% 
        arrange(desc(Freq)) %>%
        mutate(Weight = log1p(Freq))
    

    thread.matrix <- findThread(train.df) 
    
    # 2nd Feature - Weights sender activity in active threads
    senders.df <- emailThread(thread.matrix)
    
    # 3rd Feature - Assign weights to thread message activity
    # (taking into account time span)
    thread.weights <- getThreads(thread.matrix, train.df) %>% 
                        data.frame(stringsAsFactors = FALSE)
    names(thread.weights) <- c("Thread", "Freq", "Response", "Weight")
    thread.weights <- mutate(thread.weights, Freq = as.numeric(Freq),
                             Response = as.numeric(Response),
                             Weight = as.numeric(Weight)) %>%
                    subset(is.na(thread.weights$Freq) == FALSE)
    
    # 4th Feature - Weight terms associated with active threads
    term.weights <- weightTerms(thread.weights)
    
    # 5th Feature - Notes common terms in all emails
    msg.weights <- weightMessage(train.df) %>% subset(Weight > 0)
    
    return(c(from.weight, senders.df, thread.weights, term.weights, msg.weights))
}



run <- function(){
    
    email.df <- createTable(easyham.path)
    half <- 1: (round(nrow(email.df) / 2))
    
    # Top half is the training set
    train.df <- email.df[half, ]
    test.df <- email.df[half + 1: nrow(email.df), ]
    
    rankers <- createRanker(train.df)
    
    train.ranks <- lapply(train.df, function(msg){rankMessage(msg, rankers)})
    train.ranks.matrix <- do.call(rbind, train.ranks)
    train.ranks.df <- data.frame(train.ranks.matrix, stringsAsFactors = FALSE)
    
    print(head(train.ranks.df))
}