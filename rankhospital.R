## Function for identifying best hospital by state and outcome
rankhospital <- function(state, outcome, num = "best") {

        ## Read outcome data and format values
        raw <- read.csv("outcome-of-care-measures.csv", 
                        colClasses="character",
                        na.strings="Not Available")
        data <- raw[ , c(2, 7, 11, 17, 23)]
        colnames(data) <- c("hospital", "state", "heart_attack", 
                                "heart_failure", "pneumonia")
        data[ ,3] <- suppressWarnings(as.numeric(data[ ,3]))
        data[ ,4] <- suppressWarnings(as.numeric(data[ ,4]))
        data[ ,5] <- suppressWarnings(as.numeric(data[ ,5]))
        
        ## Check that state and outcome are valid
        if(!(state %in% data$state)) {
                stop("invalid state")
        }
        if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
                stop("invalid outcome")
        }
        
        ## Return hospital in state with specified 30-day death rank
        data <- data[data$state==state, ]
        if(outcome == "heart attack") {
                data <- data[!is.na(data$heart_attack), ]
                ndx <- order(data$heart_attack, data$hospital)
                data <- data[ndx, ]
        } else if (outcome == "heart failure") {
                data <- data[!is.na(data$heart_failure), ]
                ndx <- order(data$heart_failure, data$hospital)
                data <- data[ndx, ]
        } else if (outcome == "pneumonia") {
                data <- data[!is.na(data$pneumonia), ]
                ndx <- order(data$pneumonia, data$hospital)
                data <- data[ndx, ]
        }
        if (num == "best") {rank <- 1}
        else if (num == "worst") {rank<- nrow(data)}
        else if (num > nrow(data)) {return(NA)}
        else {rank <- num}
        rank.hospital <- data$hospital[[rank]]
        rank.hospital
}