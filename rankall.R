## Function for identifying best hospital by state and outcome
rankall <- function(outcome, num = "best") {

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
        
        ## Check that outcome is valid
        if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
                stop("invalid outcome")
        }
        
        ## Return hospitals in all states with specified 30-day death rank
        if(outcome == "heart attack") {
                sub.data <- data[!is.na(data$heart_attack), c(1, 2, 3)]
        } else if (outcome == "heart failure") {
                sub.data <- data[!is.na(data$heart_failure), c(1, 2, 4)]
        } else if (outcome == "pneumonia") {
                sub.data <- data[!is.na(data$pneumonia), c(1, 2, 5)]
        }
        colnames(sub.data) <- c("hospital", "state", "value")
        outframe = data.frame(hospital=character(), state=character())
        for(state in unique(sub.data$state)) {
                for.data <- sub.data[sub.data$state==state, ]
                ndx <- order(for.data$value, for.data$hospital)
                for.data <- for.data[ndx, ]
                if (is.numeric(num) & num > nrow(for.data)) {
                        outln <- c(NA, as.character(state))}
                else {
                        if (num == "best") {rank <- 1}
                        else if (num == "worst") {rank<- nrow(for.data)}
                        else {rank <- num}
                        outln <- for.data[rank, c("hospital", "state")]
                }
                outframe <- rbind(outframe, outln)
        }
        rownames(outframe) <- outframe$state
        ndx2 <- order(outframe$state)
        outframe <- outframe[ndx2, ]
        outframe
}