rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that outcome is valid
    outcomes = c("heart attack", "heart failure", "pneumonia")
    outcome <- tolower(outcome)
    
    if (!outcome %in% outcomes) stop("invalid outcome")
    
    ## For each state, find the hospital of the given rank
    if      (outcome == "heart attack")  outcome_col <- 11
    else if (outcome == "heart failure") outcome_col <- 17
    else                                 outcome_col <- 23
    
    outcome_data[, outcome_col] <- suppressWarnings(as.numeric(outcome_data[ ,outcome_col]))
    parsed_data <- outcome_data[, c(2, 7, outcome_col)]
    complete_cases <- parsed_data[complete.cases(parsed_data), ]
    cases_by_state <- split(complete_cases, complete_cases[, 2])
    sorted_by_rate <- lapply(cases_by_state, function(x) x[order(x[ ,3], x[ ,1]), ])
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    if (num == "best")
        nth_hospital <- sapply(sorted_by_rate, function(x) x[1, 1:2])
    else if (num == "worst")
        nth_hospital <- sapply(sorted_by_rate, function(x) x[length(x[[1]]), 1:2])
    else
        nth_hospital <- sapply(sorted_by_rate, function(x) c(x[num, 1], x[1, 2]))
    
    nth_hospital_df <- data.frame(t(nth_hospital))
    colnames(nth_hospital_df) <- c("hospital", "state")
    nth_hospital_df
}
