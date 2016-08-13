best <- function(state, outcome) {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    states   = unique(outcome_data[,7])
    outcomes = c("heart attack", "heart failure", "pneumonia")
    state   <- toupper(state)
    outcome <- tolower(outcome)
    
    if (!state %in% states)     stop("invalid state")
    if (!outcome %in% outcomes) stop("invalid outcome")
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    if      (outcome == "heart attack")  mortality_rates <- suppressWarnings(as.numeric(outcome_data[,11]))
    else if (outcome == "heart failure") mortality_rates <- suppressWarnings(as.numeric(outcome_data[,17]))
    else                                 mortality_rates <- suppressWarnings(as.numeric(outcome_data[,23]))
    
    mins <- tapply(mortality_rates, outcome_data[,7], function(x) min(x, na.rm = TRUE))
    
    outcome_data[which(mortality_rates == mins[state] & outcome_data[,7] == state), 2]
}
