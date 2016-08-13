rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    states   = unique(outcome_data[,7])
    outcomes = c("heart attack", "heart failure", "pneumonia")
    state   <- toupper(state)
    outcome <- tolower(outcome)
    
    if (!state %in% states)     stop("invalid state")
    if (!outcome %in% outcomes) stop("invalid outcome")
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    if      (outcome == "heart attack")  mortality_rates <- suppressWarnings(as.numeric(outcome_data[,11]))
    else if (outcome == "heart failure") mortality_rates <- suppressWarnings(as.numeric(outcome_data[,17]))
    else                                 mortality_rates <- suppressWarnings(as.numeric(outcome_data[,23]))
    
    rates_by_state <- split(mortality_rates, outcome_data[,7])
    state_rates <- rates_by_state[state]
    
    hospital_name_rate <- data.frame(hospital_name = outcome_data[outcome_data[,7] == state, 2],
                                     state_rates)
    hospital_name_rate_complete <- hospital_name_rate[complete.cases(hospital_name_rate), ]
    sorted_by_rate <- hospital_name_rate_complete[order(hospital_name_rate_complete[,2],
                                                        hospital_name_rate_complete[,1]), ]
    
    if (num == "best")
        return(as.character(sorted_by_rate[1,1]))
    else if (num == "worst")
        return(as.character(sorted_by_rate[nrow(sorted_by_rate),1]))
    else
        return(as.character(sorted_by_rate[num,1]))
}
