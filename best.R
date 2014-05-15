best <- function(state, outcome) {
        OutcomeRead <- read.csv("outcome-of-care-measures.csv")   ## Read outcome data
        
        StateList <- as.character(names(table(OutcomeRead$State)))    ##Lists all the states present
        TestStates <- grep(state,StateList)   ## Check that state and outcome are valid
        
        if (length(TestStates) == 0){
                ErrStates <- paste("Error in best(",state,", ", outcome,") : invalid states")
                stop("invalid state")
                
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        
}
