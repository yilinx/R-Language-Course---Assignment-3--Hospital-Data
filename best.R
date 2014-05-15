best <- function(state, outcome) {
        OutcomeRead <- read.csv("outcome-of-care-measures.csv")   ## Read outcome data
        
        StateList <- as.character(names(table(OutcomeRead$State)))      ##Lists all the states present
                                                                        ##table function creates the freq table of States
                                                                        ##the states will be the names of this freq table but it is in a list form
                                                                        ##convert the list of state names to character
        ## Check that state and outcome are valid
        
        TestStates <- grep(state,StateList)   
        TestOutcome <- grep(outcome,c("heart attack","heart failure","pneumonia"))
        
        if (length(TestStates) == 0){
                stop("invalid state")   
        }
        
        if (length(TestOutcome) == 0){
                stop("invalid outcome")   
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        
}
