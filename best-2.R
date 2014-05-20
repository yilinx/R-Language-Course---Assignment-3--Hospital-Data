best <- function(state, outcome) {
        readfile <- "outcome-of-care-measures.csv"
        OutcomeRead <- read.csv(readfile, na.strings="Not Available")## Read outcome data
        
        StateList <- as.character(names(table(OutcomeRead$State)))
        TestStates <- grep(state,StateList)   ## Check States
        TestOutcome <- grep(outcome,c("heart attack","heart failure","pneumonia")) ##Check outcome
        
        if (length(TestStates) == 0){
                stop("invalid state")   
        }
        
        if (length(TestOutcome) == 0){
                stop("invalid outcome")   
        }
        
        
        ReduceOR <- OutcomeRead[,c(2,7,11,17,23)]
        names(ReduceOR) <- c("Hospital","State","heart attack","heart failure","pneumonia")
        ReduceOR2 <- subset(ReduceOR, State==state, select=c("Hospital",outcome))
        ReduceOR2 <- ReduceOR2[complete.cases(ReduceOR2[,outcome]),]
        ##bestHosp <- ReduceOR2[ReduceOR2[,outcome]==min(ReduceOR2[,outcome]),] 
        bestVal <- min(ReduceOR2[,outcome],na.rm=T)
        bestHosp <- ReduceOR2[outcome=BestVal,]
        
        bestHosp
        ## Check that state and outcome are valid
        ## Return hospital name in that state with lowest 30-day death
        ## rate
}