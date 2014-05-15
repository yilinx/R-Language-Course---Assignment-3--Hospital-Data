rankhospital <- function(state, outcome, num = "best") {
        OutcomeRead <- read.csv("outcome-of-care-measures.csv", colClasses="character")   ## Read outcome data; important: set colClasses to character, 
        ##else the various mortality rate columns will be read as factor, and will screw up the order function
        
        StateList <- as.character(names(table(OutcomeRead$State)))      ##Lists all the states present
        ##table function creates the freq table of States
        ##the states will be the names of this freq table but it is in a list form
        ##convert the list of state names to character
        ## Check that state and outcome are valid
        
        TestStates <- grep(state,StateList)   ## Check States
        TestOutcome <- grep(outcome,c("heart attack","heart failure","pneumonia")) ##Check outcome
        
        if (length(TestStates) == 0){
                stop("invalid state")   
        }
        
        if (length(TestOutcome) == 0){
                stop("invalid outcome")   
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        NormOutcome <- gsub(" ", "_",outcome) #normalize the "outcome" input by replacing " " with "_" 
        ReduceOR <- OutcomeRead[,c(2,7,11,17,23)] #select columns of interest
        names(ReduceOR) <- c("Hospital","State","heart_attack","heart_failure","pneumonia") ##rename columns
        ReduceOR <- ReduceOR[ReduceOR$State==state,c("Hospital","State",NormOutcome)] ##select State of interest
        ReduceOR <- ReduceOR[ReduceOR[,3]!="Not Available",]  ##remove entries with "Not Available"
        ReduceOR2 <- data.frame(as.character(ReduceOR[,1]),as.numeric(ReduceOR[,3]))  #construct new dataframe with the right classes
        names(ReduceOR2) <- c("Hospital",NormOutcome)   #rename ReduceOR2
        OrderHospital <- order(ReduceOR2[,2],ReduceOR2[,1], decreasing=F)  #find new order using column 2 while column 1 as the tie-breaker
        CompareHospital <- ReduceOR2[OrderHospital,]   ##re-order ReduceOR2
        
        if(num=="best"){
                as.character(CompareHospital[1,1])
        } else if (num=="worst"){
                LastVal <- nrow(CompareHospital)
                as.character(CompareHospital[LastVal,1])
        } else {
                as.character(CompareHospital[num,1])
        }
        
}