rankall <- function(outcome, num = "best") {
        rankall <- data.frame()
        OutcomeRead <- read.csv("outcome-of-care-measures.csv", colClasses="character")   ## Read outcome data; important: set colClasses to character, 
        ##else the various mortality rate columns will be read as factor, and will screw up the order function
        
        StateList <- names(table(OutcomeRead$State))     ##Lists all the states present
        ##table function creates the freq table of States
        ##the states will be the names of this freq table but it is in a list form
        ##convert the list of state names to character
        ## Check that state and outcome are valid
        
        
        TestOutcome <- grep(outcome,c("heart attack","heart failure","pneumonia")) ##Check outcome
        
        
        if (length(TestOutcome) == 0){
                stop("invalid outcome")   
        }
        
        ## For each state, find the hospital of the given rank
        
        NormOutcome <- gsub(" ", "_",outcome) #normalize the "outcome" input by replacing " " with "_" 
        ReduceOR <- OutcomeRead[,c(2,7,11,17,23)] #select columns of interest
        names(ReduceOR) <- c("Hospital","State","heart_attack","heart_failure","pneumonia") ##rename columns
        
        for(i in 1:length(StateList)){
                state <- StateList[[i]]
                
                ReduceOR2 <- ReduceOR[ReduceOR$State==state,c("Hospital","State",NormOutcome)] ##select State of interest
                ReduceOR2 <- ReduceOR2[ReduceOR2[,3]!="Not Available",]  ##remove entries with "Not Available"
                ReduceOR3 <- data.frame(as.character(ReduceOR2[,1]),as.numeric(ReduceOR2[,3]),as.character(ReduceOR2[,2]))  #construct new dataframe with the right classes
                names(ReduceOR3) <- c("Hospital",NormOutcome,"State")   #rename ReduceOR3
                
                OrderHospital <- order(ReduceOR3[,2],ReduceOR3[,1], decreasing=F)  #find new order using column 2 while column 1 as the tie-breaker
                CompareHospital <- ReduceOR3[OrderHospital,]   ##re-order ReduceOR2
                
                if(num=="best"){
                        rankallEntry <- data.frame(as.character(CompareHospital[1,1]),as.character(CompareHospital[1,3]))
                } else if (num=="worst"){
                        LastVal <- nrow(CompareHospital)
                        rankallEntry <- data.frame(as.character(CompareHospital[LastVal,1]),as.character(CompareHospital[LastVal,3]))
                } else {
                        rankallEntry <- data.frame(as.character(CompareHospital[num,1]),as.character(CompareHospital[num,3]))
                }
                
                rankall <- rbind(rankall,rankallEntry)
        
        }
        
        names(rankall) <- c("hospital","state")
        rankall
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
}
