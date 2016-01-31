best <- function(state, outcome) {
    ## check the outcome desired


    ## Read outcome data
    df<-read.csv("outcome-of-care-measures.csv")
        
    ## Check that state and outcome are valid
    if(!is.element(state,df$State)){
        stop('invalid state')
        
    }
    if(!is.element(outcome,c('heart attack','heart failure','pneumonia'))){
        stop('invalid outcome')
    }
    OCol=0
    if (outcome=="heart attack"){
        OCol<-11
        
    } else if(outcome=="heart failure"){
        OCol<-17
    }else{
        OCol<-23
    }
    
    ##Subset data frame to only contain state, hospital and outcome
    df<-subset(df[,c(2,7,OCol)],State==state)
    colnames(df)[3]<-outcome
    ## Return hospital name in that state with lowest 30-day death
    idx <- which.min(suppressWarnings(as.numeric(as.character(df[,3]))))
    df[idx,]

    
}