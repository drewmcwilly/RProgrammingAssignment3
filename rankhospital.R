rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    df<-read.csv("outcome-of-care-measures.csv")
    ## Check that state and outcome are valid
    
    if(!is.element(state,df$State)){
        stop('invalid state')
        
    }
    if(!is.element(outcome,c('heart attack','heart failure','pneumonia'))){
        stop('invalid outcome')
    }
    OCol<-0
    if (outcome=="heart attack"){
        OCol<-11
    } else if(outcome=="heart failure"){
        OCol<-17
    }else{
        OCol<-23
    }
    ##force measurement to numeric and remove nas
    df[,OCol]<-suppressWarnings(as.numeric(as.character(df[,OCol])))
    colnames(df)[OCol]<-outcome
    df<-na.omit(subset(df[,c(2,7,OCol)],State==state,na.rm=TRUE))
    
    
    ##create data frame with ordering by rank and then hospital name
    df_Rank<-df[order(df[3],df$Hospital.Name),]
    
    
    ##create data frame ordered hospitals and add rank field
    Rank<-1:nrow(df_Rank)
    df_return<-data.frame(df_Rank,Rank)
    if(num=="best"){
        num=1
    } else if (num=="worst"){
        num<-nrow(df)
    } else {
        num<-num
    }
        
    ## Return hospital name in that state with the given rank
  df_return[num,]
   
    
}
