rankall <- function(outcome, num = "best") {
## this is probably not the most efficient code but this was written with the goal of avoiding for statements or any loops
## this function relies on comparing data frames to determine the given rank of hospitals in all of the states in the data set

        
## Read outcome data
    df<-read.csv("outcome-of-care-measures.csv")
##Check that state and outcome are valid
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
    if(num=="best"){
        rnk=1
    } else if (num=="worst"){
        rnk<-nrow(df)
    } else {
        rnk<-num
    }
    
#change format of columns and rename for ease of use   
    df[]<-lapply(df,as.character)
    df[,OCol]<-suppressWarnings(as.numeric(as.character(df[,OCol])))
    colnames(df)[OCol]<-"measurement"
    
#subset and remove NAs    
    df<-na.omit(subset(df[,c(2,7,OCol)],na.rm=TRUE))


## For each state, find the hospital of the given rank
#order by state/outcome/hospital name    
    df<-df[order(df$State,df$measurement,df$Hospital.Name),]
    
#Create state Ranking ties.method here dictates that the earlier result will take a lower number
    df$Rank<-ave(df$measurement,df$State,FUN=function(x) rank(x,ties.method = "first"))
    colnames(df)[4]<-"Rank"
    if (num=="worst"){
        g<-tapply(df$Rank,df$State, max)
        mrg_df<-data.frame(State=names(g),Rank=as.numeric(unname(g)))
    } else {
        mrg_df<-data.frame(State=unique(df$State),Rank=rnk)
    }
    
        

## Return a data frame with the hospital names and the
## (abbreviated) state name
    df_return<-merge(x=df,y=mrg_df, by.x=c("State","Rank"), by.y=c("State","Rank"), all.y=TRUE)
    subset(df_return[,c(3,1)])
   
    
}