library(here)
library(tidyverse)
library(dplyr)

rankhospital <- function(state, outcome, ranking = "best" ) 
{
        ## Read outcome data
        df_outcome <- read.csv(here::here("data", 
                                          "raw_data", 
                                          "outcome-of-care-measures.csv"), 
                               colClasses = "character")
        
        
        df_outcome[df_outcome == "Not Available"] <- NA
        
        
        outcome_clean <- df_outcome%>% 
                select(State,Hospital.Name,starts_with("Hospital.30.Day.Death"))
        
        for (i in 3:5)
        {
                outcome_clean[, i] <- as.numeric(outcome_clean[, i])
        }
        
        colnames(outcome_clean) = c("State","Hospital_Name", "heart attack", "heart failure", "pneumonia")
        
        ## Check that state and outcome are valid
        if(!outcome%in%colnames(outcome_clean[3:5])) stop("invalid outcome")
        
        dfResult <- outcome_clean %>% drop_na()
        
        states<- dfResult %>% 
                select(State) %>% 
                unique()
        
        if (!(state %in% states$State)) stop("invalid State")
        
        result <- dfResult %>% 
                filter(State == state) %>% 
                arrange(get(outcome))%>%                       # Applying row_number function
                mutate(rank = row_number())%>% 
                group_by(State)%>% 
                select(Hospital_Name, outcome, rank)
        
        x<- result%>%slice_max(rank)
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        if(ranking == "best")result<- result%>%slice(1)  
        else if(ranking == "worst") result <- result%>% slice_max(get(outcome))  
        else if (ranking < x["rank"]) result <- result%>% slice(ranking) 
        else (return (NA))
        
        
        return(result)
}


