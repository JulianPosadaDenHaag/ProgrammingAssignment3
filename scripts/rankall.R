library(here)
library(tidyverse)
library(dplyr)

rankall <- function(outcome, ranking ="best" ) 
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
        
        states<- distinct(outcome_clean[1])
        
        ## Check that state and outcome are valid
        if(!outcome%in%colnames(outcome_clean[3:5])) stop("invalid outcome")
        
        dfResult <- outcome_clean %>% drop_na()
        
        result <- dfResult %>% # Applying row_number function
                select(State,Hospital_Name, outcome) %>% 
                arrange(get(outcome), State,Hospital_Name) %>% 
                group_by(State) 
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        if(ranking == "best")result<- result%>%slice_head() %>% select(Hospital_Name, State)  
        else if(ranking == "worst") result <- result%>% slice_max(get(outcome)) %>% select(Hospital_Name, State)  
        else result <- result%>% slice(ranking)  %>% select(Hospital_Name, State) 
        
        as<- merge(x= states, y=result, by= "State", all = TRUE)
        return(as)
}

