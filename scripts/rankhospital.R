
library(here)
library(tidyverse)

rankhospital <- function(state, outcomes, num = "best")
{
        
        ## Read outcome data
        outcome <- read.csv(here::here("data", 
                                       "raw_data", 
                                       "outcome-of-care-measures.csv"), 
                            colClasses = "character")
        
        outcome[outcome == "Not Available"] <- NA
        
        outcome_clean <- outcome%>% 
                select(State,Hospital.Name,starts_with("Hospital.30.Day.Death"))
        
        for (i in 3:5)
        {
                outcome_clean[, i] <- as.numeric(outcome_clean[, i])
        }
        
        colnames(outcome_clean) = c("State","Hospital_Name", "heart_attack", "heart_failure", "pneumonia")
        
        ## Check that state and outcome are valid
        dfResult <- outcome_clean %>% 
                drop_na()
        
        if (outcomes == "heart attack") {result <- dfResult %>%
                select(State, Hospital_Name, heart_attack)%>% 
                arrange(heart_attack,Hospital_Name)}
        
        else if (outcomes == "heart failure") {result <- dfResult %>%
                select(State, Hospital_Name, heart_failure) %>% 
                arrange(heart_failure, Hospital_Name)}
        
        else if (outcomes == "pneumonia") {result <- dfResult %>% 
                select(State, Hospital_Name, pneumonia)%>% 
                arrange(pneumonia,Hospital_Name)}
        
        else stop("invalid outcome")
        
        states<- dfResult %>% 
                select(State) %>% 
                unique()
        
        if (!(state %in% states$State)) stop("Not a valid State")
        
        result <- result %>% 
                filter(State == state) 
        
        if (num == "best") ranking<- 1
        
        else if (num == "worst") ranking<- as.numeric(length(result$Hospital_Name))
        
        else  ranking<- as.numeric(num) 
        
        
        return(result)
        
}





