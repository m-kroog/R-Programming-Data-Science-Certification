best <- function(state, outcome) {
        outcome_data <- read.csv("c:/coursera/outcome-of-care-measures.csv",
                                 na.strings = "Not Available", stringsAsFactors = FALSE)        
        ## subset relevant data
        care_df <- as.data.frame(outcome_data[ , c(2, 7, 11, 17, 23)])
        ## assign column names
        colnames(care_df) <- c("name", "state", "heart attack", "heart failure", "pneumonia")
        ## assign outcome names
        care_names <- colnames(care_df)[3:5]
        ## set conditions for invalid state and outcome
        if (!any(care_df[2] == state)) stop("invalid state")
        if (!any(outcome %in% care_names)) stop("invalid outcome")
        ## subset state that matches argument
        care_df_state <- care_df[care_df[, "state"] %in% state,]
        ## create a dataframe containing name, state and outcome data
        care_df_final <- data.frame(care_df_state[1], care_df_state[2])
        care_df_final["outcome"] <- care_df_state[outcome]
        ## subset outcome that matches argument
        results <- care_df_final[which.min(care_df_final[, "outcome"]),]
        ## print hospital name        
        results[,1]
}