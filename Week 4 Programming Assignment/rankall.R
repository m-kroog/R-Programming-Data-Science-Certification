rankall <- function(outcome, num = "best") {
        outcome_data <- read.csv("c:/coursera/outcome-of-care-measures.csv",
                                 na.strings = "Not Available", stringsAsFactors = FALSE)
        
        ## subset relevant data
        care_df <- data.frame(outcome_data[ , c(2, 7, 11, 17, 23)])
        ## assign column names
        colnames(care_df) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
        
        if (!any(outcome %in% colnames(care_df[3:5]))) stop("invalid outcome")
        
        ## create data frame with name, state and outcome data
        care_df_update <- care_df[,1:2]
        care_df_update["condition"] <- care_df[outcome]                
        
        ## order outcome data and split into a list
        care_df_list <- care_df_update[order(care_df_update$state, care_df_update$condition, decreasing = FALSE),] 
        care_df_list <- split(care_df_list, care_df_list$state)

        ## set conditionals for the best, worst and rank
        if (num %in% "best") {
        best <- lapply(care_df_list, head, 1)
        best <- do.call(rbind, best)
        best <- best[1:2]
        best
        } else if (num %in% "worst") {
        worst <- lapply(care_df_list, na.omit)
        worst <- lapply(worst, tail, 1)
        worst <- do.call(rbind, worst)
        worst <- worst[1:2]
        worst
        } else {
        rank <- lapply(care_df_list, function(x) return(x[num,]))
        rank <- do.call(rbind, rank)
        rank <- rank[1:2]
        rank
        }
}
