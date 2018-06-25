rankhospital <- function(state, outcome, num = "best") {
        outcome_data <- read.csv("c:/coursera/outcome-of-care-measures.csv",
                            na.strings = "Not Available", stringsAsFactors = FALSE)
        
        care_df <- as.data.frame(outcome_data[ , c(2, 7, 11, 17, 23)])
        colnames(care_df) <- c("name", "state", "heart attack", "heart failure", "pneumonia")
        
        if (!any(care_df[2] == state)) stop("invalid state")
        if (!any(outcome %in% care_names)) stop("invalid outcome")
        
        care_names <- c("heart attack", "heart failure", "pneumonia")
        care_df_update <- care_df[,1:2]
        care_df_update["condition"] <- care_df[outcome]
        
        care_df_state <- care_df_update[which(care_df_update[, "state"] %in% state),]
        care_df_order <- care_df_state[order(care_df_state$condition, care_df_state$name,
                                                     decreasing = FALSE),]
        care_df_order <- na.omit(care_df_order)
        
        if (num %in% "best") {
                print(care_df_order[1, 1])        
        } else if (num %in% "worst") {
                worst <- tail(care_df_order, 1)
                print(worst[1, 1])
        } else if (num %in% 1:nrow(care_df_order)) {
                rank <- care_df_order[num,]
                print(rank[,1])
        } else {
                return(NA)
        }
}