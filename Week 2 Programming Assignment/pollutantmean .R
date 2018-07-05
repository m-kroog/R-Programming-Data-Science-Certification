pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## create a vector of all the files in the specdata directory
        path <- list.files("C:/coursera/specdata", pattern = "*.csv", full.names = TRUE) 
        ## read all the files in the vector path into a list
        combine_all <- lapply(path, read.csv)
        ## bind all the elements of the list into a data frame
        full_df <- do.call(rbind, combine_all)
        ## subset all the rows which match the argument "id"
        id_subset <- full_df[which(full_df[, "ID"] %in% id),]
        ## subset all the rows which match the argument "pollutant"
        pollutant_subset <- id_subset[[pollutant]]
        ## take the mean
        mean(pollutant_subset, na.rm = TRUE)
}

