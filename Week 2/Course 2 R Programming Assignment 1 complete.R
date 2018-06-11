complete <- function(directory, id = 1:332) {
        ## create a vector of all the files in the specdata directory
        path <- list.files("C:/coursera/specdata", pattern = "*.csv", full.names = TRUE)
        ## create a function that reads all files in a vector and takes the sum of the complete cases
        complete_cases <- function(x) {
                sum(complete.cases(read.csv(x)))
        }
        ## runs the comlete_cases function over all elements in path
        nobs <- unlist(lapply(path[id], complete_cases))
        ## create a data frame
        data.frame(id = id, nobs = nobs)
        
}

