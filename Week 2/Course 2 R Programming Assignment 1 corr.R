corr <- function(directory, threshold = 0) {
        ## create a vector of all the files in the specdata directory
        path <- list.files("C:/coursera/specdata", pattern = "*.csv", full.names = TRUE) 
        ## create an empty numeric vector
        corr_num <- numeric()
        ## create a function that reads in and finds the correlation of columns 2 and 3
        corr <- function(x) {
                corr_read <- na.omit(read.csv(x)[2:3])
                corr_df <- cor(corr_read$sulfate, corr_read$nitrate)
                corr_num <- c(corr_num, as.vector(corr_df))
        }
        ## apply the function corr over the variable path
        corr_vec <- unlist(lapply(path, corr))
        
        ## create a function that reads all files in a vector and takes the sum of the complete cases
        complete <- function(x) {
                sum(complete.cases(read.csv(x)))
        }
        ## apply the complete function over the variable path
        nobs <- unlist(lapply(path, complete))
        ## create a data frame
        nobs <- data.frame(threshold = nobs, correlation = corr_vec)
        ## subset the argument "threshold"
        nobs <- nobs[which(nobs[, "threshold"] > threshold),]
        ## subset the column "correlation"
        nobs <- as.numeric(unlist(nobs["correlation"]))
}
