#Part 1: pollutantmean.R
#Write a function named 'pollutantmean' that calculates the mean of a pollutant 
#(sulfate or nitrate) across a specified list of monitors. The function 
#'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
#Given a vector monitor ID numbers, 
#'pollutantmean' reads that monitors' particulate matter data from the directory 
#specified in the 'directory' argument and returns the mean of the pollutant 
#across all of the monitors, ignoring any missing values coded as NA. 
#A prototype of the function is as follows

pollutantmean <- function(directory, pollutant, id = 1:332) {
        location <- paste(getwd(), directory, sep="/")
        filenames <- list.files(location, full.names = TRUE)
        dat <- data.frame()
        for(i in id) {
                data <- read.csv(filenames[i], header = TRUE, sep = ",")
                dat <- rbind(dat, data)
        }
        mean(dat[, pollutant], na.rm = TRUE)
}
#pollutantmean("specdata", "sulfate", 1:10)
#pollutantmean("specdata", "nitrate", 70:72)
#pollutantmean("specdata", "nitrate", 23)



Part 2: complete.R
#Write a function that reads a directory full of files and reports 
#the number of completely observed cases in each data file. 

#The function should return a data frame where the first column is 
#the name of the file and the second column is the number of complete cases.

complete <- function(directory, id = 1:332) {
    location <- paste(getwd(), directory, sep="/")
    filenames <- list.files(location)
    dat <- data.frame()
    for(i in id) {
            data <- read.csv(filenames[i], stringsAsFactors = FALSE)
            data <- na.omit(data)
            df <- data.frame("id" = i, "nobs" = nrow(data))
            dat <- rbind(dat, df)
    }
    dat
}



#Part 3: corr.R
#Write a function that takes a directory of data files and a threshold 
#for complete cases and calculates the correlation between sulfate and nitrate 
#for monitor locations where the number of completely observed cases 
#(on all variables) is greater than the threshold. 

#The function should return a vector of correlations for the monitors 
#that meet the threshold requirement. If no monitors meet the threshold requirement, 
#then the function should return a numeric vector of length 0. 

corr <- function(directory, threshold = 0) {
                path <- paste(getwd(), directory, sep = "/")
                filenames <- list.files(path)
                correlations <- vector(mode = "numeric", length = 0)
                for(i in 1:332) {
                        data <- read.csv(filenames[i])
                        data <- na.omit(data)
                        number <- nrow(data)
                        if (number > threshold) {
                                correlations <- c(correlations, cor(data$sulfate, data$nitrate))
                        }
                }
                correlations
}
