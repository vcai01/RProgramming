## 2. Find the best hospital in a state: best.R
data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors=FALSE)
# select column by position
data_selected <- data[c(2, 7, 11, 17, 23)]
names(data_selected) <- c("Hospital.Name", "State", "heart attack", "heart failure", "pneumonia")

best <- function(state, outcome) {
    dat <- select(data_selected, c("Hospital.Name", "State", outcome))
    state_data <- filter(dat, State == state)
    # it will generate wrong output if na.omit() is done right after read.csv()
    state_df <- na.omit(state_data)
    state_list <- unique(state_data$State)
    disease <- c("heart attack", "heart failure", "pneumonia")
    if(!(state %in% state_list)) {
        return(stop("invalid state"))
    }
    if(outcome == "heart attack") {
        data_arranged <- arrange(state_df, `heart attack`)
        return(data_arranged[1,1])
    }
    if(outcome == "heart failure") {
        data_arranged <- arrange(state_df, `heart failure`)
        return(data_arranged[1,1])
    }
    if(outcome == "pneumonia") {
        data_arranged <- arrange(state_df, pneumonia)
        return(data_arranged[1,1])
    }
    if(!(outcome %in% disease)) {
        return(stop("invalid outcome"))
    }
}


## 3. Ranking hospitals by outcome in a state: rankhospital.R
library(dplyr)
data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors=FALSE)
# select column by position
data_selected <- data[c(2, 7, 11, 17, 23)]
# rename columns
names(data_selected) <- c("Hospital.Name", "State", "heart attack", "heart failure", "pneumonia")

rankhospital <- function(state, outcome, num = "best") {
    # select related outcome: eg. heart attack, or heart failure, or pneumonia
    dat <- select(data_selected, c("Hospital.Name", "State", outcome))
    df <- na.omit(dat)
    # filter data of a specific state
    state_data <- filter(df, State == state)
    # sort the data according to the outcome which are rates
    data_sorted <- state_data[order(state_data[outcome], state_data$Hospital.Name),]
    if(num == "best") {
        return(data_sorted$Hospital.Name[1])
    }
    if(num == "worst") {
        return(data_sorted$Hospital.Name[nrow(data_sorted)])
    }
    if(num <= nrow(data_sorted)) {
        return(data_sorted$Hospital.Name[num])
    }
    if(num > nrow(data_sorted)) {
        return(print(NA))
    }
}

# rankhospital("TX", "heart failure", 4)


## 4. Ranking hospitals in all states: rankall.R
library(dplyr)
data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors=FALSE)
data_selected <- data[c(2, 7, 11, 17, 23)]
names(data_selected) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")

rankall <- function(outcome, num = "best") {
    dat <- select(data_selected, c("hospital", "state", outcome))
    # important to do na.omit after selecting the right variables
    df <- na.omit(dat)
    df_sorted <- df[order(df$state, df[outcome], df$hospital),]
    state_data <- split(df_sorted, df_sorted$state)
    if(num == "best") {
        # use USE.NAMES=TRUE with sapply and get a named vector
        hospital <- sapply(state_data, function(x) x[1, 1], USE.NAMES = TRUE)
    }
    if(num == "worst") {
        hospital <- sapply(state_data, function(x) x[nrow(x), 1], USE.NAMES = TRUE)
    } else {
        hospital <- sapply(state_data, function(x) x[num, 1], USE.NAMES = TRUE)
    }
    # *and use the vector’s names to get states
    return(data.frame(hospital, state=names(hospital), row.names=names(hospital)))
}
