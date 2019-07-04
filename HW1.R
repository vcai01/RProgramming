df = read.csv("hw1_data.csv")

#Q16: How many missing values are in the Ozone column of this data frame?
ozone <- df$Ozone
bad <- is.na(ozone)
sum(bad)

#Q17: What is the mean of the Ozone column in this dataset? Exclude missing values (coded as NA) from this calculation.
mean(df$Ozone, na.rm = True) 

#Q18: Extract the subset of rows of the data frame where Ozone values are above 31 and Temp values are above 90. What is the mean of Solar.R in this subset?
lst1 <- subset(df, Ozone > 31 & Temp > 90) 
mean(lst1$Solar.R)

#Q19: What is the mean of "Temp" when "Month" is equal to 6?
lst2 <- subset(df, Month == 6)
mean(lst2$Temp)

#Q20: What was the maximum ozone value in the month of May (i.e. Month is equal to 5)?
lst3 <- subset(df, Month == 5)
max(lst3$Ozone, na.rm = TRUE)
