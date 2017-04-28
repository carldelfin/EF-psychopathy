# create a dataframe using variables 18 and 4-14
table1 <- data.frame(describe(data[c(18, 4:14)]))

# remove columns we don't want, 
# and only retain mean, SD, median, min and max
table1 <- table1[-c(1,2,6,7,10,11,12,13)]

# rename column headings
names(table1) <- c("Mean", "SD", "Median", "Min", "Max")
