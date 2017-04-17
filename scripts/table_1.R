# create a dataframe using variables 18 and 4-14
table1 <- data.frame(describe(data[c(18, 4:14)]))

# columns 1, 2, 6, 7 and 10 from the describe output
# correspond to vars, n, trimmed mean, mad and range
# and are fairly superflous and thus removed
table1 <- table1[-c(1,2,6,7,10,11,12,13)]

# rename column headings
names(table1) <- c("Mean", "SD", "Median", "Min", "Max")
