# describe data
table1 <- data.frame(describe(data))

# add variable names
table1$variable <- row.names(table1)

# reorder keeping only relevant columns,
# round numerical columns to two decimal points
table1 <- table1[, c(14, 3:4, 8:9)]
table1[, c(2:5)] <- round(table1[, c(2:5)], 2)

# rename column headings
names(table1) <- c("Variable", "Mean", "SD", "Min", "Max")

gd_results <- gs_key(KEY)
gd_results <- gd_results %>%
  gs_edit_cells(ws = "table1", input = table1)

rm(table1)
