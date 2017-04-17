# create new column in data called 'group',
# and att labels based on occurence of SUD and ADHD
data$group <- "Neither ADHD nor SUD"
data$group[data$ADHD=="yes"] <- "ADHD only"
data$group[data$SUD=="yes"] <- "SUD only"
data$group[data$ADHDandSUD=="yes"] <- "ADHD and SUD"

# reorder group levels
data$group <- factor(data$group, ordered = TRUE,
                    levels = c("Neither ADHD nor SUD",
                               "ADHD only",
                               "SUD only",
                               "ADHD and SUD"))

# create table
t2 <- table(data$group)

# calculate percentage (with no decimals)
p2 <- round(prop.table(t2) * 100, 0)

# bind them together, rename, and output using pander
table2 <- rbind(t2, p2)
row.names(table2) <- c("Frequency", "Percent")

