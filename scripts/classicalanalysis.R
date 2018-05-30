# select data
cordata <- data[c(psychopathicTraits, executiveFunctions)]

# use corr.test from psych package and create matrix
corp <- as.matrix(corr.test(cordata, adjust = "none")$p)
corpfdr <- as.matrix(corr.test(cordata, adjust = "fdr")$p)
corr <- as.matrix(corr.test(cordata, adjust = "fdr")$r)

# remove diagonal
corp[upper.tri(corp, diag = TRUE)] <- NA
corpfdr[lower.tri(corpfdr, diag = TRUE)] <- NA # upper diagonal contains fdr p-values
corr[upper.tri(corr, diag = TRUE)] <- NA

# revolve around diagonal to make it lower again
corpfdr <- t(corpfdr) 

# melt to long format for ggplot use
corp <- melt(corp)
corpfdr <- melt(corpfdr)
corr <- melt(corr)

# remove NAs
corp <- corp[-which(is.na(corp[, 3])),]
corpfdr <- corpfdr[-which(is.na(corpfdr[, 3])),]
corr <- corr[-which(is.na(corr[, 3])),]

# turn back into data frames
corp <- data.frame(corp)
corpfdr <- data.frame(corpfdr)
corr <- data.frame(corr)

# merge into one data frame
cor <- corp
cor$pfdr <- corpfdr$value
cor$r <- corr$value

# more sensible column names
colnames(cor) <- c("X", "Y", "p", "pfdr", "r")

# reorder levels
cor$X = factor(cor$X, levels = rev(c(psychopathicTraits, executiveFunctions)), ordered = TRUE)
cor$Y = factor(cor$Y, levels = c(psychopathicTraits, executiveFunctions), ordered = TRUE)

# round to two decimal points
cor$p <- round(cor$p, 2)
cor$pfdr <- round(cor$pfdr, 2)
cor$r <- round(cor$r, 2)

# add factor variables indicating statistical significance,
# i.e., p < .05
cor$sigp <- as.factor(ifelse(cor$p < 0.05, "yes", "no"))
cor$sigp2 <- cor$p
cor$sigp2[cor$sigp2 < 0.01] <- "< .01"

cor$sigpfdr <- as.factor(ifelse(cor$pfdr < 0.05, "yes", "no"))
cor$sigpfdr2 <- cor$pfdr
cor$sigpfdr2[cor$sigpfdr2 < 0.01] <- "< .01"

# remove temporary variables
rm(cordata, corp, corpfdr, corr)