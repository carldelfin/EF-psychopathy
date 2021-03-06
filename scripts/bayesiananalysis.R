# subset data to contain only those variables we want to correlate
cordata <- data[c(psychopathicTraits, executiveFunctions)]

# temporary variables for storage inside the loop(s)
corList <- NULL
tempCor <- NULL
tempRes <- NULL

# set counters to keep track of how long MCMC takes
k <- 1
l <- length(unique(cordata)) * length(unique(cordata)) - length(unique(cordata))

set.seed(seed)

# start MCMC loop
# (I know, I know, nested for loops are the devils work)
for (i in unique(names(cordata))) {
  # tempdata is cordata minus i, so that we don't correlate across the diagonal
  tempdata <- cordata[, !(names(cordata) %in% i)]
  for (j in unique(names(tempdata))) {
    # variable names
    var1 <- i
    var2 <- j
    # temporary storage for correlation data
    tempCor <- jzs_cor(cordata[, i],
                       tempdata[, j],
                       alternative = c("two.sided"),
                       n.iter = niter,
                       n.burnin = nburnin,
                       standardize = TRUE)
    
    # temporary storage for extracted data
    tempRes <- as.data.frame(cbind(
      var1 = var1,
      var2 = var2,
      correlation = tempCor$Correlation,
      posteriorprob = tempCor$PosteriorProbability,
      bayesfactor = tempCor$BayesFactor))
    
    # store in nested list
    corList[[i]][[j]] <- tempRes
    
    # notify that we're done with this round of MCMC
    cat(paste("\n---------- MCMC number", k, "out of", l, "is complete! -----------\n\n"))
    
    # add one to counter
    k <- k + 1
    
    # rinse and repeat!
  }
}

# unlist the nested list
unlisted = lapply(corList, rbindlist)

# melt into long format for use with ggplot2
result <- reshape2::melt(unlisted,
                         id.vars = c("var1",
                                     "var2",
                                     "correlation",
                                     "posteriorprob",
                                     "bayesfactor"))

result <- as.data.frame(result)

# a variable called L1 is added when melting, 
# I don't have time to figure out why,
# I'll just remove it and carry on :-)
result <- subset(result, select = -c(L1))

# round to two decimal points
result$correlation <- round(as.numeric(
  levels(result$correlation))[result$correlation], 2)

result$posteriorprob <- round(as.numeric(
  levels(result$posteriorprob))[result$posteriorprob], 2)

result$bayesfactor <- round(as.numeric(
  levels(result$bayesfactor))[result$bayesfactor], 2)

# order variables (this probably looks weird, but I want to match a specific
# format which requires some unintuitive reversing and coordinate flipping)
result$var1 = factor(result$var1,
                     levels = rev(c(psychopathicTraits, executiveFunctions)),
                     ordered = TRUE)

result$var2 = factor(result$var2,
                     levels = c(psychopathicTraits, executiveFunctions),
                     ordered = TRUE)

# create nameVals variable to use in subsequent matrix creation
nameVals <- sort(unique(unlist(result[1:2])))

# construct zero matrix of correct dimensions with row and column names
corMat <- matrix(0,
                 length(nameVals),
                 length(nameVals),
                 dimnames = list(nameVals, nameVals))

probMat <- matrix(0,
                 length(nameVals),
                 length(nameVals),
                 dimnames = list(nameVals, nameVals))

bayesMat <- matrix(0,
                   length(nameVals),
                   length(nameVals),
                   dimnames = list(nameVals, nameVals))

# fill the matrix indexing on row and column names
corMat[as.matrix(result[c("var1", "var2")])] <- result[["correlation"]]
probMat[as.matrix(result[c("var1", "var2")])] <- result[["posteriorprob"]]
bayesMat[as.matrix(result[c("var1", "var2")])] <- result[["bayesfactor"]]

# add NAs to diagnoal
corMat[lower.tri(corMat, diag = TRUE)] <- NA
probMat[lower.tri(probMat, diag = TRUE)] <- NA
bayesMat[lower.tri(bayesMat, diag = TRUE)] <- NA

# melt to long format (again)
corDat <- melt(corMat)
probDat <- melt(probMat)
bayesDat <- melt(bayesMat)

# remove NAs
corDat <- corDat[-which(is.na(corDat[, 3])),]
probDat <- probDat[-which(is.na(probDat[, 3])),]
bayesDat <- bayesDat[-which(is.na(bayesDat[, 3])),]

# turn into data frames
result_corr <- data.frame(corDat)
result_prob <- data.frame(probDat)
result_bayes <- data.frame(bayesDat)

# merge the two dataframes into one
result_corr$bayes <- result_bayes$value
result_corr$prob <- result_prob$value

colnames(result_corr) <- c("X",
                           "Y",
                           "cor",
                           "bayes",
                           "prob")
result_full <- result_corr

# reorder levels
result_full$X = factor(result_full$X,
                       levels = rev(c(psychopathicTraits, executiveFunctions)),
                       ordered = TRUE)

result_full$Y = factor(result_full$Y,
                       levels = c(psychopathicTraits, executiveFunctions),
                       ordered = TRUE)

# add binary factor variable indicating probability (i.e., posterior probability > or < .50)
result_full$prob2 <- as.factor(ifelse(result_full$prob > 0.5, "yes", "no"))

# posterior probabilities rounded to 1 are brought down a notch to .99
result_full$prob3 <- result_full$prob
result_full$prob3[result_full$prob3 == 1] <- 0.99

# remove temporary stuff
rm(cordata, corList, tempCor, tempRes, var1, var2, i, j, k, l, unlisted,
   tempdata, corDat, probDat, bayesDat, result, nameVals, corMat, probMat, bayesMat,
   result_corr, result_prob, result_bayes, nburnin, niter)