regressionsummary <- as.list(NULL)

for (i in 1:5) {
  regressionsummary[i] <- capture.output(cat(paste0(
    outcomeVariables[i],
    ": ",
    "F(",
    modelWald[[i]]$Res.Df[1],
    ",",
    abs(modelWald[[i]]$Df[2]),
    ") = ",
    round(modelWald[[i]]$F[2], 2),
    ", p = ",
    round(modelWald[[i]]$`Pr(>F)`[2], 3),
    ", R2CV = ",
    round(mean(modelCV[[i]]$resample$Rsquared), 3),
    ", 95% CI [",
    round(mean(modelCV[[i]]$resample$Rsquared) - 2 * (sd(modelCV[[i]]$resample$Rsquared)/sqrt(length(modelCV[[i]]$resample$Rsquared))), 3),
    ", ",
    round(mean(modelCV[[i]]$resample$Rsquared) + 2 * (sd(modelCV[[i]]$resample$Rsquared)/sqrt(length(modelCV[[i]]$resample$Rsquared))), 3),
    "]",
    sep = "")))
}