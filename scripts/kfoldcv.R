# from http://stackoverflow.com/a/16030020/3980197
# via http://www.statmethods.net/stats/regression.html
#
#' Calculate k fold cross validated r2

kfoldcv = function(lmfit, folds = 5, runs = 10, seed = 2017) {
  library(magrittr)
  
  # get data
  data = lmfit$model
  modsum <- summary(lmfit)
  
  # seed
  if (!is.na(seed)) set.seed(seed)
  
  v_runs = sapply(1:runs, FUN = function(run) {
    
    # randomly shuffle the data
    data2 = data[sample(nrow(data)), ]
    
    # create n equally size folds
    folds_idx <- cut(seq(1, nrow(data2)), breaks = folds, labels = FALSE)
    
    # perform n fold cross validation
    sapply(1:folds, function(i) {
      
      # segement data by fold using the which() function
      
      test_idx = which(folds_idx==i, arr.ind=TRUE)
      test_data = data2[test_idx, ]
      train_data = data2[-test_idx, ]
      
      # weights
      if ("(weights)" %in% data) {
        wtds = train_data[["(weights)"]]
      } else {
        train_data$.weights = rep(1, nrow(train_data))
      }
      
      # fit
      fit = lm(formula = lmfit$call$formula, data = train_data, weights = .weights)
      
      # predict
      preds = predict(fit, newdata = test_data)
      
      # correlate to get r2
      cor(preds, test_data[[1]], use = "p")^2
    }) %>%
      mean()
  })
  
  # calculate adjusted cv rsq
  
  n <- modsum$df[1] + modsum$df[2]
  p <- modsum$df[1] # including intercept
  cvrsq <- mean(v_runs)
  cvadjrsq <- 1 - (1-cvrsq) * ((n-1)/(n-p-1))
  
  # return
  c("R2" = summary(lmfit)$r.squared,
    "Adj. R2" = summary(lmfit)$adj.r.squared,
    "CV R2" = mean(v_runs),
    "Adj. CV R2" = cvadjrsq)
}