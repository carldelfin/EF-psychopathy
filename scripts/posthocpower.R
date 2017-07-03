###########################################
#       post hoc power analysis           #
#   use lm objects and cvrsq as input     #
###########################################

posthocpower <- function(model, cvrsq) {
  modsum <- summary(model)                  # extract summary of model
  n <- modsum$df[1] + modsum$df[2]          # n = total number of observations
  p <- modsum$df[1]                         # p = number of predictors + intercept
  u <- p-1                                  # u = numerator d.f.
  v <- n-p                                  # v = denominator d.f.
  f2 <- cvrsq/(1-cvrsq)                     # f2 = effect size
  
  power <- pwr.f2.test(u = u,
                       v = v,
                       f2 = f2,
                       sig.level = 0.05,    # use alpha = 0.05
                       power = )            # = solve for power
  
  power <- round(power$power, 3)            # select power, round to 3 digits
  return(power)                             # return power
}
