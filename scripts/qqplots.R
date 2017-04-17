################
#   QQ plots   #
################

ggQQ1 = function(lm) {
  d <- data.frame(std.resid = rstandard(lm))
  y <- quantile(d$std.resid[!is.na(d$std.resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  p <- ggplot(data=d, aes(sample=std.resid)) +
    geom_abline(slope = slope, intercept = int, color = "red") +
    stat_qq(shape=1, size=2) +
    labs(x="Theoretical Quantiles",
         y="Standardized Residuals") +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=10),
          plot.title=element_text(size=12)) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0))
  return(p)
}

ggQQ2 = function(lm) {
  d <- data.frame(std.resid = rstandard(lm))
  y <- quantile(d$std.resid[!is.na(d$std.resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  p <- ggplot(data=d, aes(sample=std.resid)) +
    geom_abline(slope = slope, intercept = int, color = "red") +
    stat_qq(shape=1, size=2) +
    labs(x="Theoretical Quantiles",
         y="Standardized Residuals") +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=10),
          plot.title=element_text(size=12)) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0))
  return(p)
}

step1qqplots <- lapply(step1models, ggQQ1)
step2qqplots <- lapply(step2models, ggQQ2)
