################
#   QQ plots   #
################

ggQQ = function(lm) {
  d <- data.frame(std.resid = rstandard(lm))
  y <- quantile(d$std.resid[!is.na(d$std.resid)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y) / diff(x)
  int <- y[1L] - slope * x[1L]
  p <- ggplot(data = d, aes(sample = std.resid)) +
    geom_abline(slope = slope, intercept = int, color = "red") +
    stat_qq(shape = 16, size = 2, alpha = 0.5) +
    labs(x = "Theoretical quantiles",
         y = "Standardized residuals") +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          plot.title = element_text(size = 12)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0))
  return(p)
}

qqPlots <- lapply(modelList, ggQQ)

qqplots <- plot_grid(
  qqPlots[[1]] + ggtitle("PCL-R Interpersonal facet"), 
  qqPlots[[2]] + ggtitle("PCL-R Affective facet"), 
  qqPlots[[3]] + ggtitle("PCL-R Lifestyle facet"),
  qqPlots[[4]] + ggtitle("PCL-R Antisocial facet"),
  ncol = 4, nrow = 1, labels = NULL)

rm(ggQQ, qqPlots)