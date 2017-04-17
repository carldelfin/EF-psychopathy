#################################
#   residual vs fitted plots    #
#################################

step1resfitplots <- lapply(step1models, function(i) {
    ggplot(i, aes(.fitted, .resid)) + 
    geom_point() +
    stat_smooth(method="loess") + 
    geom_hline(yintercept=0, col="red", linetype="dashed") + 
    xlab("Fitted values") + 
    ylab("Residuals") +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=10),
          plot.title=element_text(size=12))
})

step2resfitplots <- lapply(step2models, function(i) {
  ggplot(i, aes(.fitted, .resid)) + 
    geom_point() +
    stat_smooth(method="loess") + 
    geom_hline(yintercept=0, col="red", linetype="dashed") + 
    xlab("Fitted values") + 
    ylab("Residuals") +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    theme(axis.text=element_text(size=10),
          axis.title=element_text(size=10),
          plot.title=element_text(size=12))
})