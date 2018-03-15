##################################
#   residuals vs fitted plots    #
##################################

# lapply through modelList plotting fitted vs residuals
resfitPlots <- lapply(modelList, function(i) {
    ggplot(i, aes(.fitted, .resid)) + 
    geom_point(shape = 16, size = 2, alpha = 0.5) +
    stat_smooth(method = "loess") + 
    geom_hline(yintercept = 0, col = "red", linetype = "dashed") + 
    xlab("Fitted values") + 
    ylab("Residuals") +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          plot.title = element_text(size = 12))
})

# combine into grid
rfplots <- plot_grid(
  resfitPlots[[1]] + ggtitle("PCL-R interpersonal facet"), 
  resfitPlots[[2]] + ggtitle("PCL-R affective facet"), 
  resfitPlots[[3]] + ggtitle("PCL-R lifestyle facet"),
  resfitPlots[[4]] + ggtitle("PCL-R antisocial facet"),
  ncol = 4, nrow = 1, labels = NULL)

# keep environment clean
rm(resfitPlots)