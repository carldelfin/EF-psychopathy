#################################
#   relative importance  plots  #
#################################

riplots <- lapply(modelRI, function(i) {
  
  names <- names(sort(i$lmg, decreasing = FALSE))
  values <- as.numeric(sort(i$lmg, decreasing = FALSE)) * 100
  tempdata <- data.frame(vars = factor(c(names), levels = c(names)), percent = c(values))
  
  ggplot(data = tempdata, aes(x = vars, y = percent)) + 
    geom_bar(alpha = 0.6, position = position_dodge(), stat = "identity") +
    scale_x_discrete(name = "", expand = c(0, 0.5)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 5)) +
    xlab("") +
    ylab(expression({italic(R)}^2)) +
    geom_text(data = tempdata,
              aes(x = vars, group = vars, y = percent + 0.05, 
                  label = format(percent, nsmall = 1, digits = 0, scientific = FALSE)), 
              color = "black", position = position_dodge(0.8), hjust = .5, vjust = -0.2, size = 3) +
    theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 12),
          axis.title.y = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 12))
})

plot1 <- riplots[[1]] + ggtitle("PCL-R Interpersonal facet") + annotate("text", x = 1.9, y = 4.7, label = paste("Model~italic(R)^2 == ", round(sum(riplots[[1]]$data[2]), 2)), size = 4, parse = TRUE)
plot2 <- riplots[[2]] + ggtitle("PCL-R Affective facet") + annotate("text", x = 1.9, y = 4.7, label = paste("Model~italic(R)^2 == ", round(sum(riplots[[2]]$data[2]), 2)), size = 4, parse = TRUE)
plot3 <- riplots[[3]] + ggtitle("PCL-R Lifestyle facet") + annotate("text", x = 1.9, y = 4.7, label = paste("Model~italic(R)^2 == ", round(sum(riplots[[3]]$data[2]), 2)), size = 4, parse = TRUE)
plot4 <- riplots[[4]] + ggtitle("PCL-R Antisocial facet") + annotate("text", x = 1.9, y = 4.7, label = paste("Model~italic(R)^2 == ", round(sum(riplots[[4]]$data[2]), 2)), size = 4, parse = TRUE)


riplot <- plot_grid(
  plot1,
  plot2,
  plot3,
  plot4,
  ncol = 4, nrow = 1, labels = NULL)

rm(modelRI, riplots, plot1, plot2, plot3, plot4)