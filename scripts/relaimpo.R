############################################
#   relative importace estimation plots    #
############################################

step1riplots <- lapply(step1ri, function(i) {
  names <- names(sort(i$lmg, decreasing = FALSE))
  values <- (as.numeric(sort(i$lmg, decreasing=FALSE))*100)
  tempdata <- data.frame(vars=factor(c(names),
                                     levels=c(names)),
                         percent=c(values))
  ggplot(data = tempdata, aes(x=vars, y=percent)) + 
    geom_bar(alpha = 0.6, position=position_dodge(), stat="identity") +
    scale_x_discrete(name="", expand = c(0,0.5)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
    xlab("") +
    ylab(expression("Relative importance (%)")) +
    geom_text(data = tempdata,
              aes(x = vars, group=vars, y = percent + 0.15, 
                  label = format(percent, nsmall = 1, digits=0, scientific = FALSE)), 
              color="black", position=position_dodge(0.8), hjust=.5, vjust=-0.2, size=3) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=11),
          axis.title.y = element_text(size=11),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 12))
})

step2riplots <- lapply(step2ri, function(i) {
  names <- names(sort(i$lmg, decreasing = FALSE))
  values <- (as.numeric(sort(i$lmg, decreasing=FALSE))*100)
  tempdata <- data.frame(vars=factor(c(names),
                                     levels=c(names)),
                         percent=c(values))
  ggplot(data = tempdata, aes(x=vars, y=percent)) + 
    geom_bar(alpha = 0.6, position=position_dodge(), stat="identity") +
    scale_x_discrete(name="", expand = c(0,0.5)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
    xlab("") +
    ylab(expression("Relative importance (%)")) +
    geom_text(data = tempdata,
              aes(x = vars, group=vars, y = percent + 0.15, 
                  label = format(percent, nsmall = 1, digits=0, scientific = FALSE)), 
              color="black", position=position_dodge(0.8), hjust=.5, vjust=-0.2, size=3) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=11),
          axis.title.y = element_text(size=11),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 12))
})