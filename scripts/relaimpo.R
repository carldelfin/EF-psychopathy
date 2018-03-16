# ----------------------------------------------------
#               relative importance  plots
# ----------------------------------------------------

riplots <- lapply(modelRI, function(i) {
  
  # prepare data to be plotted
  names <- names(sort(i$lmg, decreasing = FALSE))
  values <- round(as.numeric(sort(i$lmg, decreasing = FALSE)) * 100, 2)
  totalvar <- round(i$R2 * 100, 2)
  tempdata <- data.frame(vars = factor(c(names), levels = c(names)), percent = values, totalvar = totalvar)
   
  
  # plot
  ggplot(data = tempdata, aes(x = vars, y = percent)) + 
    geom_bar(alpha = 0.9, position = position_dodge(), stat = "identity") +
    scale_x_discrete(name = "",
                     expand = c(0, 0.5),
                     labels = c("SOCPS" = "SOC problems solved",
                                "SSTSSRT" = "SST stop-signal RT",
                                "SSTMRT" = "SST mean RT",
                                "IEDstages" = "IED stages completed",
                                "SWMtotaler" = "SWM errors",
                                "IEDtotaler" = "IED errors",
                                "SWMstrategy" = "SWM strategy score",
                                "SOCMITT5" = "SOC MITT")) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 5)) +
    xlab("") +
    ylab(expression({italic(R)}^2)) +
    
    # this is predictor variance
    geom_text(data = tempdata,
              aes(x = vars,
                  group = vars,
                  y = percent + 0.05,
                  label = format(percent, nsmall = 1, digits = 2, scientific = FALSE)),
              color = "black",
              position = position_dodge(0.8),
              hjust = .5,
              vjust = -0.2,
              size = 2) +
    theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 8),
          axis.title.y = element_text(size = 8),
          axis.text.y = element_text(size = 8),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    
    # this is model variance
    annotate("text",
             x = 2.8,
             y = 4.7,
             label = paste("Model~italic(R)^2 == ", totalvar),
             size = 2.4,
             parse = TRUE)
})

# manually add titles
plot1 <- riplots[[1]] + ggtitle("PCL-R interpersonal facet") + theme(plot.title = element_text(size = 8, face = "plain"))
plot2 <- riplots[[2]] + ggtitle("PCL-R affective facet") + theme(plot.title = element_text(size = 8, face = "plain"))
plot3 <- riplots[[3]] + ggtitle("PCL-R lifestyle facet") + theme(plot.title = element_text(size = 8, face = "plain"))
plot4 <- riplots[[4]] + ggtitle("PCL-R antisocial facet") + theme(plot.title = element_text(size = 8, face = "plain"))

# arrange in grid
riplot <- plot_grid(plot1, plot2, plot3, plot4,
                    ncol = 4,
                    nrow = 1,
                    labels = c("(A)", "(B)", "(C)", "(D)"),
                    label_size = 8)

rm(modelRI, riplots, plot1, plot2, plot3, plot4)