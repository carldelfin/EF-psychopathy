plot1 <- ggplot(data = data, aes(x = facet1, y = SOCMITT5)) + 
  geom_point(shape = 21, colour = "black", fill = "gray", size = 1, stroke = 1) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  annotate("text",
           x = 8,
           y = 28,
           label = paste0("r = ",
                          round(cor(data$facet1, data$SOCMITT5), 2),
                          "\np = ",
                          round(cor.test(data$facet1, data$SOCMITT5)$p.value, 3)),
           size = 3.5, hjust = 1) +
  theme(
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 10)) +
  labs(x = "PCL-R Facet 1", y = "SOC MITT 5")

plot2 <- ggplot(data = data, aes(x = facet2, y = SOCMITT5)) + 
  geom_point(shape = 21, colour = "black", fill = "gray", size = 2, stroke = 1) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  annotate("text",
           x = 8,
           y = 28,
           label = paste0("r = ",
                          round(cor(data$facet2, data$SOCMITT5), 2),
                          "\np = ",
                          round(cor.test(data$facet2, data$SOCMITT5)$p.value, 3)),
           size = 3.5, hjust = 1) +
  theme(
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 10)) +
  labs(x = "PCL-R Facet 2", y = "SOC MITT 5")

plot3 <- ggplot(data = data, aes(x = facet3, y = SOCMITT5)) + 
  geom_point(shape = 21, colour = "black", fill = "gray", size = 2, stroke = 1) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  annotate("text",
           x = 10,
           y = 28,
           label = paste0("r = ",
                          round(cor(data$facet3, data$SOCMITT5), 2),
                          "\np = ",
                          round(cor.test(data$facet3, data$SOCMITT5)$p.value, 3)),
           size = 3.5, hjust = 1) +
  theme(
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 10)) +
  labs(x = "PCL-R Facet 3", y = "SOC MITT 5")

plot4 <- ggplot(data = data, aes(x = facet4, y = SOCMITT5)) + 
  geom_point(shape = 21, colour = "black", fill = "gray", size = 2, stroke = 1) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  annotate("text",
           x = 10,
           y = 28,
           label = paste0("r = ",
                          round(cor(data$facet4, data$SOCMITT5), 2),
                          "\np = ",
                          round(cor.test(data$facet4, data$SOCMITT5)$p.value, 3)),
           size = 3.5, hjust = 1) +
  theme(
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 10)) +
  labs(x = "PCL-R Facet 4", y = "SOC MITT 5")

scatterPlot <- plot_grid(
  plot1, 
  plot2, 
  plot3,
  plot4,
  ncol = 4, nrow = 1, labels = NULL)