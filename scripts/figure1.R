# legend titles with italics
ctitle <- expression(paste("Pearson's ", italic("r")))
ptitle <- expression(paste(italic("p"), "-value"))

# sensible axis titles
labels = rev(c("PCL-R interpersonal facet",
               "PCL-R affective facet",
               "PCL-R lifestyle facet",
               "PCL-R antisocial facet",
               "IED stages completed",
               "IED errors",
               "SWM errors",
               "SWM strategy score",
               "SST stop-signal RT",
               "SST mean correct RT",
               "SOC MITT",
               "SOC problems solved"))

# --------------------------------------------------------------------------
#                         correlation plot
# --------------------------------------------------------------------------

plot_cor <- ggplot(cor, aes(x = X, y = Y, fill = r))+
  geom_tile() +
  scale_fill_distiller(type = "div",
                       palette = "RdYlGn", direction = 1,
                       values = NULL, space = "Lab", na.value = "grey50",
                       guide = "colorbar",
                       name = ctitle,
                       limits = c(-1, 1),
                       breaks = c(-1, 0, 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 9, hjust = 1))+
  geom_text(aes(x = X, y = Y, label = gsub("0\\.", "\\.", r)), color = "black", size = 3.2) +
  coord_flip() +
  scale_x_discrete(expand = c(0, 0),
                   name = "", 
                   labels = labels) +
  scale_y_discrete(expand = c(0, 0),
                   name = "", 
                   labels = rev(labels)) +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", 
                               title.hjust = 0.5)) +
  geom_rect(xmin = 0 , xmax = 8.5 , ymin = 0 , ymax = 4.5,
            color = "black",
            linetype = 2,
            fill = NA) +
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 1, 
                                   size = 9,
                                   hjust = 1),
        axis.text.y = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.9, 0.7),
        legend.direction = "horizontal",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10))

# --------------------------------------------------------------------------
#                         p-value plot
# --------------------------------------------------------------------------

plot_p <- ggplot(cor, aes(x = X, y = Y, fill = p))+
  geom_tile() +
  scale_fill_distiller(type = "div",
                       palette = "RdYlGn", direction = -1,
                       values = NULL, space = "Lab", na.value = "grey50",
                       guide = "colorbar",
                       name = ptitle,
                       limits = c(0, 1),
                       breaks = c(0, 0.5, 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 9, hjust = 1)) +
  geom_text(aes(x = X, y = Y, label = gsub("0\\.", "\\.", sigp2)), color = "black", size = 3.2) +
  coord_flip() +
  scale_x_discrete(expand = c(0, 0),
                   name = "", 
                   labels = labels) +
  scale_y_discrete(expand = c(0, 0),
                   name = "", 
                   labels = rev(labels)) +
  labs(x = "", y = "") + 
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", 
                               title.hjust = 0.5)) +
  geom_rect(xmin = 0 , xmax = 8.5 , ymin = 0 , ymax = 4.5,
            color = "black",
            linetype = 2,
            fill = NA) +
  geom_tile(data = cor,
            aes(colour = factor(sigp, c("yes", "no")),
                size = factor(sigp, c("yes", "no"))), 
            alpha = 0,
            width = 0.75,
            height = 0.85,
            linetype = "dotted",
            show.legend = FALSE) +
  # below is for use with FDR correction
  #geom_tile(data = cor,
  #          aes(colour = factor(sigpfdr, c("yes", "no")),
  #              size = factor(sigpfdr, c("yes", "no"))), 
  #          alpha = 0,
  #          width = 0.75,
  #          height = 0.85,
  #          linetype = "solid",
  #          show.legend = FALSE) + 
  scale_colour_manual("sigp", values = c("#161616", "white")) + 
  scale_size_manual("sigp", values = c(0.3, 0)) +
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 1, 
                                   size = 9,
                                   hjust = 1),
        axis.text.y = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.9, 0.7),
        legend.direction = "horizontal",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10))

# --------------------------------------------------------------------------
#                         posterior probability plot
# --------------------------------------------------------------------------

plot_prob <- ggplot(result_full, aes(x = X, y = Y, fill = prob))+
  geom_tile() +
  scale_fill_distiller(type = "div",
                       palette = "RdYlGn", direction = 1,
                       values = NULL, space = "Lab", na.value = "grey50",
                       guide = "colorbar",
                       name = "Posterior probability",
                       limits = c(0, 1),
                       breaks = c(0, 0.5, 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 9, hjust = 1)) +
  geom_text(aes(x = X, y = Y, label = prob3), color = "black", size = 3.2) +
  coord_flip() +
  scale_x_discrete(expand = c(0, 0),
                   name = "", 
                   labels = labels) +
  scale_y_discrete(expand = c(0, 0),
                   name = "", 
                   labels = rev(labels)) +
  labs(x = "", y = "") + 
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", 
                               title.hjust = 0.5)) +
  geom_rect(xmin = 0 , xmax = 8.5 , ymin = 0 , ymax = 4.5,
            color = "black",
            linetype = 2,
            fill = NA) +
  geom_tile(data = result_full,
            aes(colour = factor(prob2, c("yes", "no")),
                size = factor(prob2, c("yes", "no"))), 
            alpha = 0,
            width = 0.75,
            height = 0.85,
            linetype = "dotted",
            show.legend = FALSE) +
  scale_colour_manual("prob2", values = c("#161616", "white")) + 
  scale_size_manual("prob2", values = c(0.3, 0)) +
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 1, 
                                   size = 9,
                                   hjust = 1),
        axis.text.y = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.9, 0.7),
        legend.direction = "horizontal",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10))

# --------------------------------------------------------------------------
#                         bayes factor plot
# --------------------------------------------------------------------------

plot_bayes <- ggplot(result_full, aes(x = X, y = Y, fill = bayes)) +
  geom_tile() +
  scale_fill_distiller(type = "div",
                       palette = "RdYlGn", direction = 1,
                       values = NULL, space = "Lab", na.value = "#1A9850",
                       guide = "colorbar",
                       name = "Bayes factor",
                       limits = c(0, 3.01),
                       breaks = c(0, 1/3, 1, 3.01),
                       labels = c("0", "1/3", "1", "3+")) +
  geom_text(aes(x = X, y = Y, label = formatC(bayes, format = "E", digits = 1)), size =  2.5) +
  coord_flip() +
  scale_x_discrete(expand = c(0, 0),
                   name = "", 
                   labels = labels) +
  scale_y_discrete(expand = c(0, 0),
                   name = "", 
                   labels = rev(labels)) +
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", 
                               title.hjust = 0.5)) +
  geom_rect(xmin = 0 , xmax = 8.5 , ymin = 0 , ymax = 4.5,
            color = "black",
            linetype = 2,
            fill = NA) +
  theme(axis.text.x = element_text(angle = 45,
                                   vjust = 1, 
                                   size = 9,
                                   hjust = 1),
        axis.text.y = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.9, 0.7),
        legend.direction = "horizontal",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10))

bayesplot <- plot_grid(plot_cor, plot_p, 
                       plot_prob, plot_bayes,
                       ncol = 2, nrow = 2, labels = "AUTO")

rm(cor, result_full, plot_cor, plot_p, plot_prob, plot_bayes, ctitle, ptitle, labels)