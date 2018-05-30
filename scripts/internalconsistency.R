pcltotal <- data[, pclItems]

totalAlpha <- psych::alpha(pcltotal)
facet1Alpha <- psych::alpha(pcltotal[, c("pcl1", "pcl2", "pcl4", "pcl5")])
facet2Alpha <- psych::alpha(pcltotal[, c("pcl6", "pcl7", "pcl8", "pcl16")])
facet3Alpha <- psych::alpha(pcltotal[, c("pcl3", "pcl9", "pcl13", "pcl14", "pcl15")])
facet4Alpha <- psych::alpha(pcltotal[, c("pcl10", "pcl12", "pcl18", "pcl19", "pcl20")])

# variable names
varnames <- c("PCL-R total score",
              "PCL-R facet 1",
              "PCL-R facet 2",
              "PCL-R facet 3",
              "PCL-R facet 4")

# item N minimum
itemmin <- c(min(totalAlpha$item.stats$n),
             min(facet1Alpha$item.stats$n),
             min(facet2Alpha$item.stats$n),
             min(facet3Alpha$item.stats$n),
             min(facet4Alpha$item.stats$n))

# item N maximum
itemmax <- c(max(totalAlpha$item.stats$n),
             max(facet1Alpha$item.stats$n),
             max(facet2Alpha$item.stats$n),
             max(facet3Alpha$item.stats$n),
             max(facet4Alpha$item.stats$n))

# Cronbach's alpha
calpha <- c(totalAlpha$total$raw_alpha,
            facet1Alpha$total$raw_alpha,
            facet2Alpha$total$raw_alpha,
            facet3Alpha$total$raw_alpha,
            facet4Alpha$total$raw_alpha)

# mean corrected item-total correlation
mcit <- c(mean(totalAlpha$item.stats$r.cor),
          mean(facet1Alpha$item.stats$r.cor),
          mean(facet2Alpha$item.stats$r.cor),
          mean(facet3Alpha$item.stats$r.cor),
          mean(facet4Alpha$item.stats$r.cor))

# mean inter-item correlation
miic <- c(totalAlpha$total$average_r,
          facet1Alpha$total$average_r,
          facet2Alpha$total$average_r,
          facet3Alpha$total$average_r,
          facet4Alpha$total$average_r)

consistencydata <- data.frame(varnames,
                              itemmin,
                              itemmax,
                              round(calpha, 2),
                              round(mcit, 2),
                              round(miic, 2))

colnames(consistencydata) <- c("PCL-R variable",
                               "min N per item",
                               "max N per item",
                               "Cronbach's alpha",
                               "Mean corrected item-total correlation",
                               "Mean inter-item correlation")

# create Word document
doc <- docx()
doc <- addTitle(doc, "Internal consistency")
doc <- addFlexTable(doc, FlexTable(consistencydata))

# save
writeDoc(doc, file = "hidden/results/internalconsistency.docx")

rm(pcltotal, totalAlpha, facet1Alpha, facet2Alpha, 
   facet3Alpha, facet4Alpha, consistencydata, doc,
   calpha, itemmax, itemmin, mcit, miic, varnames, pclItems)