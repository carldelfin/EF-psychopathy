# formulas
formulaList <- as.list(NULL)
formulaList <- lapply(outcomeVariables, function(i) {paste(i, paste(predictorList, collapse = "+"), sep = "~")})

# models
modelList <- lapply(formulaList, function(x, data) eval(bquote(lm(.(x), data))), data = data)

# diagnostic plots
source("scripts/resfitplots.R")
source("scripts/qqplots.R")

# save plots for visual examination
ggsave(filename = "figures/rfplots.tiff",
       plot = rfplots,
       height = 3,
       width = 16,
       dpi = 300,
       compression = "lzw")

ggsave(filename = "figures/qqplots.tiff",
       plot = qqplots,
       height = 3,
       width = 16,
       dpi = 300,
       compression = "lzw")

# remove everything
rm(rfplots, qqplots, formulaList, modelList)