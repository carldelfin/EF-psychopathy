# create data subset
cordata <- data[c(21, 23:34)]

# create a correlation matrix using Pearson's r
cor.matrix <- round(cor(cordata, use = "pairwise.complete.obs", method = "pearson"), digits = 2)
p.matrix <- round(cor_pmat(cordata), 3)

cor.matrix <- matrix(paste0("r = ", cor.matrix, ", p = ", p.matrix), 13, 13)

dimnames(cor.matrix)[[1]] <- names(cordata)
dimnames(cor.matrix)[[2]] <- names(cordata)

cor.matrix[upper.tri(cor.matrix, diag = TRUE)] <- NA

gd_results <- gs_key(KEY)
gd_results <- gd_results %>%
  gs_edit_cells(ws = "correlations", input = cor.matrix)

rm(cor.matrix, p.matrix, cordata)