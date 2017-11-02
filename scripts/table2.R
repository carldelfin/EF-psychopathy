tidytable <- lapply(modelHC, tidy) # tidy object for step 1 models

for (i in 1:length(tidytable)) {
  tidytable[[i]]        <- tidytable[[i]][-1, ]           # remove intercept 
  tidytable[[i]]$cilow  <- round(modelCI[[i]][-1, 1], 2)  # add LL CI
  tidytable[[i]]$cihigh <- round(modelCI[[i]][-1, 2], 2)  # add UL CI
  tidytable[[i]][, 2]   <- round(tidytable[[i]][, 2], 2)  # round estimate
  tidytable[[i]][, 5]   <- round(tidytable[[i]][, 5], 3)  # round p-value
  tidytable[[i]]        <- transform(tidytable[[i]], ci = paste(cilow, cihigh, sep=", "))
  tidytable[[i]]        <- tidytable[[i]][, -c(1, 3, 4, 6, 7)] # remove columns
  tidytable[[i]]        <- tidytable[[i]][, c(1, 3, 2)]  # reorder columns
}

table2 <- as.data.frame(cbind(tidytable[[1]], NA,
                              tidytable[[2]], NA,
                              tidytable[[3]], NA,
                              tidytable[[4]], NA,
                              tidytable[[5]]))

table2$rownames <- c("SOCMITT5", "SOCPS", "IEDstages", 
                     "IEDtotaler", "SWMstrategy", "SWMtotaler",
                     "SSTSSRT", "SSTSSD50")

# reorder columns so that 'rownames' is first
col_idx <- grep("rownames", names(table2))
table2 <- table2[, c(col_idx, (1:ncol(table2))[-col_idx])]
remove(col_idx)

# create FlexTable
table2 = FlexTable(table2,
                   header.columns = FALSE,
                   header.text.props = textProperties(font.size = 8),
                   body.text.props = textProperties(font.size = 8))

# add headers, level 1
table2 = addHeaderRow(table2,
                      value = c("",
                                "Total score", "",
                                "Facet 1", "", 
                                "Facet 2", "", 
                                "Facet 3", "",
                                "Facet 4"),
                      colspan = c(1, 3, 1, 3, 1, 3, 1, 3, 1, 3))

# add headers, level 2
table2 = addHeaderRow(table2,
                      value = c("",
                                "B", "95% CI", "p", "",
                                "B", "95% CI", "p", "",
                                "B", "95% CI", "p", "",
                                "B", "95% CI", "p", "",
                                "B", "95% CI", "p"))