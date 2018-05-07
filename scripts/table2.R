# this is done right, and works
tidytable <- lapply(modelHC, tidy)
tidytable <- lapply(tidytable, function(i) {i[-1, ]})

# this is bad practice, but also works
for (i in 1:length(tidytable)) {
  tidytable[[i]]$cilow  <- round(modelCI[[i]][-1, 1], 4)  # add LL CI
  tidytable[[i]]$cihigh <- round(modelCI[[i]][-1, 2], 4)  # add UL CI
  tidytable[[i]][, 2]   <- round(tidytable[[i]][, 2], 2)  # round estimate
  tidytable[[i]][, 5]   <- round(tidytable[[i]][, 5], 3)  # round p-value
  tidytable[[i]]        <- transform(tidytable[[i]], ci = paste(cilow, cihigh, sep=", "))
  tidytable[[i]]$ci     <- as.character(tidytable[[i]]$ci)
  tidytable[[i]]        <- tidytable[[i]][, -c(3, 4, 6, 7)] # remove columns
  tidytable[[i]]        <- tidytable[[i]][, c(1, 2, 4, 3)]  # reorder columns
}

# cbind lists into data frame, add space between models
table2 <- as.data.frame(cbind(tidytable[[1]], "",
                              tidytable[[2]], "",
                              tidytable[[3]], "",
                              tidytable[[4]]))

# add row indicating model
mod <- c("Interpersonal facet", "", "", "", "", 
         "Affective facet", "", "", "", "", 
         "Lifestyle facet", "", "", "", "", 
         "Antisocial facet", "", "", "")

# rbind into one data frame
table2 <- rbind(mod, table2)

# keep environment clean
rm(i, tidytable, mod, modelHC, modelCI)