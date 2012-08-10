# Drop last 3 columns (one contains sparse notes)
firms.df <- read.csv(file.path(getwd(), "Data", "Clubs-firms", "Firms_v46.2.csv"), as.is = TRUE, na.strings = c("NA", ""))[, 1:14]


## Nation

# mark unsure and remove
nation.unsure <- grepl("\\?", firms.df[, "Nation"])
firms.df[, "Nation"] <- sub("\\?", "", firms.df[, "Nation"])

# whitespace

firms.df[grepl("^ $", firms.df[, "Nation"]), "Nation"] <- NA
firms.df[, "Nation"] <- gsub("^\\s|\\s$", "", firms.df[, "Nation"])

# clean up country listings
firms.df[, "Nation"] <- gsub("^US$|^.USA$", "USA", firms.df[, "Nation"])
firms.df[, "Nation"] <- gsub(".*[Hh]ungary|^AH$", "Austria-Hungary", firms.df[, "Nation"])
firms.df[, "Nation"] <- gsub("Germanu", "Germany", firms.df[, "Nation"])
# separators and split listings

firms.df[, "Nation"] <- sub(";", "," firms.df[, "Nation"])


