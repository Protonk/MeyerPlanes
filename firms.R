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
firms.df[, "Nation"] <- gsub(" \\([^()]*\\)", "", firms.df[, "Nation"])


# separators and split listings

firms.df[, "Nation"] <- sub(";", ",", firms.df[, "Nation"])

firms.breakout <- breakMultiples(data = firms.df, column = "Nation")


## Years

# more sensible name for now
names(firms.df)[3] <- "Year Start"

# Generate column for imputed years as we'll
# drop a number of qualifiers

firms.df[, "Year Imputed"] <- gsub("\\D", "", firms.df[, "Year Start"])

# average year ranges

ranged <- firms.df[grep("\\d{8}", firms.df[, "Year Imputed"]), "Year Imputed"]
avg.ranged <- round((as.numeric(substr(ranged, 0, 4)) + as.numeric(substr(ranged, 5, 8)))/2)
firms.df[grep("\\d{8}", firms.df[, "Year Imputed"]), "Year Imputed"] <- avg.ranged

# 189X to 1895

firms.df[, "Year Imputed"] <- sub("189$", "1895", firms.df[, "Year Imputed"])

# Mark NA

firms.df[nchar(firms.df[, "Year Imputed"]) == 0, "Year Imputed"] <- NA

# multiple locations combined with years

catMultipleCountries <- function() {
  firms.multiple <- cbind(firms.breakout, firms.df[, "Year Imputed"])
  names(firms.multiple) <- c(names(firms.breakout), "Year")
  
  firms.reduced.final <- data.frame(matrix(NA, ncol = 3, nrow = 0))
  names(firms.reduced.final) <- c("Country", "Year", "Firm Starts")
  for (i in (1:(ncol(firms.breakout) - 1))) {
    firms.reduced <- ddply(firms.multiple, c(i,5), "nrow")
    names(firms.reduced) <- c("Country", "Year", "Firm Starts")
    firms.reduced.final <- rbind(firms.reduced.final, firms.reduced)
  }
  firms.reduced.final[, "Year"] <- as.numeric(as.character(firms.reduced.final[, "Year"]))
  return(firms.reduced.final)
}

firms.reduced.df <- catMultipleCountries()

firms.reduced.df[, "Country"] <- as.character(firms.reduced.df[, "Country"])

other.countries <- names(table(firms.reduced.df[, "Country"]))[table(firms.reduced.df[, "Country"]) <= 6]
firms.reduced.df[firms.reduced.df[, "Country"] %in% other.countries, "Country"] <- "Other"
firms.reduced.df[, "Country"] <- factor(firms.reduced.df[, "Country"])

  








