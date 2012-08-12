
# String processing library. Doesn't add too much to R but makes a number of functions
# more sensible
library(stringr)
# Data munching library. Split lists, arrays, dataframes and operate piecewise.
library(plyr)

### Utility functions

# casefolding from chartr()'s help page 
# No NA checking done beforehand
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s,1,1)),
{s <- substring(s,2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# Split into multiples 
# I'll be doing this for firms, clubs, articles and patents, so this will be turned into a
# reusable function
# Breaks columns w/ multiple classifications into single classifications

breakMultiples <- function(data, column, split.regex = ", | and ", binary = TRUE) {
  # strsplit creates a list from the splitting regex
  mult.list <- strsplit(data[, column], split.regex)
  # grab length for each list element
  mult.cols <- laply(mult.list, length)
  # fill space out first (makes it a little faster and easier to understand)
  prefill.mat <- matrix(NA,
                        nrow = nrow(data),
                        ncol = max(mult.cols))
  colnames(prefill.mat) <- 
  # select only those elements which are in the ith category
  for (i in 1:max(mult.cols)) {
    prefill.mat[, i] <- laply(mult.list, `[`, i)
  }
  df.out <- data.frame(lapply(data.frame(prefill.mat), factor))

  # Note for where there are multiple columns
  if (binary) {
    df.out <- data.frame(cbind(df.out, mult.cols > 1))
    }
  else {
    df.out <- data.frame(cbind(df.out, mult.cols))
  }
  names(df.out) <- c(paste(column, "Category", 1:max(mult.cols), sep = " "), "Multiple Cats")
  return(df.out)
}

### Patents

# Matches country codes to full names
langs.str <- c('Britain','Germany','France','United States')
patents.df[, "Country"] <- langs.str[match(patents.df[, "Country"], c("br", "de", "fr", "us"))]

## Authors

# Drop location information for authors (~ 400 rows)
patents.df[, "Authors"] <- gsub("\\\n.*$|\\(.*\\)", "", patents.df[, "Authors"])

## Field 


# You indication the presence of a question mark denoted a potentially unsure 
# classification. We note this and remove the question mark
patents.df[, "Classification Unsure"] <- grepl("\\?+", patents.df[, "Field"])
# also drop trailing spaces. Leave unknowns
patents.df[, "Field"] <- sub("\\?{2}", "Unknown", patents.df[, "Field"])
patents.df[, "Field"] <- sub("\\?|\\s+$", "", patents.df[, "Field"])

# Multiple classifications are split most commonly by semi-colons and
# sometimes by commas
patents.df[, "Field"] <- gsub(",|\\s+;\\s+", "; ", patents.df[, "Field"])


# Capitalize words
patents.df[!is.na(patents.df[, "Field"]), "Field"] <- capwords(patents.df[!is.na(patents.df[, "Field"]), "Field"])

## Years

patents.df[grep("\\?", patents.df[, "Year"]), ] <- NA

### Generate title information from Summary

## Simple table of terms. Potentially useful for visualization
# lowercase and drop hyphens
term.table <- table(tolower(gsub("-", " ", patents.df[, "English.Title.Summary"])))
term.table <- term.table[term.table >= 3]
term.table <- term.table[order(term.table, decreasing = TRUE)]

## Categories for titles (need input here)

# Right now this is an arbitrary/ad-hoc classification for different titles. 
# 
# airships.pat <- "balloon|dirigible|(air|aerial)[- ]?ship|luftschiff|aerostat|floatation"
# airplane.pat <- "airplane|aeroplane|flugzeug"
# flying.pat <- "^(flying|aerial)[- ]machine||apparatus|flugapparat|flugmaschine"
# component.pat <- "device|propell(er|ing)|^apparatus|steering|propulsion|^machine"


### Clubs

## Scope

# note unknown/potentially unknown classification.
# Then remove "?s"
clubs.df[, "Unknown Scope"] <- grepl("\\?", clubs.df[, "Scope"])
clubs.df[, "Scope"] <- sub("\\?", "", clubs.df[, "Scope"])


#trim whitespace
clubs.df[, "Scope"] <- gsub("^\\s+|\\s+$", "", clubs.df[, "Scope"])
clubs.df[nchar(clubs.df[, "Scope"]) == 0, "Scope"] <- NA 

# cleanup entries
clubs.df[, "Scope"] <- sub("Sttate", "State", clubs.df[, "Scope"])
clubs.df[, "Scope"] <- sub("Internati?onal,? Scientific", "International Scientific", clubs.df[, "Scope"])
clubs.df[, "Scope"] <- sub(" Club|, Multi-State", "", clubs.df[, "Scope"])
clubs.df[, "Scope"] <- sub("Univesrsity", "University", clubs.df[, "Scope"])



## Years

# TODO: Note "?"
#       establish cutoff years of interest
#       convert to numeric

# Rough matching for years. Warns about warnings introduced. 
clubs.df[, "Matched Start Year"] <- as.numeric(str_match(clubs.df[, "Start Year"], "\\d{4}"))

## Country

# Cleanup names
clubs.df[, "Country"] <- gsub("^\\s+|\\s+$", "", clubs.df[, "Country"])
clubs.df[, "Country"] <- sub("UISA|^US$", "USA", clubs.df[, "Country"])
clubs.df[, "Country"] <- sub("[Gg]er(man$|many-|mamy|manu)", "Germany", clubs.df[, "Country"])
clubs.df[, "Country"] <- sub("England,\\s?GB,\\s?UK.?$", "England, GB, UK", clubs.df[, "Country"])

# Build simplified Country factor
clubs.df[, "Country Factor"] <- "Other"
clubs.df[grepl("UK", clubs.df[, "Country"]), "Country Factor"] <- "United Kingdom"
clubs.df[grepl("Germany", clubs.df[, "Country"]), "Country Factor"] <- "Germany"
clubs.df[grepl("France", clubs.df[, "Country"]), "Country Factor"] <- "France"
clubs.df[grepl("US", clubs.df[, "Country"]), "Country Factor"] <- "United States"

clubs.df[, "Country Factor"] <- factor(clubs.df[, "Country Factor"],
                                       levels = c("Germany",
                                                  "France",
                                                  "United Kingdom",
                                                  "United States",
                                                  "Other"))


### Firms


## Country

# mark unsure and remove
firms.df[, "Nation Unsure"] <- grepl("\\?", firms.df[, "Country"])
firms.df[, "Country"] <- sub("\\?", "", firms.df[, "Country"])

# whitespace

firms.df[grepl("^ $", firms.df[, "Country"]), "Country"] <- NA
firms.df[, "Country"] <- gsub("^\\s|\\s$", "", firms.df[, "Country"])

# clean up country listings
firms.df[, "Country"] <- gsub("^US$|^.USA$", "USA", firms.df[, "Country"])
firms.df[, "Country"] <- gsub(".*[Hh]ungary|^AH$", "Austria-Hungary", firms.df[, "Country"])
firms.df[, "Country"] <- gsub("Germanu", "Germany", firms.df[, "Country"])
firms.df[, "Country"] <- gsub(" \\([^()]*\\)", "", firms.df[, "Country"])


# separators and split listings

firms.df[, "Country"] <- sub(";", ",", firms.df[, "Country"])

firms.breakout <- breakMultiples(data = firms.df, column = "Country")


## Years


# Generate column for imputed years as we'll
# drop a number of qualifiers

firms.df[, "Imputed Year"] <- gsub("\\D", "", firms.df[, "Start Year"])

# average year ranges

ranged <- firms.df[grep("\\d{8}", firms.df[, "Imputed Year"]), "Imputed Year"]

firms.df[grep("\\d{8}", firms.df[, "Imputed Year"]), "Imputed Year"] <- round((as.numeric(substr(ranged, 0, 4)) + as.numeric(substr(ranged, 5, 8)))/2)

# 189X to 1895

firms.df[, "Imputed Year"] <- sub("189$", "1895", firms.df[, "Imputed Year"])

# Mark NA

firms.df[nchar(firms.df[, "Imputed Year"]) == 0, "Imputed Year"] <- NA

# multiple locations combined with years

catMultipleCountries <- function() {
  firms.multiple <- cbind(firms.breakout, firms.df[, "Imputed Year"])
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

