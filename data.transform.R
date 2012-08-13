
# String processing library. Doesn't add too much to R but makes a number of functions
# more sensible
library(stringr)
# Data munching library. Split lists, arrays, dataframes and operate piecewise.
library(plyr)

### Utility functions

## casefolding from chartr()'s help page 
# No NA checking done beforehand
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s,1,1)),
{s <- substring(s,2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

## Split into multiples 
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


## Combine multiple locations with some other column

# The idea here is to expand on our ddply function for one column
# We have multiple columns for classification/scope/etc. and we want
# to sum each up by year or some other column

# Right now we only have the "nrow" function in ddply. future versions will
# allow for all functions ddply allows


ddplyMultiple <- function(data, inputcol, comparison) {
	# generate breakouts and match with comparison column
	multiple.breakout <- breakMultiples(data, inputcol)
	multiple.comb <- cbind(multiple.breakout, data[, comparison])
	
	# Create a 0 row data frame so we can populate it in a for loop
	df.reduced <- data.frame(matrix(NA, ncol = ncol(multiple.breakout) - 1, nrow = 0))
	# name it based on input
	working.names <- names(df.reduced) <- c(inputcol,
																					comparison,
																					"Count")
	# For each classifier count the comparisons and add them
	# to df.reduced
	for (i in (1:(ncol(multiple.breakout) - 1))) {
    intermediate.reduced <- ddply(multiple.comb, c(i,ncol(multiple.comb)), "nrow")
    names(intermediate.reduced) <- working.names
    df.reduced <- rbind(df.reduced, intermediate.reduced)
  }
  # convert from factor											 
	df.reduced[, comparison] <- as.character(df.reduced[, comparison])
	return(df.reduced)
}


## Mark "?", drop whitespace and update NA rows
# Accepts a dataframe and the column of interest. Returns a complete data frame
# due to the need to add the "unknown" classifier

markUnsure <- function(data, column) {
	# Classify rows marked with "?"
	data[, paste("Unknown", column, sep = " ")] <- grepl("\\?", data[, column])
	# Pull column out to make looking at this simpler
	int.vector <- data[, column]
	int.vector <- gsub("\\?", "", int.vector)
	int.vector <- gsub("^\\s+|\\s+$", "", int.vector)
	# mark empty elements as NA
	int.vector[nchar(int.vector) == 0] <- NA
	data[, column] <- int.vector
	return(data)
}
	



###### Individual dataset operations

### Patents

# Matches country codes to full names
langs.str <- c('Britain','Germany','France','United States')
patents.df[, "Country"] <- langs.str[match(patents.df[, "Country"], c("br", "de", "fr", "us"))]

## Authors

# Drop location information for authors (~ 400 rows)
patents.df[, "Authors"] <- gsub("\\\n.*$|\\([^()]*\\)", "", patents.df[, "Authors"])

## Field 

# Mark unsure rows, strip whitespace and update NA columns

patents.df <- markUnsure(patents.df, "Field")

# Multiple classifications are split most commonly by semi-colons and
# sometimes by commas
patents.df[, "Field"] <- gsub(",|\\s+;\\s+", "; ", patents.df[, "Field"])


# Capitalize words
patents.df[!is.na(patents.df[, "Field"]), "Field"] <- capwords(patents.df[!is.na(patents.df[, "Field"]), "Field"])

## Years

patents.df[grep("\\?", patents.df[, "Year"]), ] <- NA




### Clubs

## Scope

# Mark unsure rows, strip whitespace and update NA columns

clubs.df <- markUnsure(clubs.df, "Scope")

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
clubs.df[, "Imputed Year"] <- as.numeric(str_match(clubs.df[, "Start Year"], "\\d{4}"))

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

# Mark unsure rows, strip whitespace and update NA columns

firms.df <- markUnsure(firms.df, "Country")

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



