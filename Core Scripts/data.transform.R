

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
	# multiple.breakout should have columns for each separator (at least one)
	# plus a column for the "split" logical variable
	df.reduced <- data.frame(matrix(NA, ncol = 3, nrow = 0))
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
  # If years get turned into factors R will return the factor codes unless converted
  # to characters first								 
	df.reduced[, comparison] <- as.numeric(as.character(df.reduced[, comparison]))
	df.reduced[, inputcol] <- as.character(df.reduced[, inputcol])
	df.reduced <- df.reduced[complete.cases(df.reduced), ]
	df.reduced[, inputcol] <- factor(df.reduced[, inputcol])
	df.reduced <- df.reduced[, c(comparison, inputcol, "Count")]
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
langs.str <- c('United Kingdom','Germany','France','United States', 'Norway')
patents.df[, "Country"] <- langs.str[match(patents.df[, "Country"], c("br", "de", "fr", "us", "no"))]

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

# notations for years with question-marks rare enough to be simply marked NA.

patents.df[grep("\\?", patents.df[, "Year"]), ] <- NA

# the current dataset doesn't have any non-matching rows but this is a reasonable first filter.
patents.df[, "Year"] <- as.numeric(str_match(patents.df[, "Year"], "[0-9]{4}"))



### Clubs

## Scope

# Mark unsure rows, strip whitespace and update NA columns

clubs.df <- markUnsure(clubs.df, "Scope")

# cleanup entries
clubs.df[, "Scope"] <- sub("Sttate", "State", clubs.df[, "Scope"])
clubs.df[, "Scope"] <- sub("Internati?onal,? Scientific", "International Scientific", clubs.df[, "Scope"])
clubs.df[, "Scope"] <- sub(" Club|, Multi-State", "", clubs.df[, "Scope"])
clubs.df[, "Scope"] <- sub("Univesrsity", "University", clubs.df[, "Scope"])



## Country

# Cleanup names
clubs.df[, "Country"] <- gsub("^\\s+|\\s+$", "", clubs.df[, "Country"])
clubs.df[, "Country"] <- sub("UISA|^US$", "USA", clubs.df[, "Country"])
clubs.df[, "Country"] <- sub("[Gg]er(man$|many-|mamy|manu)", "Germany", clubs.df[, "Country"])
clubs.df[, "Country"] <- sub("England,\\s?GB,\\s?UK.?$", "England, GB, UK", clubs.df[, "Country"])

# Replace country labels wholesale.
# I may go back and change this but for now we're a little simpler
clubs.df[grepl("UK", clubs.df[, "Country"]), "Country"] <- "United Kingdom"
clubs.df[grepl("Germany", clubs.df[, "Country"]), "Country"] <- "Germany"
clubs.df[grepl("France", clubs.df[, "Country"]), "Country"] <- "France"
clubs.df[grepl("US", clubs.df[, "Country"]), "Country"] <- "United States"
# We list both Austria and Hungary for club location originally (but not for firms)
clubs.df[grepl("Austria(-| )Hungary", clubs.df[, "Country"]), "Country"] <- "Austria-Hungary"



## Years

# TODO: Note "?"
#       establish cutoff years of interest
#       convert to numeric

# Mark "?" years as well as years which are noted as "1890 or earlier"
 
clubs.df <- markUnsure(clubs.df, "Start Year")
clubs.df[, "Unknown Start Year"] <- clubs.df[, "Unknown Start Year"] | grepl("earlier", clubs.df[, "Start Year"])

# Rough matching for years.
clubs.df[, "Year"] <- as.numeric(str_match(clubs.df[, "Start Year"], "[0-9]{4}"))




### Firms


## Country

# Mark unsure rows, strip whitespace and update NA columns

firms.df <- markUnsure(firms.df, "Country")

# clean up country listings
firms.df[, "Country"] <- gsub("^US$|^.USA$", "USA", firms.df[, "Country"])
firms.df[, "Country"] <- gsub(".*[Hh]ungary|^AH$", "Austria-Hungary", firms.df[, "Country"])
firms.df[, "Country"] <- gsub("Germanu", "Germany", firms.df[, "Country"])
firms.df[, "Country"] <- gsub(" \\([^()]*\\)", "", firms.df[, "Country"])
firms.df[, "Country"] <- gsub("Czechoslo-\\s+vakia", "Czechoslovakia", firms.df[, "Country"])

# Standardize names to match patents and clubs
# bit trickier due to splits so we use str_replace() instead of sub()
# the syntax is different but the function is similar

firms.df[, "Country"] <- str_replace(firms.df[, "Country"], "USA", "United States")
firms.df[, "Country"] <- str_replace(firms.df[, "Country"], "UK", "United Kingdom")

# separators for split listings

firms.df[, "Country"] <- sub(";", ",", firms.df[, "Country"])




## Years


# Generate column for imputed years as we'll
# drop a number of qualifiers

firms.df[, "Year"] <- as.numeric(str_match(firms.df[, "Start Year"], "[0-9]{4}"))



### Articles

## Languages

art.languages <- c("French", "English", "German", "Italian", "Russian",
									 "Swedish", "Spanish", "Portuguese", "Dutch", "Unknown",
									 "Unknown", "Latin", "Czech", "French", "Multiple")

# Match numbering to strings. The second argument to match() is a bit tricky
# because we skip some numbers in the db so we can't simply take a sequence of 
# numbers from 1-15.
articles.df[, "Language"] <- art.languages[match(articles.df[, "Language"],  seq_along(art.languages))]
articles.df[, "Language"] <- factor(articles.df[, "Language"])


## Field

# Proper NA string for empty Field cells
# Will warn if encoding isn't figured out. 

articles.df[grepl("^\\s*$", articles.df[, "Field"]), "Field"] <- NA



### Exhibitions

## Country
# Normalize country names from abbreviations
exhibition.countries <- c("United Kingdom", "France", "United States", "Belgium", NA, "Russia", "Germany", "Switzerland", "Canada")

exhibits.df[, "Country"] <- exhibition.countries[match(exhibits.df[, "Country"], c("Br", "Fr", "US", "Belgium", NA, "Ru", "De", "Sw", "Ca"))]

## Year 

exhibits.df[, "Year Unsure"] <- grepl("\\?|~|;", exhibits.df[, "Year"])

# Drops all but the first year noted.
exhibits.df[, "Year"] <- gsub("^\\D?([0-9]{4}).*", "\\1", exhibits.df[, "Year"])



