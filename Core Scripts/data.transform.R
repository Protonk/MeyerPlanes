

### Utility functions

## casefolding from chartr()'s help page 
# No NA checking done beforehand
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s,1,1)),
{s <- substring(s,2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


## naming splits
## split by names is a bit more quirky

# name.split <- str_split(patents.df[, "Authors"], ", and|;")
commonNameRev <- function(x) {
	if (str_count(x, ",") == 1) {
		x <- paste0(rev(str_split_fixed(x, ",\\s+", 2)), collapse = " ")
	} else if (str_count(x, ",") == 2) {
		x <- paste0(str_split_fixed(x, ",\\s+", 3)[c(2,3,1)], collapse = " ")
	}
	return(x)
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



