#### General import for Patents, firms, clubs and articles
#### Whole file can be run as a script or sections can be run individually

### Utility functions

# Drops rows and columns with all NA values
# occurs with spreadsheet imports due to 
# formatting 

dropAllNA <- function(data) {
	row.ind <- apply(data, 1, function(x) any(!is.na(x)))
	col.ind <- apply(data, 2, function(x) any(!is.na(x)))
	data.out <- data[row.ind, ]
	data.out <- data.out[, col.ind]
	return(data.out)
}


### Patents

### Imports patent data and renames as appropriate
### Transformation and cleanup are in data.transform.R


## Path and read.csv

path.patents <- file.path(getwd(), "Data", "Patents", "Patents-Table 1.csv")

# na.strings modified to include empty date strings

patents.df <- read.csv(path.patents, as.is = TRUE, na.strings = c("NA", ""))

patents.df <- dropAllNA(patents.df)


# Rename columns

# drop a number of columns which won't be used for analysis. 

patents.df <- patents.df[, c(7, 1, 2:5, 9)]

# Year is the year protection was applied, not filing
names(patents.df) <- c("English.Title.Summary",
											 "Year",
                       "Country",
                       "Authors",
                       "Field",
                       "Patent.No",
                       "Original.Language.Title")
                       

### Clubs


## Paths and csv

path.clubs <- file.path(getwd(), "Data", "Clubs", "Clubs_v15.30.csv")

clubs.df <- read.csv(path.clubs, as.is = TRUE, na.strings = c("NA", ""))
clubs.df <- dropAllNA(clubs.df)

# Drop Notes (can return to these later but for now they're too much to handle automatically)

clubs.df <- clubs.df[, c(1, 9, 10, 2:5)]

names(clubs.df) <- c("Name",
										 "Start Year",
										 "End Year",
										 "Scope",
										 "Affiliate of",
										 "Country",
										 "City")
										 



### Firms

## Paths and csv

path.firms <- file.path(getwd(), "Data", "Firms", "Firms_v46.2.csv")


firms.df <- read.csv(path.firms, as.is = TRUE, na.strings = c("NA", ""))
firms.df <- dropAllNA(firms.df)

# Drop Notes (can return to these later but for now they're too much to handle automatically)

# Reorder a bit to get years together (just cosmetic)
firms.df <- firms.df[, c(1:3, 9, 4:8)]

names(firms.df) <- c("Short Name",
										 "Full Name",
										 "Start Year",
										 "End Year",
										 "Country",
										 "Place",
										 "Type",
										 "Founding Info",
										 "Product Info")
								 
										 
### Articles

path.articles <- file.path(getwd(), "Data", "Brockett", "articles0808.csv")

# right now titles are garbled due to encoding issues.
# NA strings for "0" and "-" in order to easily capture and note these strings
articles.df <- read.csv(path.articles, header = FALSE, as.is = TRUE, na.strings = c(NA, "-", "0"))

names(articles.df) <- c("Identifier", "Year", "Country", "Authors", "Field", "Title")



										 
