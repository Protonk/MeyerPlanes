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
	rownames(data.out) <- as.character(1:nrow(data.out))
	return(data.out)
}


### Patents

### Imports patent data and renames as appropriate
### Transformation and cleanup are in data.transform.R


## Path and read.csv

path.patents <- file.path(getwd(),
													"Data",
													"Patents",
													"AeroPatents_2013_07_09converted.csv")

# na.strings modified to include empty date strings

patents.df <- read.csv(path.patents, as.is = TRUE, na.strings = c("NA", ""))

patents.df <- dropAllNA(patents.df)


# Rename columns

# drop a number of columns which won't be used for analysis. 

patents.df <- patents.df[, c(7, 1, 2:5, 10, 12)]

# Year is the year protection was applied, not filing
names(patents.df) <-  c("English.Title.Summary",
												"Year",
												"Country",
												"Authors",
												"Field",
												"Patent.No",
												"Original.Language.Title",
												"Related.To.Aircraft")

### Clubs


## Paths and csv

path.clubs <- file.path(getwd(), "Data", "Clubs", "Clubs_v15.30.csv")

clubs.df <- read.csv(path.clubs, as.is = TRUE, na.strings = c("NA", ""))
clubs.df <- dropAllNA(clubs.df)

# Drop Notes (for now they're too much to handle automatically)

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

path.firms <- file.path(getwd(), "Data", "Firms", "FirmsV46.3.csv")


firms.df <- read.csv(path.firms, as.is = TRUE, na.strings = c("NA", ""))
firms.df <- dropAllNA(firms.df)

# Drop Notes (for now they're too much to handle automatically)

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

path.articles <- file.path(getwd(), "Data", "Publications", "articles0808.csv")


# NA strings for "0" and "-" in order to easily capture and note these strings
articles.df <- read.csv(path.articles,
												header = FALSE, as.is = TRUE,
												na.strings = c(NA, "-", "0"))

names(articles.df) <- c("Identifier", "Year", "Language",
												"Authors", "Field", "Title")

# iconv() can convert the formatting over, no problem. No more complaints
articles.df <- data.frame(llply(articles.df,
																function(x) iconv(x, "latin1", "UTF-8")),
													stringsAsFactors = FALSE)




### Exhibitions

## Paths and csv

path.exhibits <- file.path(getwd(), "Data", "Exhibitions", "exhibitions_0m.csv")

exhibits.df <- read.csv(path.exhibits, as.is = TRUE, na.strings = c("NA", ""))
exhibits.df <- dropAllNA(exhibits.df)


names(exhibits.df) <-  c("Name", 
												 "Type", 
												 "Place", 
												 "Country", 
												 "Year", 
												 "Month", 
												 "Start.day", 
												 "Days", 
												 "Notes......prominent.in.lit.", 
												 "Sources", 
												 "X")
exhibits.df <- exhibits.df[, 1:8]

													 