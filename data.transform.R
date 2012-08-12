
# String processing library. Doesn't add too much to R but makes a number of functions
# more sensible
library(stringr)
# Data munching library. Split lists, arrays, dataframes and operate piecewise.
library(plyr)



### Patents

# Matches country codes to full names
langs.str <- c('Britain','Germany','France','United States')
patents.df[, "Where.filed"] <- langs.str[match(patents.df[, "Where.filed"], c("br", "de", "fr", "us"))]

## Cleanup rows

patents.df[, "Date.Applied.For"] <- gsub("[^0-9/]", "", patents.df[, "Date.Applied.For"])
patents.df[, "Date.Granted"] <- gsub("[^0-9/]", "", patents.df[, "Date.Granted"])
# Drop location information for authors (~ 400 rows)
patents.df[, "Authors"] <- gsub("\\\n.*$|\\(.*\\)", "", patents.df[, "Authors"])

## Classification 

# The NA string for this field appears to be "NANA"
# patents.df[grep("NANA", patents.df[, "Field"]), "Field"] <- NA

# You indication the presence of a question mark denoted a potentially unsure 
# classification. We note this and remove the question mark
patents.df[, "Classification Unsure"] <- grepl("\\?+", patents.df[, "Field"])
# also drop trailing spaces. Leave unknowns
patents.df[, "Field"] <- sub("\\?{2}", "Unknown", patents.df[, "Field"])
patents.df[, "Field"] <- sub("\\?|\\s+$", "", patents.df[, "Field"])

# Multiple classifications are split most commonly by semi-colons and
# sometimes by commas
patents.df[, "Field"] <- gsub(",|\\s+;\\s+", "; ", patents.df[, "Field"])

# casefolding from chartr()'s help page 
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s,1,1)),
{s <- substring(s,2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}
# Be careful, no NA checking
patents.df[!is.na(patents.df[, "Field"]), "Field"] <- capwords(patents.df[!is.na(patents.df[, "Field"]), "Field"])


### Reduce columns to those which will be meaningful to analysis and graphing

# A number of these are being dropped due to sparseness. Try 
# apply(patents.df, 2, function(s) sum(is.na(s))) if you're not convinced

patents.df <- patents.df[, c("Year.protection.applied",
                             "Filing.Country",
                             "Authors",
                             "Field",
                             "Classification Unsure",
                             "Patent.No",
                             "More.Date.Info",
                             "English.Title.Summary",
                             "Notes",
                             "Original.Language.Title",
                             "Aircraft.Related")]

### Generate title information

## Simple table of terms. Potentially useful for visualization
# lowercase and drop hyphens
term.table <- table(tolower(gsub("-", " ", patents.df[, "English.Title.Summary"])))
term.table <- term.table[term.table >= 3]
term.table <- term.table[order(term.table, decreasing = TRUE)]

## Categories for titles (need input here)

# Right now this is an arbitrary/ad-hoc classification for different titles. 
# 
airships.pat <- "balloon|dirigible|(air|aerial)[- ]?ship|luftschiff|aerostat|floatation"
airplane.pat <- "airplane|aeroplane|flugzeug"
flying.pat <- "^(flying|aerial)[- ]machine||apparatus|flugapparat|flugmaschine"
component.pat <- "device|propell(er|ing)|^apparatus|steering|propulsion|^machine"