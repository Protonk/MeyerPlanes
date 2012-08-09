

path.patents.table <- file.path(getwd(), "Data", "Patents", "Patents-Table 1.csv")

# na.strings modified to include empty date strings
# Columns 15-19 are empty
patents.df <- read.csv(path.patents.table, as.is = TRUE, na.strings = c("NA", ""))[1:1715, 1:14]

# exact same conversion as on reduced dataset
langs.str <- c('Britain','Germany','France','United States')
patents.df[, "Where.filed"] <- langs.str[match(patents.df[, "Where.filed"], c("br", "de", "fr", "us"))]

# Rename (preliminary)

names(patents.df) <- c("Year.protection.applied",
                       "Filing.Country",
                       "Authors",
                       "Field",
                       "Patent.No",
                       "More.Date.Info",
                       "English.Title.Summary",
                       "Notes",
                       "Original.Language.Title",
                       "Supplementary.To",
                       "Aircraft.Related",
                       "Date.Applied.For",
                       "Date.Granted",
                       "Global.Patent.ID")
                       
## Cleanup rows

patents.df[, "Date.Applied.For"] <- gsub("[^0-9/]", "", patents.df[, "Date.Applied.For"])
patents.df[, "Date.Granted"] <- gsub("[^0-9/]", "", patents.df[, "Date.Granted"])
# Drop location information for authors (~ 400 rows)
patents.df[, "Authors"] <- gsub("\\\n.*$|\\(.*\\)", "", patents.df[, "Authors"])

## Classification 

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
patents.df[, "Field"] <- capwords(patents.df[, "Field"])



