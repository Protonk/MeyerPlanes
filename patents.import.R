path.patents.table <- file.path(getwd(), "Data", "Patents-Table 1.csv")

# na.strings modified to include empty date strings
# Columns 15-19 are empty
patents.df <- read.csv(path.patents.table, as.is = TRUE, na.strings = c("NA", ""))[, 1:14]

# exact same conversion as on reduced dataset
langs.str <- c('Britain','Germany','France','United States')
patents.df[, "Where.filed"] <- langs.str[match(patents.df[, "Where.filed"], c("br", "de", "fr", "us"))]

## Cleanup rows

patents.df[, "date.applied.for"] <- gsub("[^0-9/]", "", patents.df[, "date.applied.for"])
patents.df[, "date.granted"] <- gsub("[^0-9/]", "", patents.df[, "date.granted"])