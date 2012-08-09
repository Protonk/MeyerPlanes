## Read in data about clubs and firms 

clubs.df <- read.csv(file.path(getwd(), "Data", "Clubs-firms", "Clubs_v15.30.csv"),
                     as.is = TRUE, na.strings = c("NA", ""))
# Drop last 3 columns (one contains sparse notes)
firms.df <- read.csv(file.path(getwd(), "Data", "Clubs-firms", "Firms_v46.2.csv"),
                     as.is = TRUE)[, 1:14]


### CLUBS ###

## Scope

# note unknown/potentially unknown classification.
# Then remove "?s"
unknown.scope <- grepl("\\?", clubs.df[, "Scope"])
clubs.df[, "Scope"] <- sub("\\?", "", clubs.df[, "Scope"])

#trim whitespace
clubs.df[, "Scope"] <- gsub("^\\s+|\\s+$", "", clubs.df[, "Scope"])
clubs.df[nchar(clubs.df[, "Scope"]) == 0, "Scope"] <- NA 

# cleanup entries
clubs.df[, "Scope"] <- sub("Sttate", "State", clubs.df[, "Scope"])
clubs.df[, "Scope"] <- sub("Internati?onal,? Scientific", "International Scientific", clubs.df[, "Scope"])
clubs.df[, "Scope"] <- sub(" Club|, Multi-State", "", clubs.df[, "Scope"])
clubs.df[, "Scope"] <- sub("Univesrsity", "University", clubs.df[, "Scope"])

# Split into multiples 
# I'll be doing this for firms, clubs, articles and patents, so this will be turned into a
# reusable function

# captures "and" inside parens. I'll fix that in a bit
clubs.multiples <- strsplit(clubs.df[, "Scope"], ", | and ")
n.mult.cols <- max(laply(clubs.multiples, length))
# Select only the 2nd col 
second.col <- laply(clubs.multiples[sapply(clubs.multiples, length) == 2], `[`, 2)

sapply(clubs.multiples, length)

# TODO: Note "?"
#       Standardize
#       Convert to factor

## Years

# TODO: Note "?"
#       establish cutoff years of interest
#       convert to numeric

## Country

# Cleanup names
clubs.df[, "Country"] <- gsub("^\\s+|\\s+$", "", clubs.df[, "Country"])
clubs.df[, "Country"] <- sub("UISA|^US$", "USA", clubs.df[, "Country"])
clubs.df[, "Country"] <- sub("[Gg]er(man$|many-|mamy|manu)", "Germany", clubs.df[, "Country"])
clubs.df[, "Country"] <- sub("England,\\s?GB,\\s?UK.?$", "England, GB, UK", clubs.df[, "Country"])


