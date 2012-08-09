## Read in data about clubs and firms 

clubs.df <- read.csv(file.path(getwd(), "Data", "Clubs-firms", "Clubs_v15.30.csv"),
                     as.is = TRUE, na.strings = c("NA", ""))
# Drop last 3 columns (one contains sparse notes)
firms.df <- read.csv(file.path(getwd(), "Data", "Clubs-firms", "Firms_v46.2.csv"),
                     as.is = TRUE)[, 1:14]


### CLUBS ###

## Scope

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


