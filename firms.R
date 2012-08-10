# Drop last 3 columns (one contains sparse notes)
firms.df <- read.csv(file.path(getwd(), "Data", "Clubs-firms", "Firms_v46.2.csv"),
                     as.is = TRUE)[, 1:14]