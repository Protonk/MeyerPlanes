


firms.reduced.df <- catMultipleCountries()

firms.reduced.df[, "Country"] <- as.character(firms.reduced.df[, "Country"])

other.countries <- names(table(firms.reduced.df[, "Country"]))[table(firms.reduced.df[, "Country"]) <= 6]
firms.reduced.df[firms.reduced.df[, "Country"] %in% other.countries, "Country"] <- "Other"
firms.reduced.df[, "Country"] <- factor(firms.reduced.df[, "Country"])

firms.reduced.df <- firms.reduced.df[complete.cases(firms.reduced.df), ]

  
prelim.firm.bar <- ggplot(data = subset(firms.reduced.df, Year > 1860 & Year < 1920), aes(x = Year, y = `Firm Starts`, fill = Country)) + geom_bar(position = "stack", stat= "identity")









