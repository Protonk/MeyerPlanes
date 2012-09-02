


byYearRollup <- function(start = 1870, end = 1916) {
	output.list <- input.list <- list(Articles = articles.df,
										 							  Clubs = clubs.df,
										 								Exhibits = exhibits.df,
										 								Firms = firms.df,
										 								Patents = patents.df)
	for (i in names(input.list)) {
		output.list[[i]] <- ddply(input.list[[i]], "Year", "nrow")
		names(output.list[[i]])[2] <- i
	}
	# From http://stackoverflow.com/a/8097519/1188479 
	out.df <- Reduce(function(...) merge(..., , by = "Year", all=T), output.list)
	out.df[is.na(out.df)] <- 0
	return(subset(out.df, Year >= 1870 & Year <= 1916))
}

by.type.ts <- byYearRollup()

# don't include articles because we have language, not country.
byCountryRollup <- function(start = 1870, end = 1916) {
	output.list <- input.list <- list(Clubs = clubs.df,
										 								Exhibits = exhibits.df,
										 								Firms = firms.df,
										 								Patents = patents.df)
	for (i in names(input.list)) {
		output.list[[i]] <-ddplyMultiple(data = input.list[[i]], inputcol = "Country", comparison = "Year")
		test <- list(countries = NA, years = range(output.list[[i]][, "Year"]))
		output.list[[i]] <- dcast(output.list[[i]], Country ~ Year, value.var = "Count", fun.aggregate = sum)
		test$countries <- output.list[[i]][, "Country"]
	}
	browser()
	output.list 								
}

testme <- byCountryRollup()


Reduce(function(...) apply(..., 2, sum, na.rm=T), test)