


byYearRollup <- function(start = 1870, end = 1916) {
	output.list <- input.list <- list(Articles = articles.df,
										 							  Clubs = clubs.df,
										 								Exhibits = exhibits.df,
										 								Firms = firms.df,
										 								Patents = patents.df)
	for (i in names(input.list)) {
		output.list[[i]] <- ddply(input.list[[i]], "Year", i = NROW(piece))
	}
	# From http://stackoverflow.com/a/8097519/1188479 
	# will change as this takes forever
	out.df <- Reduce(function(...) merge(..., , by = "Year", all=T), output.list)
	return(subset(out.df, Year >= 1870 & Year <= 1916))
}