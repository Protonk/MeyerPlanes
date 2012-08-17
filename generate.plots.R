# Plotting script for patents.df 

### Preplotting

## Preplot functions for by year/country (or year/field, etc.) plots
## Generates list for preplotting so we can pluck out variables of interest for later.

preplotGen <- function(data.in = patents.df, by.var = "Country", start = 1850, end = 1916) {
	# deparse(substitute()) is an R trick to get a character representation of an object name
	type.inferred <- switch(deparse(substitute(data.in)),
													patents.df = "Patents",
													clubs.df = "Clubs",
													firms.df = "Firms",
													articles.df = "Articles")		
	# Range of years
	year.range <- c(start, end)																			
	# Split the dataset by year and the "by.var" column and generate a dataframe of counts
	preplot.df <- ddply(data.in, c("Year", by.var), "nrow")
	preplot.df <- preplot.df[complete.cases(preplot.df), ]
	names(preplot.df) <- c("Year", by.var, type.inferred)
	preplot.df[, "Year"] <- as.numeric(preplot.df[, "Year"])
	preplot.df[, by.var] <- factor(preplot.df[, by.var])
	# subset based on our year constraints
	preplot.df <- subset(preplot.df, Year >= start & Year <= end)

	# Title text
	by.var.text <- paste0(tolower(by.var), " ", start, '-', end)
	type.text <- switch(type.inferred,
											 Patents = "Aeronautically-relevant patents by",
											 Clubs = "Aeronautical club starts by",
											 Firms = "Aeronautical firm starts by",
											 Articles = "Aeronautically-relevant articles by")
	# return a list object so we can pluck out what we need to plot later 
	# and not carry arguments around							 
	return(list(Data = preplot.df,
							Type = type.inferred,
							Range = year.range,
							By = by.var,
							Title = paste(type.text, by.var.text, sep = " ")))
}





### Themes

## General theme options

axis.options <- opts(axis.text.x  = theme_text(size = 14))
				

inset.legend <- opts(legend.background = theme_rect(fill="white"), 
                     legend.justification=c(0,1), legend.position=c(0,1), 
                     legend.text = theme_text(size = 16)) 

title.theme <- opts(plot.title = theme_text(size=20))


## Web/Presentation Theme
## These will retain the gray background and some of the major/minor lines
## 




### Actual plots


# Plots by year should come from a common expectation of structure.
# Year is the cleaned up start year, publication year or year applied (depending on the dataset)
# Country (or language, or anything else)

# Generate ggplot objects with the correct mapping and scale.
# we can now just save these or type plotObjGen() + geom_line() at the console
# with the right arguments and get the plot we want.

plotObjGen <- function(fill = FALSE, ...) {
	preplot.list <- preplotGen(...)
	if (fill) {	
		ggobj <- ggplot(data = preplot.list$Data, aes_string(x = "Year",
																												 y = preplot.list$Type,
																												 fill = preplot.list$By))
	} else {
		ggobj <- ggplot(data = preplot.list$Data, aes_string(x = "Year",
																												 y = preplot.list$Type,
																												 colour = preplot.list$By))																											 
	}
	ggobj <- ggobj + xlab('') + ylab(preplot.list$Type) + opts(title = preplot.list$Title)
	return(ggobj)													
}

  		 

inset.country <- country.plot + opts(legend.background = theme_rect(fill="white"), 
                                     legend.justification=c(0,1), legend.position=c(0,1), 
                                     legend.text = theme_text(size = 16))    
    
facet.country <- country.plot + facet_grid(Country ~ . , labeller = label_bquote('')) + 
                   guides(colour = FALSE) + opts(strip.background = theme_rect(colour = NA, fill = NA)) +
                   geom_text(aes_string(x = 1855, y = 40, label = "Country"), show_guide = FALSE, hjust = 0, size = 7)