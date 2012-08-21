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
	##  Split the dataset by year and the "by.var" column and generate a dataframe of counts
	
	preplot.df <- ddply(data.in, c("Year", by.var), "nrow")

	names(preplot.df) <- c("Year", by.var, type.inferred)
	
	# Cleanup
	preplot.df <- preplot.df[complete.cases(preplot.df), ]
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

# Plots by year should come from a common expectation of structure.
# Year is the cleaned up start year, publication year or year applied (depending on the dataset)
# Country (or language, or anything else)

# Generate ggplot objects with the correct mapping and scale.
# we can now just save these or type plotObjGen() + geom_line() at the console
# with the right arguments and get the plot we want.

plotObjGen <- function(preplot, fill = FALSE) {
	Count <- preplot$Type
	By <- preplot$By
	
	if (fill) {	
		ggobj <- ggplot(data = preplot$Data, 
									  aes_string(x = "Year", y = Count, fill = By))
	} else {
		ggobj <- ggplot(data = preplot$Data, 
									  aes_string(x = "Year", y = Count, colour = By))																											 
	}
	ggobj <- ggobj + xlab('') + ylab(paste(Count, "per year")) + opts(title = preplot$Title)
	return(ggobj)													
}

# add facet labels
insetFacetLabel <- function(preplot) {
	By <-preplot$By
	# Text location generated based on ranges of inputs
	text.loc <- c(min(preplot$Data[, "Year"]) + 1,
								0.6*max(ddply(preplot$Data, c(By), function(x) max(x[, preplot$Type]))[, 2]))
	return(list(Labels = geom_text(data = preplot$Data, aes_string(x = text.loc[1], y = text.loc[2], label = By, colour = By), show_guide = FALSE, hjust = 0, size = 7),
							Facet = facet_grid(paste(By, "~ .") , labeller = label_bquote(''))))    
}

### Themes

## General theme options


inset.legend <- opts(legend.background = theme_rect(fill="white"), 
                     legend.justification=c(0,1), legend.position=c(0,1), 
                     legend.text = theme_text(size = 16)) 

# requested changes to display options

meyer.theme <- opts(plot.title = theme_text(size=20),
									  axis.text.x  = theme_text(size = 14))

## Web/Presentation Theme
## These will retain the gray background and some of the major/minor lines
## 




#### Actual plots



### Patents

## Generating the graph primitives

# Going to 1916 here
# Generate the list first. This way if we want to subset further or otherwise fiddle with
# data (like ordering factors) we can
patents.list <- preplotGen(data.in = patents.df, by.var = "Country", start = 1860, end = 1916)

# This is all we need for bar chart. geom_bar() looks for a "fill" variable, not "colour"
# We use stat = "identity" because otherwise ggplot2 will try and sum up columns 
# We've already done that with ddply
# "stack" is the default presentation

patents.country.fill <- plotObjGen(preplot = patents.list, fill = TRUE)  + geom_bar(stat = "identity")

# Line plot. 
patents.country.line <- plotObjGen(preplot = patents.list, fill = FALSE)  + geom_line(size = 1)


## Adding theme and layer changes

patents.country.inset <- patents.country.fill + inset.legend

## Faceting

# Somewhat more complicated due to the nature of facet_grid()
# the options drop the facet labels 

patents.country.facet <- patents.country.fill +  insetFacetLabel(patents.list)$Facet + insetFacetLabel(patents.list)$Labels +
 opts(strip.background = theme_rect(colour = NA, fill = NA)) + guides(fill = FALSE)





### Clubs

## Preplotting


clubs.list <- preplotGen(data.in = clubs.df, by.var = "Country_Factor", start = 1880, end = 1916)

# The variable has a space. However we also don't want
# the title to be "country factor" so we can change it easily here

clubs.list$Title <- sub(".factor ", " ", clubs.list$Title)


# Bar chart, similar to the patent plot

clubs.country.fill <- plotObjGen(preplot = clubs.list, fill = TRUE) + geom_bar(stat = "identity")

## Theme and layer changes are likewise relient on the same syntax and functions

clubs.country.inset <- clubs.country.fill + inset.legend

### faceting


clubs.country.facet <- clubs.country.fill + insetFacetLabel(clubs.list)$Facet + insetFacetLabel(clubs.list)$Labels +
	opts(strip.background = theme_rect(colour = NA, fill = NA)) + guides(fill = FALSE)



