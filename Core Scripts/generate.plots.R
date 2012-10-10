# Plotting script for patents.df 

library(ggplot2) 

### Preplotting

## Generate labels

# Add a column for annotation (sorry, but ggplot is built like this)
# idea for building a small data frame and attaching w/
# group = NULL from here: http://trinkerrstuff.wordpress.com/2012/09/01/add-text-annotations-to-ggplot2-faceted-plot/
labelLoc <- function(Data , By, Type) {
	
	labels.df <- ddply(Data, By, function(x) sum(x[, Type]))
	names(labels.df)[2] <- "Count.Notation"
	labels.df[, 1] <- as.character(labels.df[, 1])

	# Locations
	x.loc <- min(Data[, "Year"], na.rm = TRUE) + 1
	y.loc <- 0.6*max(ddply(Data, c(By), function(x) max(x[, Type]))[, 2])

	labels.df[, "x"] <- x.loc
	labels.df[, "y.By"] <- y.loc
	labels.df[, "y.Count"] <- 0.6*y.loc
	return(labels.df)
}


## Preplot functions for by year/country (or year/field, etc.) plots
## Generates list for preplotting so we can pluck out variables of interest for later.

preplotGen <- function(data.in = patents.df, by.var = "Country", start = 1850, end = 1909, threshold = 6, plot.other = TRUE) {
	
	# deparse(substitute()) is an R trick to get a character representation of an object name
	type.inferred <- switch(deparse(substitute(data.in)),
													patents.df = "Patents",
													clubs.df = "Clubs",
													firms.df = "Firms",
													articles.df = "Articles")		
	# Range of years
	year.range <- c(start, end)																			
	##  Split the dataset by year and the "by.var" column and generate a dataframe of counts
	if (type.inferred == "Firms") {
		preplot.df <- ddplyMultiple(data = data.in, inputcol = by.var, comparison = "Year")
	} else {	
		preplot.df <- ddply(data.in, c("Year", by.var), "nrow")
	}
	names(preplot.df) <- c("Year", by.var, type.inferred)
	
	# Cleanup
	preplot.df <- preplot.df[complete.cases(preplot.df), ]
	preplot.df[, "Year"] <- as.numeric(preplot.df[, "Year"])
	preplot.df[, by.var] <- factor(preplot.df[, by.var])
	
	# subset based on our year constraints
	preplot.df <- subset(preplot.df, Year >= start & Year <= end)
	
	
	# Accepts same arguments from local function environment and
	# generates plot info (and notation) for left out groups

	genThreshold <- function(threshold, Data = preplot.df, By = by.var, Type = type.inferred, plot.other) {

		# Tabulates the countries or languages (or whatever is specified in "By") and cuts off 
		# below a certain threshold
		items.retained <- names(table(Data[, By])[order(table(Data[, By]), decreasing = TRUE)][1:threshold])
		
		# All of the "other" results summed by year into 
		# their own category (Count.Notation added as well)
		other <- Data[!Data[, By] %in% items.retained, ]
		if (nrow(other) != 0 & plot.other) {
			other <- ddply(other, "Year", function(x) sum(x[, Type]))
			names(other)[2] <- Type
			other[, By] <- "Other"
			
			other <- other[, c(By, "Year", Type)]
		# Added back to the original dataframe and the factor levels condensed
			output.df <- rbind(Data[Data[, By] %in% items.retained, ], other)
			output.df[, By] <- factor(as.character(output.df[, By]))
		} else {
			output.df <- Data[Data[, By] %in% items.retained, ]
		}
		return(output.df)
	}

	preplot.df <- genThreshold(threshold = threshold, plot.other = plot.other)
	
	# Title text
	
	by.var.text <- paste0(tolower(by.var), " ", start, '-', end)
	type.text <- switch(type.inferred,
											 Patents = "Aeronautically-relevant patents by",
											 Clubs = "Aeronautical club starts by",
											 Firms = "Aeronautical firm starts by",
											 Articles = "Aeronautically-relevant articles by")
	
	# Add a dataframe for annotation (sorry, but ggplot is built like this)
	labels.df <- labelLoc(Data = preplot.df, By = by.var, Type = type.inferred)
	
	# return a list object so we can pluck out what we need to plot later 
	# and not carry arguments around							 
	return(list(Data = preplot.df,
							Type = type.inferred,
							Range = year.range,
							By = by.var,
							Labels = labels.df,
							Title = paste(type.text, by.var.text, sep = " ")))
}
preplotGen(data.in = patents.df, by.var = "Country", start = 1860, end = 1916)

# Plots by year should come from a common expectation of structure.
# Year is the cleaned up start year, publication year or year applied (depending on the dataset)
# Country (or language, or anything else)

# add facet labels
insetFacetLabel <- function(preplot, facet = NULL) {
	if (!is.null(facet)) {
		By <- facet
	} else {
		By <-preplot$By
	}
					
	return(list(Labels = geom_text(data = preplot$Labels, aes_string(x = "x", y = "y.By", label = By, colour = By, group = NULL), show_guide = FALSE, hjust = 0, size = 7),
							N = geom_text(data = preplot$Labels, aes(x = x, y = y.Count, label = paste("N =", Count.Notation), group = NULL), show_guide = FALSE, hjust = 0, size = 5),
							Facet = facet_grid(paste(By, "~ .") , labeller = label_bquote(''))))    
}




### Themes

## General theme options


inset.legend <- opts(legend.background = theme_rect(fill="white"), 
                     legend.justification=c(0,1), legend.position=c(0,1), 
                     legend.text = theme_text(size = 16)) 

# requested changes to display options

meyer.theme <- opts(plot.title = theme_text(size=20),
									  axis.text.x  = theme_text(size = 14),
									  axis.text.y = theme_text(size = 12),
									  axis.title.y = theme_text(size = 13, angle = 90),
									  panel.background = theme_rect(fill='#EBEBEB', colour=NA))

## Web/Presentation Theme
## These will retain the gray background and some of the major/minor lines
## 




#### Actual plots



### Patents

## Generating the graph primitives

# Going to 1916 here
# Generate the list first. This way if we want to subset further or otherwise fiddle with
# data (like ordering factors) we can
patents.list <- preplotGen(data.in = patents.df, by.var = "Country", start = 1860, end = 1909)

# This is all we need for bar chart. geom_bar() looks for a "fill" variable, not "colour"
# We use stat = "identity" because otherwise ggplot2 will try and sum up columns 
# We've already done that with ddply
# "stack" is the default presentation


patents.country.fill <- ggplot(data = patents.list$Data, 
															 aes_string(x = "Year", y = patents.list$Type, fill = patents.list$By)) +
													xlab("") + ylab(paste(patents.list$Type, "per year")) + 
													opts(title = patents.list$Title) + geom_bar(stat = "identity") + 
													meyer.theme


## Adding theme and layer changes

patents.country.inset <- patents.country.fill + inset.legend

## Faceting

# Somewhat more complicated due to the nature of facet_grid()
# the options drop the facet labels 

patents.country.facet <- patents.country.fill +  insetFacetLabel(patents.list) +
 opts(strip.background = theme_rect(colour = NA, fill = NA)) + guides(fill = FALSE)





### Clubs

## Preplotting

# I start at 1895 for firms because we don't see anything much before then
# Set a threshold (you can change this) for minimum number of clubs. We save it as 
# an object so we can put it in the footnote later
clubs.list <- preplotGen(data.in = clubs.df, by.var = "Country", start = 1895, end = 1909, threshold = 6)
clubs.list.1860 <- preplotGen(data.in = clubs.df, by.var = "Country", start = 1860, end = 1909, threshold = 6)

# Bar chart, similar to the patent plot

clubs.country.fill <- ggplot(data = clubs.list$Data, 
															 aes_string(x = "Year", y = clubs.list$Type, fill = clubs.list$By)) +
													xlab("") + ylab(paste(clubs.list$Type, "per year")) + 
													opts(title = clubs.list$Title) + geom_bar(stat = "identity") +
													meyer.theme

clubs.country.fill.1860 <- ggplot(data = clubs.list.1860$Data, 
															 aes_string(x = "Year", y = clubs.list$Type, fill = clubs.list.1860$By)) +
													xlab("") + ylab(paste(clubs.list.1860$Type, "per year")) + 
													opts(title = clubs.list.1860$Title) + geom_bar(stat = "identity") +
													meyer.theme


## Theme and layer changes are likewise relient on the same syntax and functions

clubs.country.inset <- clubs.country.fill + inset.legend

clubs.country.inset.1860 <- clubs.country.fill.1860 + inset.legend



### faceting

clubs.country.facet.1860 <- clubs.country.fill.1860 + insetFacetLabel(clubs.list.1860) +
	opts(strip.background = theme_rect(colour = NA, fill = NA)) + guides(fill = FALSE)

clubs.country.facet <- clubs.country.fill + insetFacetLabel(clubs.list) +
	opts(strip.background = theme_rect(colour = NA, fill = NA)) + guides(fill = FALSE)


### Firms

## Preplotting

# I start at 1895 for firms because we don't see anything much before then
# Firm preplotting is a bit different to the large number of (coded) multinational firms
# Top 6 countries plotted. We can do more than 6 for some plots but 6 is sensible	
firms.list <- preplotGen(data.in = firms.df, by.var = "Country", start = 1895, end = 1909, threshold = 6)


# Bar chart, similar to the patent plot

firms.country.fill <- ggplot(data = firms.list$Data, 
															 aes_string(x = "Year", y = firms.list$Type, fill = firms.list$By)) +
													xlab("") + ylab(paste(firms.list$Type, "per year")) + 
													opts(title = firms.list$Title) + geom_bar(stat = "identity") +
													meyer.theme

## Theme and layer changes are likewise relient on the same syntax and functions

firms.country.inset <- firms.country.fill + inset.legend

### faceting


firms.country.facet <- firms.country.fill + insetFacetLabel(firms.list) +
	opts(strip.background = theme_rect(colour = NA, fill = NA)) + guides(fill = FALSE)


### Club/Firm plots

clubs.flat <- clubs.list[["Data"]]
firms.flat <- firms.list[["Data"]]
clubs.flat[, "Country"] <- as.character(clubs.flat[, "Country"])
firms.flat[, "Country"] <- as.character(firms.flat[, "Country"])

merged.flat <- merge(clubs.flat, firms.flat, by = c("Country", "Year"), all = TRUE)
merged.flat[is.na(merged.flat)] <- 0
# Italy has no firm starts and Russia has no club starts. Canada has very few of either
merged.flat <- merged.flat[!merged.flat[, "Country"] %in% c("Italy", "Russia", "Canada"), ]

library(reshape2)

cf.melt <- melt(merged.flat, id.vars = c("Country", "Year"), measure.vars = c("Clubs", "Firms"))

names(cf.melt) <- c("Country", "Year", "Type", "Start")

rm(firms.flat, clubs.flat, merged.flat)


clubs.firms.list <- list(Data = subset(cf.melt, Year >= 1895 & Year <= 1909),
												 Type = "Start",
												 Range = c(1895, 1909),
												 By = "Country",
												 Title = "Combined firm and club starts 1895-1916")

clubs.firms.list$Labels <- labelLoc(Data = clubs.firms.list$Data, By = "Country", Type = clubs.firms.list$Type)
				

clubs.firms.fill <- ggplot(data = clubs.firms.list$Data, aes(x = Year, y = Start, fill = Type)) + 
	xlab("") + ylab("Starts per year") + opts(title = clubs.firms.list$Title) + 
	geom_bar(stat = "identity", position = "dodge") + meyer.theme


### Articles

# Start in 1870 as there is a much longer lead-in for articles than clubs, etc.
# ends at 1909 because Brockett is 1910
# set threshold to 4
articles.list <- preplotGen(data.in = articles.df, by.var = "Language", start = 1870, end = 1909, threshold = 4)


articles.lang.fill <- ggplot(data = articles.list$Data, 
															 aes_string(x = "Year", y = articles.list$Type, fill = articles.list$By)) +
													xlab("") + ylab(paste(articles.list$Type, "per year")) + 
													opts(title = articles.list$Title) + geom_bar(stat = "identity") +
													meyer.theme
													
articles.lang.inset <- articles.lang.fill + inset.legend	

articles.lang.facet <- articles.lang.fill + insetFacetLabel(articles.list) +
  opts(strip.background = theme_rect(colour = NA, fill = NA)) + guides(fill = FALSE)							