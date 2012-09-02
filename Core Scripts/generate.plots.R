# Plotting script for patents.df 

library(ggplot2) 

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
	
	# Add a column for annotation (sorry, but ggplot is built like this)
	n.type <- ddply(preplot.df, by.var, function(x) sum(x[, type.inferred]))
	n.type[, 2] <- factor(paste0("N = ", n.type[, 2]))
	names(n.type)[2] <- "Count.Notation"
	
	preplot.df <- merge(preplot.df, n.type, by = by.var)
	
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

# add facet labels
insetFacetLabel <- function(preplot, facet = NULL) {
	if (!is.null(facet)) {
		By <- facet
	} else {
		By <-preplot$By
	}
	# Text location generated based on ranges of inputs
	text.loc <- c(min(preplot$Data[, "Year"]) + 1,
								0.6*max(ddply(preplot$Data, c(By), function(x) max(x[, preplot$Type]))[, 2]))						
	return(list(Labels = geom_text(data = preplot$Data, aes_string(x = text.loc[1], y = text.loc[2], label = By, colour = By), show_guide = FALSE, hjust = 0, size = 7),
							N = geom_text(data = preplot$Data, aes_string(x = text.loc[1], y = 0.3*text.loc[2], label = "Count.Notation"), show_guide = FALSE, hjust = 0, size = 5),
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


patents.country.fill <- ggplot(data = patents.list$Data, 
															 aes_string(x = "Year", y = patents.list$Type, fill = patents.list$By)) +
													xlab("") + ylab(paste(patents.list$Type, "per year")) + 
													opts(title = patents.list$Title) + geom_bar(stat = "identity")


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
clubs.list <- preplotGen(data.in = clubs.df, by.var = "Country", start = 1895, end = 1916)

# Set a threshold (you can change this) for minimum number of firms. We save it as 
# an object so we can put it in the footnote later
club.threshold <- 6

club.countries.retained <- names(table(clubs.df[, "Country"])[order(table(clubs.df[, "Country"]), decreasing = TRUE)][1:club.threshold])

clubs.list$Data <- clubs.list$Data[clubs.list$Data[, "Country"] %in% club.countries.retained, ]


# Bar chart, similar to the patent plot

clubs.country.fill <- ggplot(data = clubs.list$Data, 
															 aes_string(x = "Year", y = clubs.list$Type, fill = clubs.list$By)) +
													xlab("") + ylab(paste(clubs.list$Type, "per year")) + 
													opts(title = clubs.list$Title) + geom_bar(stat = "identity")



## Theme and layer changes are likewise relient on the same syntax and functions

clubs.country.inset <- clubs.country.fill + inset.legend

### faceting


clubs.country.facet <- clubs.country.fill + insetFacetLabel(clubs.list) +
	opts(strip.background = theme_rect(colour = NA, fill = NA)) + guides(fill = FALSE)


### Firms

## Preplotting

# I start at 1895 for firms because we don't see anything much before then
firms.list <- preplotGen(data.in = firms.df, by.var = "Country", start = 1895, end = 1916)

# Firm preplotting is a bit different to the large number of (coded) multinational firms


# Top 6 countries plotted. We can do more than 6 for some plots but 6 is sensible

firm.threshold <- 6

firm.countries.retained <- names(table(firms.df[, "Country"])[order(table(firms.df[, "Country"]), decreasing = TRUE)][1:firm.threshold])

firms.list$Data <- firms.list$Data[firms.list$Data[, "Country"] %in% firm.countries.retained, ]

# Bar chart, similar to the patent plot

firms.country.fill <- ggplot(data = firms.list$Data, 
															 aes_string(x = "Year", y = firms.list$Type, fill = firms.list$By)) +
													xlab("") + ylab(paste(firms.list$Type, "per year")) + 
													opts(title = firms.list$Title) + geom_bar(stat = "identity")

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
clubs.firms.list <- list(Data = subset(cf.melt, Year >= 1895 & Year <= 1916),
												 Type = "Start",
												 Range = c(1895, 1916),
												 By = "Type",
												 Title = "Combined firm and club starts 1895-1916")

n.type <- ddply(clubs.firms.list$Data, "Country", function(x) sum(x[,"Start"]))
# this will squawk with a warning. 
n.type[, 2] <- factor(paste0("N = ", n.type[, 2]))
names(n.type)[2] <- "Count.Notation"
	
clubs.firms.list$Data <- merge(clubs.firms.list$Data, n.type, by = "Country")
rm(n.type)

				

clubs.firms.fill <- ggplot(data = clubs.firms.list$Data, aes_string(x = "Year", y = clubs.firms.list$Type, fill = clubs.firms.list$By)) +
	xlab("") + ylab("Starts per year") + opts(title = clubs.firms.list$Title) + 
	geom_bar(stat = "identity", position = "dodge")
	
	
clubs.firms.facet <- clubs.firms.fill + insetFacetLabel(clubs.firms.list, facet = "Country") +
  opts(strip.background = theme_rect(colour = NA, fill = NA)) + guides(fill = FALSE)


### Articles

# Start in 1870 as there is a much longer lead-in for articles than clubs, etc.
# ends at 1909 because Brockett is 1910
articles.list <- preplotGen(data.in = articles.df, by.var = "Language", start = 1870, end = 1909)

articles.threshold <- 4

articles.countries.retained <- names(table(articles.df[, "Language"])[order(table(articles.df[, "Language"]), decreasing = TRUE)][1:articles.threshold])

articles.list$Data <- articles.list$Data[articles.list$Data[, "Language"] %in% articles.countries.retained, ]


articles.country.fill <- ggplot(data = articles.list$Data, 
															 aes_string(x = "Year", y = articles.list$Type, fill = articles.list$By)) +
													xlab("") + ylab(paste(articles.list$Type, "per year")) + 
													opts(title = articles.list$Title) + geom_bar(stat = "identity")
													
articles.country.inset <- articles.country.fill + inset.legend	

articles.country.facet <- articles.country.fill + insetFacetLabel(articles.list) +
  opts(strip.background = theme_rect(colour = NA, fill = NA)) + guides(fill = FALSE)							