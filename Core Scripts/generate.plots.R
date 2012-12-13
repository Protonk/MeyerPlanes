
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


# Emulate/capture ggplot2 colors 
# from http://stackoverflow.com/a/8197703/1188479

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

color.key <- gg_color_hue(10)
names(color.key) <- c('United Kingdom','United States', 'Germany','France', 
											'Russia', 'Italy', 'Canada', 'Norway', 
											'Switzerland', 'Other')

## Combine multiple locations with some other column

# The idea here is to expand on our ddply function for one column
# We have multiple columns for classification/scope/etc. and we want
# to sum each up by year or some other column

# Right now we only have the "nrow" function in ddply. future versions will
# allow for all functions ddply allows


ddplyMultiple <- function(data, inputcol, comparison, split.regex = ", | and ") {
	# generate breakouts and match with comparison column
	# strsplit creates a list from the splitting regex
  mult.list <- str_split(data[, inputcol], split.regex)
  # max columns to form matrix & name columns
  max.cols <- max(sapply(mult.list, length))
  # fill space out first (makes it a little faster and easier to understand)
  prefill.mat <- matrix(NA,
                        nrow = nrow(data),
                        ncol = max.cols)
  # select only those elements which are in the ith category
  for (i in 1:max.cols) {
    prefill.mat[, i] <- sapply(mult.list, `[`, i)
  }
  multiple.breakout <- data.frame(lapply(data.frame(prefill.mat), factor))

  names(multiple.breakout) <- c(paste(inputcol, "Category", 1:max.cols, sep = " "))
	multiple.comb <- cbind(multiple.breakout, data[, comparison])

	# Create a 0 row data frame so we can populate it in a for loop
	# multiple.breakout should have columns for each separator (at least one)
	# plus a column for the "split" logical variable
	df.reduced <- data.frame(matrix(NA, ncol = 3, nrow = 0))
	# name it based on input
	working.names <- names(df.reduced) <- c(inputcol,
																					comparison,
																					"Count")
	# For each classifier count the comparisons and add them
	# to df.reduced
	for (i in 1:ncol(multiple.breakout)) {
    intermediate.reduced <- ddply(multiple.comb, c(i,ncol(multiple.comb)), "nrow")
    names(intermediate.reduced) <- working.names
    df.reduced <- rbind(df.reduced, intermediate.reduced)
  }
  # convert from factor			
  # If years get turned into factors R will return the factor codes unless converted
  # to characters first								 
	df.reduced[, comparison] <- as.numeric(as.character(df.reduced[, comparison]))
	df.reduced[, inputcol] <- as.character(df.reduced[, inputcol])
	df.reduced <- df.reduced[complete.cases(df.reduced), ]
	df.reduced[, inputcol] <- factor(df.reduced[, inputcol])
	df.reduced <- df.reduced[, c(comparison, inputcol, "Count")]
	return(df.reduced)
}


## Preplot functions for by year/country (or year/field, etc.) plots
## Generates list for preplotting so we can pluck out variables of interest for later.


preplotGen <- function(data.in = patents.df, by.var = "Country", threshold = 6) {
	
	# deparse(substitute()) is an R trick to get a character representation of an object name
	type.inferred <- switch(deparse(substitute(data.in)),
													patents.df = "Patents",
													clubs.df = "Clubs",
													firms.df = "Firms",
													articles.df = "Articles",
													exhibits.df = "Exhibits")		
																		
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
	
	# Accepts same arguments from local function environment and
	# generates plot info (and notation) for left out groups

	genThreshold <- function(threshold, Data = preplot.df, By = by.var, Type = type.inferred) {

		# Tabulates the countries or languages (or whatever is specified in "By") and cuts off 
		# below a certain threshold
		items.retained <- aggregate(Data[, Type], list(Named = Data[, By]), FUN = sum)[order(aggregate(Data[, Type], list(By = Data[, By]), FUN = sum)[, "x"], decreasing = TRUE), "Named"][1:threshold]
		# All of the "other" results summed by year into 
		# their own category (Count.Notation added as well)
		other <- Data[!Data[, By] %in% items.retained, ]
		if (nrow(other) != 0) {
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

	preplot.df <- genThreshold(threshold = threshold)
	
	
	# Add a dataframe for annotation (sorry, but ggplot is built like this)
	labels.df <- labelLoc(Data = preplot.df, By = by.var, Type = type.inferred)
	
	# Create a consistent color scheme
	
	color.out <- color.key[names(color.key) %in% labels.df[, by.var]]
	
	# return a list object so we can pluck out what we need to plot later 
	# and not carry arguments around							 
	return(list(Data = preplot.df,
							Type = type.inferred,
							By = by.var,
							Labels = labels.df,
							Colors = color.out))
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
					
	return(list(Labels = geom_text(data = preplot$Labels, aes_string(x = "x", y = "y.By", label = By, colour = By, group = NULL), show_guide = FALSE, hjust = 0, size = 7),
							N = geom_text(data = preplot$Labels, aes(x = x, y = y.Count, label = paste("N =", Count.Notation), group = NULL), show_guide = FALSE, hjust = 0, size = 5),
							Facet = facet_grid(paste(By, "~ .") , labeller = label_bquote(''))))    
}




### Themes

## General theme options


inset.legend <- opts(legend.background = theme_rect(fill="white"), 
                     legend.justification=c(0,1), legend.position=c(0,1), 
                     legend.text = theme_text(size = 18)) 

# requested changes to display options

meyer.theme <- opts(plot.title = theme_text(size=22),
									  axis.text.x  = theme_text(size = 14),
									  axis.text.y = theme_text(size = 13),
									  axis.title.y = theme_text(size = 15, angle = 90),
									  panel.background = theme_rect(fill='#EBEBEB', colour=NA))


## Web/Presentation Theme
## These will retain the gray background and some of the major/minor lines
## 




#### Actual plots



### Patents

## Generating the graph primitives

# Going to 1914 here
# Generate the list first. This way if we want to subset further or otherwise fiddle with
# data (like ordering factors) we can
patents.list <- preplotGen(data.in = patents.df, by.var = "Country")

# This is all we need for bar chart. geom_bar() looks for a "fill" variable, not "colour"
# We use stat = "identity" because otherwise ggplot2 will try and sum up columns 
# We've already done that with ddply
# "stack" is the default presentation


patents.country.fill <- ggplot(data = patents.list$Data, 
															 aes_string(x = "Year", y = patents.list$Type, fill = patents.list$By)) +
													xlab("") + ylab(paste(patents.list$Type, "per year")) + 
													xlim(1860, 1914) +
													opts(title = "Aeronautically relevant patents by Country 1860-1914") + 
													geom_bar(stat = "identity") + scale_fill_manual(values = patents.list$Colors) +
													meyer.theme 


## Adding theme and layer changes

patents.country.inset <- patents.country.fill + inset.legend

## Faceting

# Somewhat more complicated due to the nature of facet_grid()
# the options drop the facet labels 

patents.country.facet <- patents.country.fill +  insetFacetLabel(patents.list) +
 opts(strip.background = theme_rect(colour = NA, fill = NA)) + guides(fill = FALSE) +
 scale_colour_manual(values = patents.list$Colors)





### Clubs

## Preplotting

# I start at 1895 for firms because we don't see anything much before then
# Set a threshold (you can change this) for minimum number of clubs. We save it as 
# an object so we can put it in the footnote later
clubs.list <- preplotGen(data.in = clubs.df, by.var = "Country", threshold = 4)

# Bar chart, similar to the patent plot

clubs.country.fill <- ggplot(data = clubs.list$Data, 
															 aes_string(x = "Year", y = clubs.list$Type, fill = clubs.list$By)) +
													xlab("") + ylab(paste(clubs.list$Type, "per year")) +
													xlim(1895, 1912) + 
													opts(title = "Aeronautical club starts by country 1895-1912") + 
													geom_bar(stat = "identity") + scale_fill_manual(values = clubs.list$Colors) +
													meyer.theme

clubs.country.fill.1860 <- ggplot(data = clubs.list$Data, 
															 aes_string(x = "Year", y = clubs.list$Type, fill = clubs.list$By)) +
													xlab("") + ylab(paste(clubs.list$Type, "per year")) +
													xlim(1860, 1912) + 
													opts(title = "Aeronautical club starts by country 1860-1912") +  
													geom_bar(stat = "identity") + scale_fill_manual(values = clubs.list$Colors) + 
													meyer.theme 


## Theme and layer changes are likewise relient on the same syntax and functions

clubs.country.inset <- clubs.country.fill + inset.legend

clubs.country.inset.1860 <- clubs.country.fill.1860 + inset.legend



### faceting

clubs.country.facet.1860 <- clubs.country.fill.1860 + insetFacetLabel(clubs.list) +
	opts(strip.background = theme_rect(colour = NA, fill = NA)) + guides(fill = FALSE) +
	scale_colour_manual(values = clubs.list$Colors)

clubs.country.facet <- clubs.country.fill + insetFacetLabel(clubs.list) +
	opts(strip.background = theme_rect(colour = NA, fill = NA)) + guides(fill = FALSE) +
	scale_colour_manual(values = clubs.list$Colors)


### Firms

## Preplotting

# I start at 1895 for firms because we don't see anything much before then
# Firm preplotting is a bit different to the large number of (coded) multinational firms
# Top 6 countries plotted. We can do more than 6 for some plots but 6 is sensible	
firms.list <- preplotGen(data.in = firms.df, by.var = "Country", threshold = 5)


# Bar chart, similar to the patent plot

firms.country.fill <- ggplot(data = firms.list$Data, 
															 aes_string(x = "Year", y = firms.list$Type, fill = firms.list$By)) +
													xlab("") + ylab(paste(firms.list$Type, "per year")) + 
													xlim(1895, 1909) + 
													opts(title = "Aeronautical firm starts by country 1895-1909") + 
													geom_bar(stat = "identity") + scale_fill_manual(values = firms.list$Colors) +
													meyer.theme 

## Theme and layer changes are likewise relient on the same syntax and functions

firms.country.inset <- firms.country.fill + inset.legend

### faceting


firms.country.facet <- firms.country.fill + insetFacetLabel(firms.list) +
	opts(strip.background = theme_rect(colour = NA, fill = NA)) + guides(fill = FALSE) +
	scale_colour_manual(values = firms.list$Colors)


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
articles.list <- preplotGen(data.in = articles.df, by.var = "Language", threshold = 4)


articles.lang.fill <- ggplot(data = articles.list$Data, 
															 aes_string(x = "Year", y = articles.list$Type, fill = articles.list$By)) +
													xlab("") + ylab(paste(articles.list$Type, "per year")) + 
													xlim(1850, 1909) +
													opts(title = "Aeronautically relevant articles by language 1850-1909") +
													geom_bar(stat = "identity") +
													meyer.theme
													
articles.lang.inset <- articles.lang.fill + inset.legend	

articles.lang.facet <- articles.lang.fill + insetFacetLabel(articles.list) +
  opts(strip.background = theme_rect(colour = NA, fill = NA)) + guides(fill = FALSE)	

## article line plots

# harcoded estimates
# these will eventually be moved higher up in the flow


articles.est <- read.csv("/Users/protonk/R/Dropbox/MeyerPlanes/Data/Publications/estimates.csv", as.is = TRUE) 
articles.total <- ddply(articles.list$Data, "Year", function(x) data.frame(Articles = sum(x[, "Articles"])))
# rbind and drop last row of article total
articles.total <- rbind(articles.total[-nrow(articles.total), ], articles.est)
rm(articles.est)
articles.total <- data.frame(articles.total, Language = "Total")

# merge with article data (makes plotting easier)
articles.list.hc <- articles.list

articles.list.hc$Data <- rbind(articles.list.hc$Data, articles.total)
articles.list.hc$Data <- articles.list.hc$Data[order(articles.list.hc$Data[, "Year"]), ]

# adjust hardcoded values to the list

articles.list.hc$Title <- "Aeronautically-relevant articles by language 1870-1916"
articles.list.hc$Range <- c(1870, 1916)

# plot it!

ggplot(data = articles.list.hc$Data, aes_string(x = "Year", y = articles.list.hc$Type, colour = articles.list.hc$By)) + xlab("") + 
  geom_line(aes(linetype = Language), size = 1.5) + 
  scale_linetype_manual("", values = c(rep("solid", 5), "dashed"), guide = FALSE) +
  ylab(paste(articles.list.hc$Type, "per year")) + 
  opts(title = articles.list.hc$Title)  + 
  meyer.theme + xlim(1890, 1916) + inset.legend

### Exhibits

exhibits.list <- preplotGen(data.in = exhibits.df, by.var = "Country", threshold = 4)	

exhibits.country.fill <- 	ggplot(data = exhibits.list$Data, 
															 aes_string(x = "Year", y = exhibits.list$Type, fill = exhibits.list$By)) +
													xlab("") + ylab(paste(exhibits.list$Type, "per year")) + 
													xlim(1870, 1916) +
													opts(title = "Aeronautically relevant exhibitions by country 1870-1916") +
													geom_bar(stat = "identity") + scale_fill_manual(values = exhibits.list$Colors) +
													meyer.theme 				

exhibits.country.inset <- exhibits.country.fill + inset.legend		

exhibits.country.facet <- exhibits.country.fill + insetFacetLabel(exhibits.list) +
  opts(strip.background = theme_rect(colour = NA, fill = NA)) + guides(fill = FALSE) +
	scale_colour_manual(values = exhibits.list$Colors)

											
													