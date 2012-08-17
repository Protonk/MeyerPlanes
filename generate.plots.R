# Plotting script for patents.df 

### Preplotting

## Preplot functions for by year/country (or year/field, etc.) plots
## Generates dataset and ggplot object with correct title for each type
## Looks needlessly complex but cuts down on a lot of boilerplate later on 

# Arguments
# data.in: one of the four big dataframes
# by: Country or Field or potentially Author. A year varying column to plot/facet against
# start: start year
# end: end year

# we assume some column name consistency. I'm not there yet on the processing but I'll
# get there shortly
# the cool thing is you can just hit preplotGen() + geom_line() and boom, there's a lineplot
# of patents by countries over time. 

preplotGen <- function(data.in = patents.df, by = "Country", start = 1850, end = 1916) {
	# generate the titles automatically and size them properly.
	# I may drop the sizing element later
	titleGen <- function(type, start, end) {
		title.text <- switch(type,
												 Patents = paste0("Aeronautically-relevant patents by country ", start, '-', end),
												 Clubs = paste0("Aeronautical club starts by country ", start, '-', end),
												 Firms = paste0("Aeronautical firm starts by country ", start, '-', end),
												 Articles = paste0("Aeronautically-relevant articles by country ", start, '-', end))
		return(opts(title = title.text, plot.title = theme_text(size=20)))
	}					
	# deparse(substitute()) is an R trick to get a character representation of an object name
	type.inferred <- switch(deparse(substitute(data.in)),
													patents.df = "Patents",
													clubs.df = "Clubs",
													firms.df = "Firms",
													articles.df = "Articles")									
	# Split the dataset by year and the "by" column and generate a dataframe of counts
	preplot.df <- ddply(data.in, c("Year", by), "nrow")
	preplot.df <- preplot.df[complete.cases(preplot.df), ]
	names(preplot.df) <- c("Year", by, type.inferred)
	preplot.df[, "Year"] <- as.numeric(preplot.df[, "Year"])
	preplot.df[, "Country"] <- factor(preplot.df[, "Country"])
	# subset based on our year constraints
	preplot.df <- subset(preplot.df, Year >= start & Year <= end)
	# we generate and return a ggplot object. This lets us bundle the title together so we're 
	# not holding on to that information in two places								
	ggplot.obj <- ggplot(data = preplot.df, aes_string(x = "Year", y = type.inferred, colour = by))
	return(ggplot.obj + titleGen(type = type.inferred, start = start, end = end))
}



### Themes

## Web/Presentation Theme
## These will retain the gray background and some of the major/minor lines
## 



				







inset.legend <- opts(legend.background = theme_rect(fill="white"), 
                     legend.justification=c(0,1), legend.position=c(0,1), 
                     legend.text = theme_text(size = 16)) 

### Actual plots

country.plot <- ggplot(data = subset(by.country.df, Year > beg_patents & Year <= end_patents), aes(Year, Patents, colour = Country)) +
  geom_line(size = 1) + xlab("") + ylab('Patents') +
  		 opts(title = country.title, plot.title = theme_text(size=20),
       axis.text.x  = theme_text(size = 14))

inset.country <- country.plot + opts(legend.background = theme_rect(fill="white"), 
                                     legend.justification=c(0,1), legend.position=c(0,1), 
                                     legend.text = theme_text(size = 16))    
    
facet.country <- country.plot + facet_grid(Country ~ . , labeller = label_bquote('')) + 
                   guides(colour = FALSE) + opts(strip.background = theme_rect(colour = NA, fill = NA)) +
                   geom_text(aes_string(x = 1855, y = 40, label = "Country"), show_guide = FALSE, hjust = 0, size = 7)