
### Preplotting

## Generate labels

# Add a column for annotation (sorry, but ggplot is built like this)
# idea for building a small data frame and attaching w/
# group = NULL from here: http://bit.ly/Obc0aA
labelLoc <- function(Data , By, Type) {	
	labels.df <- ddply(Data, By, function(x) sum(x[, Type]))
	names(labels.df)[2] <- "Count.Notation"
	labels.df[, 1] <- as.character(labels.df[, 1])

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

## Combine multiple locations with some other column

# The idea here is to expand on our ddply function for one column
# We have multiple columns for classification/scope/etc. and we want
# to sum each up by year or some other column

# Right now we only have the "nrow" function in ddply. future versions will
# allow for all functions ddply allows


ddplyMultiple <- function(data, inputcol, comparison,
                          split.regex = ", | and ") {
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

  names(multiple.breakout) <- c(paste(inputcol,
                                "Category",
                                1:max.cols,
                                sep = " "))
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
    intermediate.reduced <- ddply(multiple.comb,
                                  c(i,ncol(multiple.comb)),
                                  "nrow")
    names(intermediate.reduced) <- working.names
    df.reduced <- rbind(df.reduced, intermediate.reduced)
  }
  # convert from factor			
  # If years get turned into factors R will return the factor codes
  # unless converted to characters first								 
	df.reduced[, comparison] <- as.numeric(as.character(df.reduced[, comparison]))
	df.reduced[, inputcol] <- as.character(df.reduced[, inputcol])
	df.reduced <- df.reduced[complete.cases(df.reduced), ]
	df.reduced[, inputcol] <- factor(df.reduced[, inputcol])
	df.reduced <- df.reduced[, c(comparison, inputcol, "Count")]
	return(df.reduced)
}


## Preplot functions for by year/country (or year/field, etc.) plots
## Generates list for preplotting so we can pluck out variables of interest.


preplotGen <- function(data.in = patents.df,
                       data.type = "Patents",
                       by.var = "Country",
                       threshold = 6) {

	## Split the dataset by year and the "by.var" column
  ## and generate a dataframe of counts
	if (data.type == "Firms") {
		preplot.df <- ddplyMultiple(data = data.in,
                                inputcol = by.var,   
                                comparison = "Year")
	} else {	
		preplot.df <- ddply(data.in, c("Year", by.var), "nrow")
	}
	names(preplot.df) <- c("Year", by.var, data.type)
	
	# Cleanup
	preplot.df <- preplot.df[complete.cases(preplot.df), ]
	preplot.df[, "Year"] <- as.numeric(preplot.df[, "Year"])
	
	# Accepts same arguments from local function environment and
	# generates plot info (and notation) for left out groups

	genThreshold <- function(threshold,
                           Data = preplot.df,
                           By = by.var,
                           Type = data.type) {

		# Tabulates the countries or languages (or whatever is specified in "By")
    # and cuts off below a certain threshold
		items.retained <- aggregate(Data[, Type],
                                list(Named = Data[, By]),
                                FUN = sum)[order(
                                            aggregate(Data[, Type],
                                                      list(By = Data[, By]),
                                                      FUN = sum)[, "x"],
                                            decreasing = TRUE),
                                          "Named"][1:threshold]
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
	labels.df <- labelLoc(Data = preplot.df, By = by.var, Type = data.type)
	
	# Create a consistent color scheme
	color.key <- gg_color_hue(10)
  color.countries <- c('United Kingdom','United States', 'Germany','France', 
                       'Russia', 'Italy', 'Canada', 'Norway', 
                       'Switzerland',  'Spain', 'Other')
  color.key <- gg_color_hue(length(color.countries))
	color.out <- color.key[color.countries %in% labels.df[, by.var]]
	
	# return a list object so we can pluck out what we need to plot later 
	# and not carry arguments around							 
	return(list(Data = preplot.df,
							Type = data.type,
							By = by.var,
							Labels = labels.df,
							Colors = color.out))
}


# Plots by year should come from a common expectation of structure.
# Year is the cleaned up start year, publication year or year applied
# Country (or language, or anything else)

# add facet labels
insetFacetLabel <- function(preplot, facet = NULL) {
	if (!is.null(facet)) {
		By <- facet
	} else {
		By <-preplot$By
	}
	return(list(Labels = geom_text(data = preplot$Labels,
                                 aes_string(x = "x", y = "y.By",
                                     label = By, colour = By,
                                     group = NULL),
                                 show_guide = FALSE, hjust = 0, size = 7),
							N = geom_text(data = preplot$Labels,
                            aes(x = x, y = y.Count,
                                label = paste("N =", Count.Notation),
                                group = NULL),
                            show_guide = FALSE, hjust = 0, size = 5),
							Facet = facet_grid(paste(By, "~ .") ,
                                 labeller = label_bquote(''))))    
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
# Generate the list first. This way if we want to subset further or otherwise
# fiddle with data (like ordering factors) we can

overallPlot <- function(By = "Country",
                        data = c("Patents", "Clubs",
                                 "Firms", "Articles",
                                 "Exhibits"),
                        start = 1860,
                        end = 1914,
                        title = NULL,
                        facet = FALSE) {
  data.arg <- match.arg(data)
  data.src <- switch(data.arg,
                     Patents = patents.df,
                     Clubs = clubs.df,
                     Firms = firms.df,
                     Articles = articles.df,
                     Exhibits = exhibits.df)
  data.brushed <- subset(data.src, Year >= start & Year <= end)
  if(!is.null(title)) {
    plot.title <- paste(title)
  } else {
    plot.title <- paste("Aeronautically-relevant",
                        data.arg, "by", By,
                        paste0(start, "-", end))
  }
  preplot.l <- preplotGen(data.in = data.brushed,
                          by.var = By,
                          data.type = data.arg)
  plot.base <- ggplot()
  if(facet) {
    plot.base <- plot.base + insetFacetLabel(preplot.l) 
  }
  plot.out <- plot.base +
                geom_bar(data = preplot.l$Data,
                         aes_string(x = "Year",
                                    y = preplot.l$Type,
                                    fill = preplot.l$By),
                         stat = "identity") +
                scale_fill_manual(values = preplot.l$Colors)
  plot.out <- plot.out +
                ylab(paste(preplot.l$Type, "per year")) + ylab("") +
                ggtitle(plot.title)
                meyer.theme
  if(facet) {
    plot.out <- plot.out +
                   opts(strip.background = theme_rect(colour = NA, fill = NA)) +
                   guides(fill = FALSE)
  } else {
    plot.out <- plot.out + inset.legend
  }
  return(plot.out)
}
