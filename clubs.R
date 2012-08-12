library(stringr)

### CLUBS ###

## Scope

# note unknown/potentially unknown classification.
# Then remove "?s"
unknown.scope <- grepl("\\?", clubs.df[, "Scope"])
clubs.df[, "Scope"] <- sub("\\?", "", clubs.df[, "Scope"])
clubs.df[, "Unknown Scope"] <- unknown.scope

#trim whitespace
clubs.df[, "Scope"] <- gsub("^\\s+|\\s+$", "", clubs.df[, "Scope"])
clubs.df[nchar(clubs.df[, "Scope"]) == 0, "Scope"] <- NA 

# cleanup entries
clubs.df[, "Scope"] <- sub("Sttate", "State", clubs.df[, "Scope"])
clubs.df[, "Scope"] <- sub("Internati?onal,? Scientific", "International Scientific", clubs.df[, "Scope"])
clubs.df[, "Scope"] <- sub(" Club|, Multi-State", "", clubs.df[, "Scope"])
clubs.df[, "Scope"] <- sub("Univesrsity", "University", clubs.df[, "Scope"])

# Split into multiples 
# I'll be doing this for firms, clubs, articles and patents, so this will be turned into a
# reusable function
# Breaks columns w/ multiple classifications into single classifications

breakMultiples <- function(data, column, split.regex = ", | and ", binary = TRUE) {
  # strsplit creates a list from the splitting regex
  mult.list <- strsplit(data[, column], split.regex)
  # grab length for each list element
  mult.cols <- laply(mult.list, length)
  # fill space out first (makes it a little faster and easier to understand)
  prefill.mat <- matrix(NA,
                        nrow = nrow(data),
                        ncol = max(mult.cols))
  colnames(prefill.mat) <- 
  # select only those elements which are in the ith category
  for (i in 1:max(mult.cols)) {
    prefill.mat[, i] <- laply(mult.list, `[`, i)
  }
  df.out <- data.frame(lapply(data.frame(prefill.mat), factor))

  # Note for where there are multiple columns
  if (binary) {
    df.out <- data.frame(cbind(df.out, mult.cols > 1))
    }
  else {
    df.out <- data.frame(cbind(df.out, mult.cols))
  }
  names(df.out) <- c(paste(column, "Category", 1:max(mult.cols), sep = " "), "Multiple Cats")
  return(df.out)
}

## Years

# TODO: Note "?"
#       establish cutoff years of interest
#       convert to numeric

# Rough matching for years. Warns about warnings introduced. 
clubs.df[, "Matched.Start.Year"] <- as.numeric(str_match(clubs.df[, "Start.Year"], "\\d{4}"))

## Country

# Cleanup names
clubs.df[, "Country"] <- gsub("^\\s+|\\s+$", "", clubs.df[, "Country"])
clubs.df[, "Country"] <- sub("UISA|^US$", "USA", clubs.df[, "Country"])
clubs.df[, "Country"] <- sub("[Gg]er(man$|many-|mamy|manu)", "Germany", clubs.df[, "Country"])
clubs.df[, "Country"] <- sub("England,\\s?GB,\\s?UK.?$", "England, GB, UK", clubs.df[, "Country"])

# Build simplified Country factor
clubs.df[, "Country.Factor"] <- "Other"
clubs.df[grepl("UK", clubs.df[, "Country"]), "Country.Factor"] <- "United Kingdom"
clubs.df[grepl("Germany", clubs.df[, "Country"]), "Country.Factor"] <- "Germany"
clubs.df[grepl("France", clubs.df[, "Country"]), "Country.Factor"] <- "France"
clubs.df[grepl("US", clubs.df[, "Country"]), "Country.Factor"] <- "United States"

clubs.df[, "Country.Factor"] <- factor(clubs.df[, "Country.Factor"],
                                       levels = c("Germany",
                                                  "France",
                                                  "United Kingdom",
                                                  "United States",
                                                  "Other"))



#### Plotting

starts.year.country <- ddply(clubs.df, c("Matched.Start.Year", "Country.Factor"), "nrow")

starts.year.country <- starts.year.country[!is.na(starts.year.country[, "Matched.Start.Year"]), ]



# Build preplot object with data and desired relationships
beg_clubs <- 1860 ##beg_year
end_clubs <- 1915 ##end_year

clubs.title <- paste0("Aviation Club Starts ", beg_clubs, '-', end_clubs)

# Footnotes from https://github.com/kjhealy/5by5-figures/blob/master/shows.r

makeFootnote <- function(footnoteText = format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(.5)) {
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y= unit(2, "mm"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}



clubs.country.preplot <- ggplot(data = subset(starts.year.country, Matched.Start.Year > beg_clubs & Matched.Start.Year < end_clubs), 
                             aes(x = Matched.Start.Year, xend = Matched.Start.Year,
                                 y = nrow, yend = 0, colour = Country.Factor)) +
                           opts(strip.background = theme_rect(colour = NA, fill = NA),
                                title = clubs.title, plot.title = theme_text(size=20),
                                axis.text.x  = theme_text(size = 14))



# this next operation is heavily loaded
# We add segments, the facet, text and adjust some options. 
# Each operation separated by a '+' is relatively independent
# so look at them individually
clubs.country.plot <- clubs.country.preplot + facet_grid(Country.Factor ~ . , labeller = label_bquote('')) +
geom_text(aes_string(x = 1870, y = 30, label = "Country.Factor"), show_guide = FALSE, hjust = 0, size = 8) +
  guides(colour = FALSE) + xlab("") + ylab('Club Starts') + geom_segment(size = 2.5)
  
                                                               
# A stacked bar chart offers another method. I'm not happy with this
# right now and I'll return to it, but we'll see.

clubs.bar <-  clubs.country.preplot + geom_bar(aes_string(fill = "Country.Factor", group = "Country.Factor"), position="stack", stat="identity")

clubs.bar <- clubs.bar + opts(legend.background = theme_rect(fill="white"), 
                              legend.justification=c(0,1), legend.position=c(0,1), 
                              legend.text = theme_text(size = 16)) +
               xlab("") + ylab('Club Starts') + scale_fill_discrete("Country") +
               scale_colour_discrete(guide = FALSE)

