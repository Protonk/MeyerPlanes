




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

