# Plotting script for patents.df 

# general variables for plotting

beg_patents <- 1850 ##beg_year
end_patents <- 1910 ##end_year


# Preplot for country-year
by.country.df <- ddply(patents.df, c("Year.protection.applied", "Filing.Country"), "nrow")

by.country.df <- by.country.df[grep("^\\d{4}$", by.country.df[, 1]), ]
names(by.country.df) <- c("Year", "Country", "Patents")
by.country.df[, "Country"] <- factor(by.country.df[, "Country"])
by.country.df[, "Year"] <- as.numeric(by.country.df[, "Year"])

country.title <- paste0("Aeronautically-relevant patents by country ", beg_patents, '-', end_patents)

country.plot <- ggplot(data = subset(by.country.df, Year > beg_patents & Year <= end_patents), aes(Year, Patents, colour = Country)) +
  geom_line(size = 1) + xlab("") + ylab('Patents') +
  opts(title = country.title, 
       plot.title = theme_text(size=20),
       axis.text.x  = theme_text(size = 14))

inset.country <- country.plot + opts(legend.background = theme_rect(fill="white"), 
                                     legend.justification=c(0,1), legend.position=c(0,1), 
                                     legend.text = theme_text(size = 16))    
    
facet.country <- country.plot + facet_grid(Country ~ . , labeller = label_bquote('')) + 
                   guides(colour = FALSE) + opts(strip.background = theme_rect(colour = NA, fill = NA)) +
                   geom_text(aes_string(x = 1855, y = 40, label = "Country"), show_guide = FALSE, hjust = 0, size = 7)