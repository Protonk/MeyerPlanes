library(plyr)
library(ggplot2)
# Working directory set as "MeyerPlanes"
reduced.path <- file.path(getwd(), "Data", "patents_small.csv")

reduced.df <- read.csv(file = reduced.path, as.is = TRUE, header = FALSE)

# --- define languages in order listed in file ---
langs.str <- c('Britain','Germany','France','United States')

names(reduced.df) <- c("Year", "Country", langs.str)
# Replace values in country column with full names
reduced.df[, "Country"] <- langs.str[match(reduced.df[, "Country"], c("br", "de", "fr", "us"))]

# Construct by year and by country-year tables
by.year.df <- ddply(reduced.df, "Year", "nrow")
by.year.country.df <- ddply(reduced.df, c("Year", "Country"), "nrow")
names(by.year.country.df)[3] <- names(by.year.df)[2] <- "Patents"

# Convert to factor for plotting
by.year.country.df[, "Country"] <- factor(by.year.country.df[, "Country"])

# Adjustable title and limits
beg_plot <- 1850 ##beg_year
end_plot <- 1910 ##end_year
country.title <- paste0("Aeronautically-relevant patents by country\n", beg_plot, '-', end_plot)

# Summed by year
year.plot <- ggplot(data = subset(by.year.df, Year > beg_plot & Year <= end_plot),
                    aes(Year, Patents)) + geom_line() + xlab('') +
                    ylab('Count of Publications') + 
                    opts(title = sub(" by country", "", country.title))
# summed by country
country.plot <- ggplot(data = subset(by.year.country.df, Year > beg_plot & Year <= end_plot),
                       aes(Year, Patents, colour = Country)) + geom_line() + xlab('') +
                       opts(title = country.title) + ylab('Count of Publications')
