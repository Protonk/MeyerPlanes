## Get counts for all types by country

getCounts <- function(...) {
  require(reshape2)
  full.list <- list(
    Patents = preplotGen(data.in = patents.df,
                        data.type = "Patents",
                        by.var = "Country",
                        ...),
    Clubs = preplotGen(data.in = clubs.df,
                      data.type = "Clubs",
                      by.var = "Country",
                      ...),
    Firms = preplotGen(data.in = firms.df,
                      data.type = "Firms",
                      by.var = "Country",
                      ...),
    Exhibits = preplotGen(data.in = exhibits.df,
                         data.type = "Exhibits",
                         by.var = "Country",
                         ...)
  )


  full.df <- Reduce(function(...) {
                      merge(..., all=T)},
                    full.list)
  full.melt <- melt(full.df, id.vars = c("Year", "Country"))
  full.melt <- genThreshold(...,
                            Data = full.melt,
                            collect = "Country",
                            measure = "value")
  names(full.melt) <- c("Year", "Country",
                        "Type", "Count")

  return(full.melt)
}

linePlot <- function(data, 
                     scaled = TRUE) {
  if(scaled) {
    scale.vals <- ddply(full.test[complete.cases(data), ],
                        c("Type"),
                        function(x) {
                          scale(x[, "Count"], center = FALSE)
                        })[, 2]
    data[!is.na(data[, "Count"]), "Count"] <- scale.vals
  }
  plot <- ggplot(data) + geom_line(aes(x = Year, y = Count, colour = Type)) + facet_wrap(~ Country)
  return(plot)
}
