### Plot patents

patents.country <- overallPlot(By = "Country",
                             data = "Patents")


patents.facet <- overallPlot(By = "Country",
                           data = "Patents",
                           facet = TRUE)

indvLinePlot <- function(data,
                         data.type,
                         by.var,
                         start = 1860,
                         end = 1920,
                         ...) {
  start.y <- start
  end.y <- end

  preplot.df <- preplotGen(data.in = data,
                       data.type = data.type,
                       by.var = by.var,
                       start = start,
                       end = end,
                       ...)
  preplot.df <- ddply(preplot.df, "Year",
                   function(x) {
                      sum(x[, data.type], na.rm = TRUE)
                   })
  names(preplot.df) <- c("Year", data.type)
  plot <- ggplot(preplot.df) +
            geom_line(aes_string(x = "Year", y = data.type),
              size = 2, colour = "blue")
  plot <- plot + meyer.theme +
            ggtitle(paste(data.type, "per Year, ", start.y, "-", end.y)) +
            xlab("")
  return(plot)
}

patents.line <- indvLinePlot(data = patents.df,
                             data.type = "Patents",
                             by.var = "Country",
                             start = 1890,
                             end = 1910, threshold = 6)