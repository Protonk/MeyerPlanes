### Plot patents

patents.country <- overallPlot(By = "Country",
                             data = "Patents")


patents.facet <- overallPlot(By = "Country",
                           data = "Patents",
                           facet = TRUE)

patents.line <- indvLinePlot(data = patents.df,
                             data.type = "Patents",
                             by.var = "Country",
                             start = 1890,
                             end = 1910, threshold = 6)