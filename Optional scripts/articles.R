### Plot articles

articles.country <- overallPlot(By = "Language",
                             data = "Articles")


articles.facet <- overallPlot(By = "Language",
                           data = "Articles",
                           facet = TRUE)

articles.line <- indvLinePlot(data = articles.df,
                              data.type = "Articles",
                              by.var = "Language",
                              start = 1890,
                              end = 1909, threshold = 6)