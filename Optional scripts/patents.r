### Plot patents

patents.country <- overallPlot(By = "Country",
                             data = "Patents")


patents.facet <- overallPlot(By = "Country",
                           data = "Patents",
                           facet = TRUE)