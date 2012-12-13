##### Simple script to run import , transformation and plot generation
##### Will also load required libraries
##### I will keep the local calls to load/require the libraries 
##### in case you want to run the scripts individually



#### Dependencies

### plyr
# plyr is a data manipulation package which helps eliminate a lot of pathological loops
# and needlessly complex structures. 

library(plyr)

## reshape2
# reshape2 offers a faster implementation of the melt/cast functionality in base R
# we use it to build some plots

library(reshape2)

### ggplot2
# ggplot2 (occasionally just referred to in the comments as ggplot) is the workhorse dependency
# for this project. All of the plots (so far) have been built using it. Some of the 
# syntax is a bit confusing but the advantages are deep. 

library(ggplot2)

## grid
# grid is needed to add footnotes to the plot. It is a helper package to ggplot2 for our
# purposes

library(grid)

## stringr
# Another Hadley Wickham package, stringr simplifies some text processing tasks. I haven't
# yet decided whether to migrate all string processing over to stringr

library(stringr)



#### Core scripts
### Need to be run in order

# Imports data and performs minimal processing
source(file.path(getwd(), "Core Scripts", "data.import.R"))

# Heavy text processing to standardize years, countries, languages, etc.
source(file.path(getwd(), "Core Scripts", "data.transform.R"))

# creates ggplot2 objects and reduced datasets for plotting
source(file.path(getwd(), "Core Scripts", "generate.plots.R"))