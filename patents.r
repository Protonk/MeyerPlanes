###### Patents 

##### Model and transformation code specific to patents. Only code which has little possible
##### overlap with other datasets goes in here

### Generate title information from Summary

## Simple table of terms. Potentially useful for visualization
# lowercase and drop hyphens
term.table <- table(tolower(gsub("-", " ", patents.df[, "English.Title.Summary"])))
term.table <- term.table[term.table >= 3]
term.table <- term.table[order(term.table, decreasing = TRUE)]

## Categories for titles (need input here)

# Right now this is an arbitrary/ad-hoc classification for different titles. 
# 
# airships.pat <- "balloon|dirigible|(air|aerial)[- ]?ship|luftschiff|aerostat|floatation"
# airplane.pat <- "airplane|aeroplane|flugzeug"
# flying.pat <- "^(flying|aerial)[- ]machine||apparatus|flugapparat|flugmaschine"
# component.pat <- "device|propell(er|ing)|^apparatus|steering|propulsion|^machine"