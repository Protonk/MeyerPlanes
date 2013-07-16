


# Footnotes from https://github.com/kjhealy/5by5-figures/blob/master/shows.r


makeFootnote <- function(text = format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(.5)) {
  require(grid)
  pushViewport(viewport())
  grid.text(label= text ,
            x = unit(1,"npc") - unit(2, "mm"),
            y= unit(2, "mm"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}

# build from our pre-created path objects
footText <- function(source) {
  path.source <- switch(source,
                        "articles" = path.articles,
                        "clubs" = path.clubs,
                        "exhibits" = path.exhibits,
                        "firms" = path.firms,
                        "patents" = path.patents)
  # Find the creation data from the file browser
  source.date <- format(file.info(path.source)$ctime, format = "%B %d, %Y")
  return(paste("Peter Meyer; data retrieved", source.date))
}


# Naming for generated images
outputName <- function(plot.obj) {
  title <- plot.obj$labels$title
  if(length(plot.obj$facet) > 1) {
    title <- paste(title, " Faceted")
  }
	return(title)
}




# we can't do footnotes and ggsave() easily so this will serve as a means to 
# export the plots exactly as we want them
printPng <- function(source = "clubs", object, name) {
  location <- file.path(getwd(), "Images", paste(name, "png", sep = "."))
  png(filename = location, 
      width = 1024, height = 632)
  print(object)
  makeFootnote(text = footText(source))
  dev.off()
}

# Structure here is obviously amenable to one big call to sapply with these
# three in a matrix. But this works for now
plot.objects <- ls()[sapply(ls(), function(x) any(class(get(x)) %in% "ggplot"))]
plot.objects <- plot.objects[!grepl("fill", plot.objects)]
plot.sources <- sapply(strsplit(plot.objects, "\\."), `[`, 1)
plot.names <- character(length(plot.objects))
for (i in seq_along(plot.objects)) {
	plot.names[i] <- outputName(get(plot.objects[i]))
	}


for (i in seq_along(plot.objects)) {
  printPng(source = plot.sources[i], object = get(plot.objects[i]), name = plot.names[i])
}

        


