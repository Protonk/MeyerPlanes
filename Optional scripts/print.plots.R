


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
outputName <- function(plot.obj, layer) {
	xval <- as.character(plot.obj$mapping$x)
	short.date <- format(Sys.time(), format = "%b%d")
  # year ranges
	range <- ifelse(is.numeric(plot.obj$data[, xval]), paste(range(plot.obj$data[, xval]), collapse = "-"), NULL)
	# Faceted plots have multiple mappings. Ends up looking weird
  if (length(plot.obj$facet) > 1) {
    type <- paste(plot.obj$options$labels$y, "by", as.character(plot.obj$facet$rows))
	} else {
	  type <- paste(unlist(Filter(nchar, plot.obj$options$labels)), collapse = " by ")
	}
	return(paste(type, layer, range, short.date))
}




# we can't do footnotes and ggsave() easily so this will serve as a means to 
# export the plots exactly as we want them
printPng <- function(source = "clubs", plot.obj = clubs.country.fill, layer) {
  location <- file.path(getwd(), "Images", paste(outputName(plot.obj, layer), "png", sep = "."))
  png(filename = location, 
      width = 1024, height = 632)
  print(plot.obj)
  makeFootnote(text = footText(source))
  dev.off()
}

# Structure here is obviously amenable to one big call to sapply with these
# three in a matrix. But this works for now
plot.objects <- ls()[sapply(ls(), function(x) class(get(x))) %in% "ggplot"]
plot.sources <- sapply(strsplit(plot.objects, "\\."), `[`, 1)
plot.layers <- sapply(strsplit(plot.objects, "\\."), tail, 1)
  
for (i in seq_along(plot.objects)) {
  printPng(source = plot.sources[i], plot.obj = get(plot.objects[i]), layer = plot.layers[i])
}

        


