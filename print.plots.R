


# Footnotes from https://github.com/kjhealy/5by5-figures/blob/master/shows.r


makeFootnote <- function(footnoteText = format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(.5)) {
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y= unit(2, "mm"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}

# Clubs output
format(file.info(path.clubs)$ctime, format = "%B %d, %Y")

# Naming for generated images
outputName <- function(plot.obj = clubs.country.inset) {
	xval <- as.character(plot.obj$mapping$x)
	short.date <- format(Sys.time(), format = "%b%d")
	range <- ifelse(is.numeric(plot.obj$data[, xval]), paste(range(plot.obj$data[, xval]), collapse = "-"), NULL)
	type <- paste(unlist(Filter(nchar, plot.obj$options$labels)), collapse = " by ")
	return(paste(type, range, short.date))
}






