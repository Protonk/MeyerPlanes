rm(list=ls())
# ---------- create_Brockett_linecharts.R ----------
# Tomonori Ishikawa
# for Peter Meyer
#
# 2011-05-27, v1: basic line charts of Brockett counts
#   of publication citations by year and country of origin


# --- strings ---


# --- directories and filenames ---
home_dir <- "C:\\Documents and Settings\\HP Owner\\My Documents\\My Dropbox\\Shared_with_Ish\\Line Charts of Brockett for Peter"
#home_dir <- "C:\\Documents and Settings\\Tomonori Ishikawa\\My Documents\\Downloads"

#proj_dir   <- paste(home_dir, 'SCAAP 2011 Report Card', sep="\\")
#data_dir   <- paste(proj_dir, 'Raw Data', sep="\\")
#latex_dir  <- paste(proj_dir, 'LaTeX', sep="\\")
#charts_dir <- paste(latex_dir, 'charts', sep="\\")

proj_dir <- home_dir
data_dir <- home_dir


# --- set the working directory ---
setwd(proj_dir)


# ----- load functions -----


# ----- load Brockett dataset -----
#column 1 is a year (there are a bunch of zeros, where the year wasn't known)
#col 2 is an abbreviation for the country: br Britain, de Germany, fr France, us US
#col 3 is 1 if the patent is in Britain
#col 4 is 1 if the patent is in Germany
#col 5 is 1 if the patent is in France
#col 6 is 1 if the patent is in US

# --- define languages in order listed in file ---
langs.str <- c('Britain','Germany','France','US')

# --- load data ---
datafile.str <- paste(data_dir, 'patents_small.csv', sep='\\')
pubdata.df <- read.csv(datafile.str, header=F)

# --- define column names ---
colnames(pubdata.df) <- c('yr','country',langs.str)

# --- redefine unknown years from 0 to NA ---
# avoid using year 0 since 0 is a possible (?) value for year in charts.
# use year NA instead. NA is unambiguously not a valid year.
pubdata.df$year <- ifelse(pubdata.df$yr==0, NA, pubdata.df$yr)
#pubdata.df[1:10,]


# ----- function: count publications by language by year -----
count_bylang <- function (lang) {
  onelang_counts.str <- paste(lang,'_counts',sep='')
  # --- sum the individual language dummy variables ---
  counts <- tapply(pubdata.df[,lang], pubdata.df$year, sum)
  # --- find beginning and ending years ---
  beg_year <- min(as.integer(names(counts)))
  end_year <- max(as.integer(names(counts)))
  # --- fill-in missing years with 0 counts ---
  temp_counts <- rep(0, end_year-beg_year+1)
  names(temp_counts) <- as.character(beg_year:end_year)
  temp_counts[names(counts)] <- counts
  # --- pass variables to the global environment ---
  assign('beg_year', beg_year, envir=.GlobalEnv)
  assign('end_year', end_year, envir=.GlobalEnv)
  assign(onelang_counts.str, temp_counts, envir=.GlobalEnv)
}

count_bylang(lang='Britain')
count_bylang(lang='Germany')
count_bylang(lang='France')
count_bylang(lang='US')
beg_plot <- 1850 ##beg_year
end_plot <- 1910 ##end_year
#ls()

# --- collate languages to plot into a dataframe ---
pubs.df <- cbind(beg_year:end_year,
  Britain_counts,
  Germany_counts,
  France_counts,
  US_counts)

colnames(pubs.df) <- c('year',langs.str[1:4])
pubs.df[1:10,]


# --- plot it ---
beg_plot <- 1850  # comment out to start plot at earliest year
end_plot <- 1909  # comment out to end plot at latest year

plot_years <- as.character(beg_plot:end_plot)
title.str <- paste("Aeronautically-relevant patents by country\n", beg_plot, '-', end_plot, sep='')

##png(file='./patents-by-lang.png')
matplot(x=pubs.df[plot_years,'year'], y=pubs.df[plot_years,langs.str[1:4]],
  type='l',
  xlab='Year', ylab='Count of Publications',
  col=c('blue','red','black','green'), lty=c('dotdash' , 'dashed','dotted', 'solid'), lwd=2)
title(main=title.str)
legend(x='topleft', legend=langs.str[1:4],
  col=c('blue','red','black','green'), lty=c('dotdash' , 'dashed','dotted', 'solid'), lwd=2)
##dev.off()




# --- alternate method ---
# this is quick to make the table, but
# it's a bitch to fill-in missing years with zeros
# for charting purposes
#table(pubdata.df$lang_num, pubdata.df$year)
