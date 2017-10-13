# packages definition
#-------------------------------------------------------------------------------
p <- c("data.table", "ggplot2", "anytime", "testthat", "lubridate", "stringr",
       "jsonlite")

# packages installation
#-------------------------------------------------------------------------------
install.packages(p, Ncpus = 1)

rm(p)
gc()