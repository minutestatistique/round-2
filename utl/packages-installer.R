# packages definition
#-------------------------------------------------------------------------------
p <- c("data.table", "ggplot2", "anytime", "testthat", "lubridate", "stringr",
       "jsonlite", "pryr")

# packages installation
#-------------------------------------------------------------------------------
install.packages(p, Ncpus = 32)

rm(p)
gc()