myMonths <- c(paste0("0", 1:9), 10:12)
myDays <- c(paste0("0", 1:9), 10:31)

setwd("data/gdelt")
# gdelt
for (y in as.character(2014:2017)) {
  for (m in myMonths) {
    for (d in myDays) {
      url_date <- paste0(y, m, d)
      url <- paste0("http://s3-us-east-2.amazonaws.com/texata-round2/gdelt/events/", url_date, ".export.CSV.gz")
      system2("wget", c(url, "&")) # UNIX multi-processing!
    }
  }
}

for (y in as.character(2014:2017)) {
  for (m in myMonths) {
    for (d in myDays) {
      url_date <- paste0(y, m, d)
      url <- paste0(url_date, ".export.CSV.gz")
      system2("gzip", c("-d", url, "&")) # UNIX multi-processing!
    }
  }
}

system2("touch", c("gdelt.tsv"))
for (y in as.character(2014:2017)) {
  for (m in myMonths) {
    for (d in myDays) {
      url_date <- paste0(y, m, d)
      url <- paste0(url_date, ".export.CSV")
      system2("cat", c(url, ">> gdelt.tsv")) # UNIX multi-processing!
    }
  }
}

# gkg
# Did not manage to guess path for downloading!

setwd("/home/rstudio/round-2")
