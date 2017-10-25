myYears <- 2013:2017
# myYears <- 2017
myMonths <- c(paste0("0", 1:9), 10:12)
# myMonths <- "01"
myDays <- c(paste0("0", 1:9), 10:31)
# myDays <- paste0("0", 1:9)

# data dir
#-------------------------------------------------------------------------------
data_dir <- file.path("/vagrant", "share", "gdelt-project", "data")

gdelt_dir <- file.path(data_dir, "gdelt")
gkg_dir <- file.path(data_dir, "gkg")
gkgc_dir <- file.path(data_dir, "gkgc")

for (d in c(gdelt_dir, gkg_dir, gkgc_dir)) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
}

# log dir
#-------------------------------------------------------------------------------
log_dir <- file.path("/vagrant", "share", "gdelt-project", "log")
if (!dir.exists(log_dir)) {
  dir.create(log_dir, recursive = TRUE)
}

system2("touch", file.path(log_dir, paste0("wget_", c("gdelt", "gkg", "gkgc"), ".log")))
system2("touch", file.path(log_dir, paste0("wget_", c("gdelt", "gkg", "gkgc"), ".err")))

# data downloading
#-------------------------------------------------------------------------------
for (y in as.character(myYears)) {
  for (m in myMonths) {
    for (d in myDays) {
      url_date <- paste0(y, m, d)
      # url <- paste0("http://s3-us-east-2.amazonaws.com/texata-round2/gdelt/events/", url_date, ".export.CSV.gz")
      url_gdelt <- paste0("http://data.gdeltproject.org/events/", url_date, ".export.CSV.zip")
      system2(command = "wget", args = c(url_gdelt, paste0("-P ", gdelt_dir), "--quiet",
                                         paste0(">> ", file.path(log_dir, "wget_gdelt.log")),
                                         paste0("2>> ", file.path(log_dir, "wget_gdelt.err")), "&")) # UNIX multi-processing!
      url_gkg <- paste0("http://data.gdeltproject.org/gkg/", url_date, ".gkg.csv.zip")
      system2(command = "wget", args = c(url_gkg, paste0("-P ", gkg_dir), "--quiet",
                                         paste0(">> ", file.path(log_dir, "wget_gkg.log")),
                                         paste0("2>> ", file.path(log_dir, "wget_gkg.err")), "&")) # UNIX multi-processing!
      url_gkgc <- paste0("http://data.gdeltproject.org/gkg/", url_date, ".gkgcounts.csv.zip")
      system2(command = "wget", args = c(url_gkgc,  paste0("-P ", gkgc_dir), "--quiet",
                                         paste0(">> ", file.path(log_dir, "unzip_gkgc.log")),
                                         paste0("2>> ", file.path(log_dir, "unzip_gkgc.err")), "&")) # UNIX multi-processing!
    }
  }
}

# data unzipping
#-------------------------------------------------------------------------------
system2("touch", file.path(log_dir, paste0("unzip_", c("gdelt", "gkg", "gkgc"), ".log")))
system2("touch", file.path(log_dir, paste0("unzip_", c("gdelt", "gkg", "gkgc"), ".err")))
for (y in as.character(myYears)) {
  for (m in myMonths) {
    for (d in myDays) {
      url_date <- paste0(y, m, d)
      url_gdelt <- file.path(gdelt_dir, paste0(url_date, ".export.CSV.zip"))
      system2("unzip", c("-qq", url_gdelt, paste0("-d ", gdelt_dir),
                         paste0(">> ", file.path(log_dir, "unzip_gdelt.log")),
                         paste0("2>> ", file.path(log_dir, "unzip_gdelt.err")), "&")) # UNIX multi-processing!
      url_gkg <- file.path(gkg_dir, paste0(url_date, ".gkg.csv.zip"))
      system2("unzip", c("-qq", url_gkg, paste0("-d ", gkg_dir),
                         paste0(">> ", file.path(log_dir, "unzip_gkg.log")),
                         paste0("2>> ", file.path(log_dir, "unzip_gkg.err")), "&")) # UNIX multi-processing!
      url_gkgc <- file.path(gkgc_dir, paste0(url_date, ".gkgcounts.csv.zip"))
      system2("unzip", c("-qq", url_gkgc, paste0("-d ", gkgc_dir),
                         paste0(">> ", file.path(log_dir, "unzip_gkgc.log")),
                         paste0("2>> ", file.path(log_dir, "unzip_gkgc.err")), "&")) # UNIX multi-processing!
    }
  }
}

# data processing
#-------------------------------------------------------------------------------
system2("touch", file.path(log_dir, paste0(c("gdelt", "gkg", "gkgc"), ".tsv")))
system2("touch", file.path(log_dir, paste0("cat_", c("gdelt", "gkg", "gkgc"), ".err")))
for (y in as.character(myYears)) {
  for (m in myMonths) {
    for (d in myDays) {
      url_date <- paste0(y, m, d)
      url_gdelt <- file.path(gdelt_dir, paste0(url_date, ".export.CSV"))
      system2("cat", c(url_gdelt,
                         paste0(">> ", file.path(gdelt_dir, "gdelt.tsv")),
                         paste0("2>> ", file.path(log_dir, "cat_gdelt.err"))))
      url_gkg <- file.path(gkg_dir, paste0(url_date, ".gkg.csv"))
      system2("cat", c(url_gkg,
                         paste0(">> ", file.path(gkg_dir, "gkg.tsv")),
                         paste0("2>> ", file.path(log_dir, "cat_gkg.err"))))
      url_gkgc <- file.path(gkgc_dir, paste0(url_date, ".gkgcounts.csv"))
      system2("cat", c(url_gkgc,
                         paste0(">> ", file.path(gkgc_dir, "gkgcounts.tsv")),
                         paste0("2>> ", file.path(log_dir, "cat_gkgc.err"))))
    }
  }
}
