# packages loading
#-------------------------------------------------------------------------------
require(data.table)
require(jsonlite)
require(stringr)
require(testthat)

# config loading
#-------------------------------------------------------------------------------
data_def <- fromJSON("conf/data-def.json")

# data importing
#-------------------------------------------------------------------------------
### gdelt
gdelt <- fread("data/gdelt.tsv",
               colClasses = rep("character",
                                length(MyVarNameXtract(data_def, "gdelt"))))
setnames(gdelt, names(gdelt), MyVarNameXtract(data_def, "gdelt"))
testthat::expect_identical(names(gdelt), MyVarNameXtract(data_def, "gdelt"))
MyDataTypeConvert(data_def, "gdelt", dec = ".", verbose = TRUE)
save(gdelt, file = "data/RData/gdelt.RData")
rm(gdelt)
gc()

### gkg
gkg <- fread("data/gkg.tsv",
             colClasses = rep("character",
                              length(MyVarNameXtract(data_def, "gkg"))))
testthat::expect_identical(names(gkg), MyVarNameXtract(data_def, "gkg"))
MyDataTypeConvert(data_def, "gkg", dec = ".", verbose = TRUE)
save(gkg, file = "data/RData/gkg.RData")
rm(gkg)
gc()

rm(data_def)
gc()
