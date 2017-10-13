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
gdelt <- fread("data/qualif_bdd_marques_v2.csv",
             header = TRUE, sep = ";",
             colClasses = rep("character",
                              length(MyVarNameXtract(data_def, "gdelt"))))
testthat::expect_identical(names(gdelt), MyVarNameXtract(data_def, "gdelt"))
MyDataTypeConvert(data_def, "gdelt", dec = ",", verbose = TRUE)
save(gdelt, file = "data/RData/gdelt.RData")
rm(gdelt)
gc()

rm(data_def)
gc()
