# packages loading
#-------------------------------------------------------------------------------
require(data.table)
require(pryr)

# data loading
#-------------------------------------------------------------------------------
# copy approach
load("data/RData/gkg.RData")
object_size(gkg)

gkg[, COUNTS_BIS := MyVectorizedSplit(COUNTS, ";")]
gkg[, THEMES_BIS := MyVectorizedSplit(THEMES, ";")]
gkg[, LOCATIONS_BIS := MyVectorizedSplit(LOCATIONS, ";")]
gkg[, PERSONS_BIS := MyVectorizedSplit(PERSONS, ";")]
gkg[, ORGANIZATIONS_BIS := MyVectorizedSplit(ORGANIZATIONS, ";")]

gkg[, TONE_BIS := MyVectorizedSplit(TONE, ",")]
gkg[, TONE_Tone := MyVectorizedTONEXtract(TONE_BIS, 1)]
gkg[, TONE_PositiveScore := MyVectorizedTONEXtract(TONE_BIS, 2)]
gkg[, TONE_NegativeScore := MyVectorizedTONEXtract(TONE_BIS, 3)]
gkg[, TONE_Polarity := MyVectorizedTONEXtract(TONE_BIS, 4)]
gkg[, TONE_ActivityReferenceDensity := MyVectorizedTONEXtract(TONE_BIS, 5)]
gkg[, TONE_SelfGroupReferenceDensity:= MyVectorizedTONEXtract(TONE_BIS, 6)]

gkg[, CAMEOEVENTIDS_BIS := MyVectorizedSplit(CAMEOEVENTIDS, ",")]
gkg[, SOURCES_BIS := MyVectorizedSplit(SOURCES, ";")]
gkg[, SOURCEURLS_BIS := MyVectorizedSplit(SOURCEURLS, "<UDIV>")]

object_size(gkg)
save(gkg, file = "data/RData/gkg_cp.RData")

# copy-free approach
load("data/RData/gkg.RData")
object_size(gkg)

gkg[, COUNTS := MyVectorizedSplit(COUNTS, ";")]
gkg[, THEMES := MyVectorizedSplit(THEMES, ";")]
gkg[, LOCATIONS := MyVectorizedSplit(LOCATIONS, ";")]
gkg[, PERSONS := MyVectorizedSplit(PERSONS, ";")]
gkg[, ORGANIZATIONS := MyVectorizedSplit(ORGANIZATIONS, ";")]

gkg[, TONE := MyVectorizedSplit(TONE, ",")]
gkg[, TONE_Tone := MyVectorizedTONEXtract(TONE, 1)]
gkg[, TONE_PositiveScore := MyVectorizedTONEXtract(TONE, 2)]
gkg[, TONE_NegativeScore := MyVectorizedTONEXtract(TONE, 3)]
gkg[, TONE_Polarity := MyVectorizedTONEXtract(TONE, 4)]
gkg[, TONE_ActivityReferenceDensity := MyVectorizedTONEXtract(TONE, 5)]
gkg[, TONE_SelfGroupReferenceDensity:= MyVectorizedTONEXtract(TONE, 6)]

gkg[, CAMEOEVENTIDS := MyVectorizedSplit(CAMEOEVENTIDS, ",")]
gkg[, SOURCES := MyVectorizedSplit(SOURCES, ";")]
gkg[, SOURCEURLS := MyVectorizedSplit(SOURCEURLS, "<UDIV>")]

object_size(gkg)
save(gkg, file = "data/RData/gkg_cf.RData")
