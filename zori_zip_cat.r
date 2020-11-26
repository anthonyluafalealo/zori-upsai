############################################################################################################################################
## This script aggregates the HUD's Urbanization Perceptions Small Area Index (UPSAI) up to the zip code level and uses the resulting     ##
## dataset to categorize zip codes in Zillow's Observed Rent Index (ZORI) as urban, suburban, and rural.                                  ##
##                                                                                                                                        ##
## Inputs:                                                                                                                                ##
## UPSAI (Downloaded 10/24/2020 from https://www.huduser.gov/portal/sites/default/files/zip/UPSAI_050820.zip)                             ##
## ZORI  (Downloaded 11/22/2020 from http://files.zillowstatic.com/research/public_v2/zori/Zip_ZORI_AllHomesPlusMultifamily_Smoothed.csv) ##
## Tract-Zip crosswalk file (Downloaded 10/25/2020 from https://www.huduser.gov/portal/datasets/usps/TRACT_ZIP_092020.xlsx                ##
## Chained CPI less food and energy (Series ID SUUR0000SA0L1E downloaded 11/22/2020 from https://data.bls.gov/cgi-bin/surveymost?su)      ##
##                                                                                                                                        ##
## Outputs:                                                                                                                               ##
## UPSAIZIP - The UPSAI aggregated up to the zip code level                                                                               ##
## ZORIUPSAI - The ZORI (by zip code) with zip codes categorized as urban, suburban, and rural based on UPSAI                             ##
##                                                                                                                                        ##
## Dependencies:                                                                                                                          ##
## This script requires the readxl library.                                                                                               ##
############################################################################################################################################

header.true <- function(df) {
	names(df) <- as.character(unlist(df[1,]))
	df[-1,]
}

library(readxl)

# Import data sets
# Use CPI for inflation adjustment because PCE doesn't have a value for October 2020 at this time
upsai <- read.csv("../Downloads/UPSAI_050820/UPSAI_050820.csv", colClasses=c(rep("character", 4), rep("numeric", 5)))
tractzip <- read_excel("../Downloads/TRACT_ZIP_092020.xlsx")
zori <- read.csv("../Downloads/Zip_ZORI_AllHomesPlusMultifamily_Smoothed(1).csv", colClasses=c("numeric", "character", "numeric", "character", rep("numeric", 82)))
cpi <- read.csv("../Documents/c-cpi-u.csv", colClasses=c(rep("character", 2), "numeric"))

# Transposing ZORI and cleaning up the resulting dataset a little
zorit <- subset(zori, select = -c(RegionID, SizeRank, MsaName))
zorit$RegionName <- paste("X", zorit$RegionName, sep="")
zorit <- t(zorit)
zorit <- as.data.frame(zorit)
zorit <- header.true(zorit)
zorit[, grepl("X", names(zorit))] <- sapply(zorit[, grepl("X", names(zorit))], as.numeric)
zorit$Month <- rownames(zorit)
rownames(zorit) <- NULL

# Applying CPI to get real values for ZORI
cpi$Month <- paste("X", cpi$Month, sep="")
zoritcpi <- merge(zorit, cpi, by="Month", all.x=TRUE, all.y=FALSE)

zoritcpi[, grepl("X", names(zoritcpi))] <- zoritcpi[, grepl("X", names(zoritcpi))] / zoritcpi[, colnames(zoritcpi)=="Value"] * 100

# Transposing ZORI matrix, now with real values, back to original configuration
zoritcpit <- t(subset(zoritcpi, select=-c(Series.Id, Value)))
zoritcpit <- as.data.frame(zoritcpit)
zoritcpit <- header.true(zoritcpit)
zoritcpit$RegionName <- rownames(zoritcpit)
rownames(zoritcpit) <- NULL
zoritcpit[, grepl("X", names(zoritcpit))] <- sapply(zoritcpit[, grepl("X", names(zoritcpit))], as.numeric)
zoritcpit$RegionName <- sub(".", "", zoritcpit$RegionName)

# Merge MSA names variable back into ZORI matrix
zorireal <- merge(subset(zori, select=c(RegionName, MsaName)), zoritcpit, by="RegionName")

# Next step is to aggregate the UPSAI, which is broken down by census tract, up to the zip code level
# Start by merging UPSAI dataset and Tract-Zip crosswalk file
upsaitractzip <- merge(upsai, tractzip, by.x="GEOID", by.y="TRACT", all.x=TRUE, all.y=FALSE)

# When aggregating up from the census tract level, we would first need to determine the number of households in each tract
# that would classify it as urban, suburban, and rural. UPSAI provides "raw" probabilities to help with that, along with
# an estimated occupied housing units variable from the 2017 ACS.

# To get the number of households that fall under each category (households that would classify their tract as urban, suburban, and rural),
# simply multiply the total number of households by the raw probabilities for each category.
upsaitractzip$UPSAI_urban_hh <- upsaitractzip$UPSAI_urban * upsaitractzip$ACS17_Occupied_Housing_Units_Est
upsaitractzip$UPSAI_suburban_hh <- upsaitractzip$UPSAI_suburban * upsaitractzip$ACS17_Occupied_Housing_Units_Est
upsaitractzip$UPSAI_rural_hh <- upsaitractzip$UPSAI_rural * upsaitractzip$ACS17_Occupied_Housing_Units_Est

# Now, we can the household-level responses up to the zip code level
upsaizip <- aggregate(cbind(ACS17_Occupied_Housing_Units_Est=upsaitractzip$ACS17_Occupied_Housing_Units_Est,
                            UPSAI_urban_hh=upsaitractzip$UPSAI_urban_hh, UPSAI_suburban_hh=upsaitractzip$UPSAI_suburban_hh,
							UPSAI_rural_hh=upsaitractzip$UPSAI_rural_hh),
					   by=list(ZIP=upsaitractzip$ZIP),
					   FUN=sum)

# At the zip code level, determine the proportion of households that would classify the zip code as urban, suburban, and rural
upsaizip$UPSAI_urban <- upsaizip$UPSAI_urban_hh / upsaizip$ACS17_Occupied_Housing_Units_Est
upsaizip$UPSAI_suburban <- upsaizip$UPSAI_suburban_hh / upsaizip$ACS17_Occupied_Housing_Units_Est
upsaizip$UPSAI_rural <- upsaizip$UPSAI_rural_hh / upsaizip$ACS17_Occupied_Housing_Units_Est

# Zip codes are then classified as urban, suburban, and rural based on which received the highest proportion
upsaizip$UPSAI_max <- apply(upsaizip[, c("UPSAI_urban", "UPSAI_suburban", "UPSAI_rural")], 1, max)
upsaizip$UPSAI_Cat <- ifelse(upsaizip$UPSAI_urban==upsaizip$UPSAI_max, 1, ifelse(upsaizip$UPSAI_suburban==upsaizip$UPSAI_max, 2, ifelse(upsaizip$UPSAI_rural==upsaizip$UPSAI_max, 3, 0)))

# Using the below aggregate function, we get the following results from the newly created UPSAI_Cat variable:
# - 13.1% of households would classify their zip code as urban
# - 48.8% would say theirs is suburban
# - 38.0% would say rural
# Compared to the Pew survey cited by the UPSAI paper, that study's results were: 25% urban, 43% suburban, and 30% rural.
# Assuming that the UPSAI_Cat variable is biased, it would then seem to be biased for the rural category. Since that is
# not the majority class, controlling the totals as the UPSAI authors did at the tract level is not necessary at the zip code level.
aggregate(x=upsaizip$ACS17_Occupied_Housing_Units_Est, by=list(upsaizip$UPSAI_Cat), FUN=sum)

# Finally, categorize the ZORI zip codes using the newly aggregated UPSAI by merging the datasets
zoriupsai <- merge(zorireal, upsaizip, by.x="RegionName", by.y="ZIP", all.x=TRUE, all.y=FALSE)



# Other summary stats
table(zoriupsai[which(zoriupsai[,93]==3),2])
table(zoriupsai[which(zoriupsai[,93]==1),2])
table(zoriupsai[which(zoriupsai[,93]==2),2])
table(zoriupsai$UPSAI_Cat)


