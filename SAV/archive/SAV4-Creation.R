#Starting over converting all percent cover metrics to BB
library(tidyverse)
library(data.table)
library(scales)
library(brms)
library(broom.mixed)
library(tidybayes)
library(bayesplot)
library(sf)
library(gtable)
library(grid)
library(gridExtra)
library(tictoc)
library(nlme)
library(colorspace)
library(here)
library(patchwork)
#library(future)
library(extrafont)
library(magick)
# library(ggspatial)
# font_import()
# loadfonts()

#Load and wrangle data------------------------------------------------------------
data_dir <- here::here("data")

file_in <- list.files(data_dir, pattern="All_SAV", full=TRUE)
SAV <- fread(file_in, sep = "|", header = TRUE, stringsAsFactors = FALSE,
             na.strings=c("NULL","","NA"))

SAV <- SAV[!is.na(ResultValue), ]

# Create data columns based on old parameter results to make script run
SAV$BB <- NA
SAV$mBB <- NA
SAV$PC <- NA
SAV$PO <- NA
SAV$SC <- NA
SAV$PA <- NA

# Fill created columns with values based on parameter names
SAV$BB[SAV$ParameterName=="Braun Blanquet Score"] <-
  SAV$ResultValue[SAV$ParameterName=="Braun Blanquet Score"]

SAV$mBB[SAV$ParameterName=="Modified Braun Blanquet Score"] <-
  SAV$ResultValue[SAV$ParameterName=="Modified Braun Blanquet Score"]

SAV$PC[SAV$ParameterName=="Percent Cover"] <-
  SAV$ResultValue[SAV$ParameterName=="Percent Cover"]

SAV$PO[SAV$ParameterName=="Percent Occurrence"] <-
  SAV$ResultValue[SAV$ParameterName=="Percent Occurrence"]

SAV$SC[SAV$ParameterName=="Shoot Count"] <-
  SAV$ResultValue[SAV$ParameterName=="Shoot Count"]

SAV$PA[SAV$ParameterName=="Presence/Absence"] <-
  SAV$ResultValue[SAV$ParameterName=="Presence/Absence"]

#Rename "Total_SAV" to "Total SAV"
SAV$CommonIdentifier[SAV$CommonIdentifier=="Total_SAV"] <- "Total SAV"

# Create a list of n years available for each managed area
SAV_sum <- SAV %>% group_by(ManagedAreaName) %>% summarize(n_yr = length(unique(Year)), yrs = list(sort(unique(Year))))

# Filtering and subsetting
SAV2 <- subset(SAV, !is.na(SAV$BB) | !is.na(SAV$mBB) | !is.na(SAV$PC) | !is.na(SAV$PO))
SAV2 <- SAV2 %>% filter(BB >= 0 & BB <= 5 | is.na(BB))
SAV2 <- SAV2 %>% filter(mBB >= 0 & mBB <= 5 | is.na(mBB))
SAV2 <- SAV2 %>% filter(PC >= 0 & PC <= 100 | is.na(PC))
SAV2 <- SAV2 %>% filter(PO >= 0 & PO <= 100 | is.na(PO))
SAV2 <- SAV2 %>% filter(Month %in% c(4:10))
setDT(SAV2)

SAV2[!is.na(BB), BB_all := fcase(BB == 0, 0, 
                                 BB > 0 & BB <= 1, 1,
                                 BB > 1, round(BB))]
SAV2[!is.na(mBB), BB_all := fcase(mBB == 0, 0,
                                  mBB > 0 & mBB <= 1, 1, 
                                  mBB > 1, round(mBB))]
SAV2[!is.na(PC), BB_all := fcase(PC == 0, 0,
                                 PC > 0 & PC <= (2.5 + (15-2.5)/2), 1,
                                 PC <= (2.5 + (15-2.5) + (37.5-15)/2), 2,
                                 PC <= (2.5 + (15-2.5) + (37.5-15) + (62.5 - 37.5)/2), 3,
                                 PC <= (2.5 + (15-2.5) + (37.5-15) + (62.5 - 37.5) + (87.5 - 62.5)/2), 4, 
                                 PC > (2.5 + (15-2.5) + (37.5-15) + (62.5 - 37.5) + (87.5 - 62.5)/2), 5)]


#Replaces two blocks of code above by using the BB_all variable to create all estimates at once.
SAV2[!is.na(BB_all), BB_pct := fcase(BB_all == 0, 0, 
                                     BB_all > 0 & BB_all <= 0.1, rescale(BB_all, from=c(0, 0.1), to=c(0,0.02)), #Added by SRD 8/31/2021
                                     BB_all > 0.1 & BB_all <= 0.5, rescale(BB_all, from=c(0.1, 0.5), to=c(0.02,0.1)),
                                     BB_all > 0.5 & BB_all <= 1, rescale(BB_all, from=c(0.5,1), to=c(0.1,2.5)),
                                     BB_all > 1 & BB_all <= 2, rescale(BB_all, from=c(1,2), to=c(2.5,15)),
                                     BB_all > 2 & BB_all <= 3, rescale(BB_all, from=c(2,3), to=c(15,37.5)),
                                     BB_all > 3 & BB_all <= 4, rescale(BB_all, from=c(3,4), to=c(37.5,62.5)),
                                     BB_all > 4 & BB_all <= 5, rescale(BB_all, from=c(4,5), to=c(62.5,87.5)))]

SAV2[, BB_pct := as.numeric(BB_pct)]
SAV2[, BB_all := as.ordered(BB_all)]
SAV2[!is.na(PO), method := "Percent Occurrence"]
SAV2[!is.na(BB), method := "Braun Blanquet"]
SAV2[!is.na(mBB), method := "Modified Braun Blanquet"]
SAV2[!is.na(PC), method := "Percent Cover"]

SAV2[!is.na(BB_all), PA := ifelse(BB_all == 0, 0, 1)]
SAV2[!is.na(PO), PA := ifelse(PO == 0, 0, 1)]

SAV2[, relyear := Year - min(Year)]

SAV3 <- SAV2 %>% filter(SpeciesGroup1 == "Seagrass" | SpeciesGroup1 == "Macroalgae")

#Temporary fix to programs 570 and 571 - Group 1 should be "Total seagrass" instead of "Total SAV"
SAV3[ProgramID %in% c(570, 571) & CommonIdentifier == "Total SAV", CommonIdentifier := "Total seagrass"]

#Temporary fix to cases where analysisunit is NA but CommonIdentifier is "Drift Algae" (and Drift_Attached is also NA); ~6,000 records
SAV3[CommonIdentifier == "Drift algae", Drift_Attached := "Drift"]

species_reject <- c("All", "NA",
                    "Vallisneria americana", "Najas guadalupensis",
                    "Hydrilla verticillata", "Potamogeton pusillus",
                    "Zannichellia palustris")
SAV3[, `:=` (analysisunit_halid = ifelse(CommonIdentifier %in% species_reject, NA, 
                                         ifelse(str_detect(CommonIdentifier, "Halophila") & is.na(SpeciesName), "Unidentified Halophila", 
                                                ifelse(SpeciesGroup1 == "Seagrass", CommonIdentifier, Drift_Attached))),
             analysisunit = ifelse(CommonIdentifier %in% species_reject, NA, 
                                   ifelse(str_detect(CommonIdentifier, "Halophila"), "Halophila spp.", 
                                          ifelse(SpeciesGroup1 == "Seagrass", CommonIdentifier, Drift_Attached))))]
SAV3[!is.na(Drift_Attached), `:=` (analysisunit_halid = paste0(analysisunit_halid, " algae"),
                                   analysisunit = paste0(analysisunit, " algae"))]

SAV4 <- subset(SAV3, !is.na(SAV3$analysisunit))

saveRDS(SAV4, here::here("output/SAV_DataUsed.rds"))
fwrite(SAV4, "output/SAV_DataUsed.txt", sep = "|")

## SAV4 has been written to SAV_DataUsed.txt which is imported in NewScript.r for analyses