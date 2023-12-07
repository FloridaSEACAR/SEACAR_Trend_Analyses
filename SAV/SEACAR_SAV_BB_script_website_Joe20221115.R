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
data_dir <- here::here("SAV/data")

file_in <- list.files(data_dir, pattern="Braun_Blanquet", full=TRUE)
file_in <- file_in[-grep("Modified", file_in)]
BB <- fread(file_in, sep = "|", header = TRUE, stringsAsFactors = FALSE,
            na.strings = "")
BB[, uid := paste0(LocationID, "_", QuadIdentifier, "_", SampleDate, "_", str_replace_all(CommonIdentifier, " ", ""))]
BB$QuadIdentifier <- as.character(BB$QuadIdentifier)
BB$BB <- BB$ResultValue
BB[, `:=` (ParameterName = NULL, ResultValue = NULL)]
BB <- BB[SpeciesGroup1 == "Seagrass" | SpeciesGroup1 == "Macroalgae", ]
BB_att <- BB[!is.na(Drift_Attached), unique(uid)]
BB_noatt <- BB[is.na(Drift_Attached), unique(uid)]
BB_dups <- BB[is.na(Drift_Attached) & uid %in% intersect(BB_noatt, BB_att), RowID]
BB <- BB[RowID %in% setdiff(RowID, BB_dups), ]

file_in <- list.files(data_dir, pattern="Modified_Braun_Blanquet", full=TRUE)
mBB <- fread(file_in, sep = "|", header = TRUE, stringsAsFactors = FALSE,
             na.strings = "")
mBB[, uid := paste0(LocationID, "_", QuadIdentifier, "_", SampleDate, "_", str_replace_all(CommonIdentifier, " ", ""))]
mBB$QuadIdentifier <- as.character(mBB$QuadIdentifier)
mBB$mBB <- mBB$ResultValue
mBB[, `:=` (ParameterName = NULL, ResultValue = NULL)]
mBB <- mBB[SpeciesGroup1 == "Seagrass" | SpeciesGroup1 == "Macroalgae", ]
mBB_att <- mBB[!is.na(Drift_Attached), unique(uid)]
mBB_noatt <- mBB[is.na(Drift_Attached), unique(uid)]
mBB_dups <- mBB[is.na(Drift_Attached) & uid %in% intersect(mBB_noatt, mBB_att), RowID]
mBB <- mBB[RowID %in% setdiff(RowID, mBB_dups), ]

file_in <- list.files(data_dir, pattern="Percent_Cover", full=TRUE)
PC <- fread(file_in, sep = "|", header = TRUE, stringsAsFactors = FALSE,
            na.strings = "")
PC[, uid := paste0(LocationID, "_", QuadIdentifier, "_", SampleDate, "_", str_replace_all(CommonIdentifier, " ", ""))]
PC$QuadIdentifier <- as.character(PC$QuadIdentifier)
PC$PC <- PC$ResultValue
PC[, `:=` (ParameterName = NULL, ResultValue = NULL)]
PC <- PC[SpeciesGroup1 == "Seagrass" | SpeciesGroup1 == "Macroalgae", ]
PC_att <- PC[!is.na(Drift_Attached), unique(uid)]
PC_noatt <- PC[is.na(Drift_Attached), unique(uid)]
PC_dups <- PC[is.na(Drift_Attached) & uid %in% intersect(PC_noatt, PC_att), RowID]
PC <- PC[RowID %in% setdiff(RowID, PC_dups), ]

file_in <- list.files(data_dir, pattern="Percent_Occurrence", full=TRUE)
PO <- fread(file_in, sep = "|", header = TRUE, stringsAsFactors = FALSE,
            na.strings = "")
PO[, uid := paste0(LocationID, "_", QuadIdentifier, "_", SampleDate, "_", str_replace_all(CommonIdentifier, " ", ""))]
PO$QuadIdentifier <- as.character(PO$QuadIdentifier)
PO$PO <- PO$ResultValue
PO[, `:=` (ParameterName = NULL, ResultValue = NULL)]
PO <- PO[SpeciesGroup1 == "Seagrass" | SpeciesGroup1 == "Macroalgae", ]
PO_att <- PO[!is.na(Drift_Attached), unique(uid)]
PO_noatt <- PO[is.na(Drift_Attached), unique(uid)]
PO_dups <- PO[is.na(Drift_Attached) & uid %in% intersect(PO_noatt, PO_att), RowID]
PO <- PO[RowID %in% setdiff(RowID, PO_dups), ]

file_in <- list.files(data_dir, pattern="Shoot_Count", full=TRUE)
SC <- fread(file_in, sep = "|", header = TRUE, stringsAsFactors = FALSE,
            na.strings = "")
SC[, uid := paste0(LocationID, "_", QuadIdentifier, "_", SampleDate, "_", str_replace_all(CommonIdentifier, " ", ""))]
SC$QuadIdentifier <- as.character(SC$QuadIdentifier)
SC$SC <- SC$ResultValue
SC[, `:=` (ParameterName = NULL, ResultValue = NULL)]
SC <- SC[SpeciesGroup1 == "Seagrass" | SpeciesGroup1 == "Macroalgae", ]
SC_att <- SC[!is.na(Drift_Attached), unique(uid)]
SC_noatt <- SC[is.na(Drift_Attached), unique(uid)]
SC_dups <- SC[is.na(Drift_Attached) & uid %in% intersect(SC_noatt, SC_att), RowID]
SC <- SC[RowID %in% setdiff(RowID, SC_dups), ]

file_in <- list.files(data_dir, pattern="Presence_Absence", full=TRUE)
PA <- fread(file_in, sep = "|", header = TRUE, stringsAsFactors = FALSE,
            na.strings = "")
PA[, uid := paste0(LocationID, "_", QuadIdentifier, "_", SampleDate, "_", str_replace_all(CommonIdentifier, " ", ""))]
PA$QuadIdentifier <- as.character(PA$QuadIdentifier)
PA$PA <- PA$ResultValue
PA[, `:=` (ParameterName = NULL, ResultValue = NULL)]
PA <- PA[SpeciesGroup1 == "Seagrass" | SpeciesGroup1 == "Macroalgae", ]
PA_att <- PA[!is.na(Drift_Attached), unique(uid)]
PA_noatt <- PA[is.na(Drift_Attached), unique(uid)]
PA_dups <- PA[is.na(Drift_Attached) & uid %in% intersect(PA_noatt, PA_att), RowID]
PA <- PA[RowID %in% setdiff(RowID, PA_dups), ]

commcols <- Reduce(intersect, list(names(BB), names(mBB), names(PC), names(PO), names(SC), names(PA)))
commcols2 <- quote(list(ProgramID, ProgramName, LocationID, ProgramLocationID, SampleDate, Year, Month, ManagedAreaName, Region, 
                   CommonIdentifier, SpeciesName, GenusName, SpeciesGroup1, Drift_Attached, SamplingMethod1, SamplingMethod2, ReportingLevel, 
                   QuadSize_m2, Grid, Depth_M, QuadIdentifier, SiteIdentifier, MADup, DataFileName, AreaID, uid))

SAV <- bind_rows(BB[, eval(commcols2)], 
                 mBB[, eval(commcols2)], 
                 PC[, eval(commcols2)], 
                 PO[, eval(commcols2)], 
                 SC[, eval(commcols2)], 
                 PA[, eval(commcols2)])
SAV <- distinct(SAV)

SAV <- merge(SAV, distinct(BB[, .(uid, BB)]), by = "uid", all = TRUE)
SAV <- merge(SAV, PC, by = "uid", all = TRUE)
SAV <- merge(SAV, PO, by = "uid", all = TRUE)
SAV <- merge(SAV, SC, by = "uid", all = TRUE)
SAV <- merge(SAV, PA, by = "uid", all = TRUE)

SAV <- bind_rows(BB, mBB, PC, PO, SC, PA)
rm(BB)
rm(mBB)
rm(PC)
rm(PO)
rm(SC)
rm(PA)

# SAV <- fread(here::here("SAV/data/Combined_SAV_column_All-2021-Sep-20.csv"))
# 
# setnames(SAV, c("[BraunBlanquetScore]", "[ModifiedBraunBlanquetScore]", "[PercentCover_%]", "[PercentOccurrence]"), c("BB", "mBB", "PC", "PO"))

SAV[, `:=` (BB = as.numeric(BB),
            mBB = as.numeric(mBB),
            PC = as.numeric(PC),
            PO = as.numeric(PO))]
# SAV[ SAV == "NA" ] <- NA
SAV$CommonIdentifier[SAV$CommonIdentifier=="Total_SAV"] <- "Total SAV"
SAV_sum <- SAV %>% group_by(ManagedAreaName) %>% summarize(n_yr = length(unique(Year)), yrs = list(sort(unique(Year))))

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

#Create a collapsed version of the long-form combined-table-style data.table.
# SAV2b <- SAV2[, c(2:34)] %>%
#   dplyr::group_by(ProgramID, LocationID, ProgramName, ProgramLocationID, GISUniqueID, SampleDate, Year, Month, 
#                   ManagedAreaName, Region, CommonIdentifier, SpeciesName, GenusName, SpeciesGroup1, SpeciesGroup2,
#                   Drift_Attached, SamplingMethod1, SamplingMethod2,ReportingLevel, QuadSize_m2, Grid, Depth_M,
#                   MADup, DataFileName, QuadIdentifier, SiteIdentifier) %>%
#   tidyr::fill(BB, mBB, PC, Hectares_ha, `[ShootCount_#]`, `[Presence/Absence_Y/N]`, PO) %>%
#   tidyr::fill(BB, mBB, PC, Hectares_ha, `[ShootCount_#]`, `[Presence/Absence_Y/N]`, PO, .direction = 'up') %>%
#   dplyr::distinct()
# SAV2b <- subset(SAV2b, !is.na(SAV2b$BB) | !is.na(SAV2b$mBB) | !is.na(SAV2b$PC) | !is.na(SAV2b$Hectares_ha) | !is.na(SAV2b$`[ShootCount_#]`) | !is.na(SAV2b$`[Presence/Absence_Y/N]`) | !is.na(SAV2b$PO))


#Create a column for BB_converted to median percent cover. ***I think this can probably be condensed to just use the BB_all variable created earlier.
# SAV2[!is.na(BB) & is.na(PC), BB_pct := fcase(BB == 0, 0, 
#                                              BB > 0 & BB <= 0.1, rescale(BB, from=c(0, 0.1), to=c(0,0.02)), #Added by SRD 8/31/2021
#                                              BB > 0.1 & BB <= 0.5, rescale(BB, from=c(0.1, 0.5), to=c(0.02,0.1)),
#                                              BB > 0.5 & BB <= 1, rescale(BB, from=c(0.5,1), to=c(0.1,2.5)),
#                                              BB > 1 & BB <= 2, rescale(BB, from=c(1,2), to=c(2.5,15)),
#                                              BB > 2 & BB <= 3, rescale(BB, from=c(2,3), to=c(15,37.5)),
#                                              BB > 3 & BB <= 4, rescale(BB, from=c(3,4), to=c(37.5,62.5)),
#                                              BB > 4 & BB <= 5, rescale(BB, from=c(4,5), to=c(62.5,87.5)))]  #Modified by SRD 8/31/2021
# 
# SAV2[!is.na(mBB) & is.na(PC), BB_pct := fcase(mBB == 0, 0, 
#                                               mBB > 0 & mBB <= 0.1, rescale(mBB, from=c(0, 0.1), to=c(0,0.02)), #Added by SRD 8/31/2021
#                                               mBB > 0.1 & mBB <= 0.5, rescale(mBB, from=c(0.1, 0.5), to=c(0.02,0.1)),
#                                               mBB > 0.5 & mBB <= 1, rescale(mBB, from=c(0.5,1), to=c(0.1,2.5)),
#                                               mBB > 1 & mBB <= 2, rescale(mBB, from=c(1,2), to=c(2.5,15)),
#                                               mBB > 2 & mBB <= 3, rescale(mBB, from=c(2,3), to=c(15,37.5)),
#                                               mBB > 3 & mBB <= 4, rescale(mBB, from=c(3,4), to=c(37.5,62.5)),
#                                               mBB > 4 & mBB <= 5, rescale(mBB, from=c(4,5), to=c(62.5,87.5)))]  #Modified by SRD 8/31/2021

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

saveRDS(SAV4, here::here("SAV/output/data/SAV4.rds"))
fwrite(SAV4, "SAV/output/data/SAV_Used.txt", sep = "|")

SAV4_sum <- SAV4 %>% group_by(method, ManagedAreaName) %>% summarize(n_yr = length(unique(Year)), yrs = list(sort(unique(Year))))

#Create a data.table of Percent Occurrence data for binomial regression ****Not needed if we use binary family instead of bernoulli.
#Warning: this is a large table with >23 million rows, so only recreate it if necessary
# POdat <- SAV4[!is.na(PO) & ProgramID != 10001, c("ProgramID", "LocationID", "Year", "Month", 
#                                                  "ManagedAreaName", "CommonIdentifier", "SpeciesGroup1", "SamplingMethod1", 
#                                                  "SamplingMethod2", "ReportingLevel", "QuadSize_m2", "Grid", "Depth_M",
#                                                  "DataFileName", "PO", "QuadIdentifier", "SiteIdentifier", "method",
#                                                  "relyear", "analysisunit")]
# 
# POdat[, Grid := as.numeric(Grid)]
# POdat[, CoverObs := as.integer()]
# 
# POdat[, `:=` (LocMaYrMoQiCi = paste0(LocationID, gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', ManagedAreaName, perl = TRUE), Year, Month, QuadIdentifier, gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', CommonIdentifier, perl = TRUE)),
#               Ind250 = 0, 
#               Ind = 0)]
# 
# nsamps <- POdat %>% group_by(ManagedAreaName, analysisunit) %>% summarize(nQuads = length(unique(LocMaYrMoQiCi)), nObs = length(CoverObs))
# setDT(nsamps)
# 
# POdat <- merge(POdat, nsamps, by = c("ManagedAreaName", "analysisunit"))
# 
# POdat[, Ind250 := fcase(
#   nQuads <= 300, 1L,
#   nQuads <= 500, sample(c(1:2), 1),
#   default = sample(c(1:ceiling(nQuads/250)), 1)
# ), by = 1:nrow(POdat)]
# 
# POdat_1 <- POdat[rep(seq(1, nrow(POdat)), (Grid * (round(PO, digits = 0)/100)))][, CoverObs := 1]
# POdat_0 <- POdat[rep(seq(1, nrow(POdat)), (100 - (Grid * (round(PO, digits = 0)/100))))][, CoverObs := 0]
# 
# POdat <- rbind(POdat_1, POdat_0)
# 
# rm(POdat_0, POdat_1, nsamps)
# saveRDS(POdat, here::here("POdat.rds"))



#Functions ---------------------------------------------------------------------
# addfits <- function(models, plot_i, param){
#   for(z in 1:length(models)){
#     plot_i <- plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[z]])$data$analysisunit)],
#                                  aes(x = relyear, y = predict(eval(models[[z]]), level = 0), color = unique(eval(models[[z]])$data$analysisunit)), lwd = 1.2)
#   }
#   return(plot_i)
# }

# addfits <- function(models, plot_i, param){
  ifelse(length(models) == 1,
         return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[1]])$data$analysisunit)],
                                   aes(x = relyear, y = predict(eval(models[[1]]), level = 0), color = unique(eval(models[[1]])$data$analysisunit)), lwd = 1.2)),
         ifelse(length(models) == 2,
                return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[1]])$data$analysisunit)],
                                          aes(x = relyear, y = predict(eval(models[[1]]), level = 0), color = unique(eval(models[[1]])$data$analysisunit)), lwd = 1.2) +
                         geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[2]])$data$analysisunit)],
                                   aes(x = relyear, y = predict(eval(models[[2]]), level = 0), color = unique(eval(models[[2]])$data$analysisunit)), lwd = 1.2)),
                ifelse(length(models) == 3,
                       return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[1]])$data$analysisunit)],
                                                 aes(x = relyear, y = predict(eval(models[[1]]), level = 0), color = unique(eval(models[[1]])$data$analysisunit)), lwd = 1.2) +
                                geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[2]])$data$analysisunit)],
                                          aes(x = relyear, y = predict(eval(models[[2]]), level = 0), color = unique(eval(models[[2]])$data$analysisunit)), lwd = 1.2) +
                                geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[3]])$data$analysisunit)],
                                          aes(x = relyear, y = predict(eval(models[[3]]), level = 0), color = unique(eval(models[[3]])$data$analysisunit)), lwd = 1.2)),
                       ifelse(length(models) == 4,
                              return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[1]])$data$analysisunit)],
                                                        aes(x = relyear, y = predict(eval(models[[1]]), level = 0), color = unique(eval(models[[1]])$data$analysisunit)), lwd = 1.2) +
                                       geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[2]])$data$analysisunit)],
                                                 aes(x = relyear, y = predict(eval(models[[2]]), level = 0), color = unique(eval(models[[2]])$data$analysisunit)), lwd = 1.2) +
                                       geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[3]])$data$analysisunit)],
                                                 aes(x = relyear, y = predict(eval(models[[3]]), level = 0), color = unique(eval(models[[3]])$data$analysisunit)), lwd = 1.2) +
                                       geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[4]])$data$analysisunit)],
                                                 aes(x = relyear, y = predict(eval(models[[4]]), level = 0), color = unique(eval(models[[4]])$data$analysisunit)), lwd = 1.2)),
                              ifelse(length(models) == 5,
                                     return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[1]])$data$analysisunit)],
                                                               aes(x = relyear, y = predict(eval(models[[1]]), level = 0), color = unique(eval(models[[1]])$data$analysisunit)), lwd = 1.2) +
                                              geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[2]])$data$analysisunit)],
                                                        aes(x = relyear, y = predict(eval(models[[2]]), level = 0), color = unique(eval(models[[2]])$data$analysisunit)), lwd = 1.2) +
                                              geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[3]])$data$analysisunit)],
                                                        aes(x = relyear, y = predict(eval(models[[3]]), level = 0), color = unique(eval(models[[3]])$data$analysisunit)), lwd = 1.2) +
                                              geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[4]])$data$analysisunit)],
                                                        aes(x = relyear, y = predict(eval(models[[4]]), level = 0), color = unique(eval(models[[4]])$data$analysisunit)), lwd = 1.2) +
                                              geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[5]])$data$analysisunit)],
                                                        aes(x = relyear, y = predict(eval(models[[5]]), level = 0), color = unique(eval(models[[5]])$data$analysisunit)), lwd = 1.2)),
                                     ifelse(length(models) == 6,
                                            return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[1]])$data$analysisunit)],
                                                                      aes(x = relyear, y = predict(eval(models[[1]]), level = 0), color = unique(eval(models[[1]])$data$analysisunit)), lwd = 1.2) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[2]])$data$analysisunit)],
                                                               aes(x = relyear, y = predict(eval(models[[2]]), level = 0), color = unique(eval(models[[2]])$data$analysisunit)), lwd = 1.2) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[3]])$data$analysisunit)],
                                                               aes(x = relyear, y = predict(eval(models[[3]]), level = 0), color = unique(eval(models[[3]])$data$analysisunit)), lwd = 1.2) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[4]])$data$analysisunit)],
                                                               aes(x = relyear, y = predict(eval(models[[4]]), level = 0), color = unique(eval(models[[4]])$data$analysisunit)), lwd = 1.2) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[5]])$data$analysisunit)],
                                                               aes(x = relyear, y = predict(eval(models[[5]]), level = 0), color = unique(eval(models[[5]])$data$analysisunit)), lwd = 1.2) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[6]])$data$analysisunit)],
                                                               aes(x = relyear, y = predict(eval(models[[6]]), level = 0), color = unique(eval(models[[6]])$data$analysisunit)), lwd = 1.2)),
                                            ifelse(length(models) == 7,
                                                   return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[1]])$data$analysisunit)],
                                                                             aes(x = relyear, y = predict(eval(models[[1]]), level = 0), color = unique(eval(models[[1]])$data$analysisunit)), lwd = 1.2) +
                                                            geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[2]])$data$analysisunit)],
                                                                      aes(x = relyear, y = predict(eval(models[[2]]), level = 0), color = unique(eval(models[[2]])$data$analysisunit)), lwd = 1.2) +
                                                            geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[3]])$data$analysisunit)],
                                                                      aes(x = relyear, y = predict(eval(models[[3]]), level = 0), color = unique(eval(models[[3]])$data$analysisunit)), lwd = 1.2) +
                                                            geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[4]])$data$analysisunit)],
                                                                      aes(x = relyear, y = predict(eval(models[[4]]), level = 0), color = unique(eval(models[[4]])$data$analysisunit)), lwd = 1.2) +
                                                            geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[5]])$data$analysisunit)],
                                                                      aes(x = relyear, y = predict(eval(models[[5]]), level = 0), color = unique(eval(models[[5]])$data$analysisunit)), lwd = 1.2) +
                                                            geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[6]])$data$analysisunit)],
                                                                      aes(x = relyear, y = predict(eval(models[[6]]), level = 0), color = unique(eval(models[[6]])$data$analysisunit)), lwd = 1.2) +
                                                            geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[7]])$data$analysisunit)],
                                                                      aes(x = relyear, y = predict(eval(models[[7]]), level = 0), color = unique(eval(models[[7]])$data$analysisunit)), lwd = 1.2)),
                                                   ifelse(length(models) == 8,
                                                          return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[1]])$data$analysisunit)],
                                                                                    aes(x = relyear, y = predict(eval(models[[1]]), level = 0), color = unique(eval(models[[1]])$data$analysisunit)), lwd = 1.2) +
                                                                   geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[2]])$data$analysisunit)],
                                                                             aes(x = relyear, y = predict(eval(models[[2]]), level = 0), color = unique(eval(models[[2]])$data$analysisunit)), lwd = 1.2) +
                                                                   geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[3]])$data$analysisunit)],
                                                                             aes(x = relyear, y = predict(eval(models[[3]]), level = 0), color = unique(eval(models[[3]])$data$analysisunit)), lwd = 1.2) +
                                                                   geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[4]])$data$analysisunit)],
                                                                             aes(x = relyear, y = predict(eval(models[[4]]), level = 0), color = unique(eval(models[[4]])$data$analysisunit)), lwd = 1.2) +
                                                                   geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[5]])$data$analysisunit)],
                                                                             aes(x = relyear, y = predict(eval(models[[5]]), level = 0), color = unique(eval(models[[5]])$data$analysisunit)), lwd = 1.2) +
                                                                   geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[6]])$data$analysisunit)],
                                                                             aes(x = relyear, y = predict(eval(models[[6]]), level = 0), color = unique(eval(models[[6]])$data$analysisunit)), lwd = 1.2) +
                                                                   geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[7]])$data$analysisunit)],
                                                                             aes(x = relyear, y = predict(eval(models[[7]]), level = 0), color = unique(eval(models[[7]])$data$analysisunit)), lwd = 1.2) +
                                                                   geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[8]])$data$analysisunit)],
                                                                             aes(x = relyear, y = predict(eval(models[[8]]), level = 0), color = unique(eval(models[[8]])$data$analysisunit)), lwd = 1.2)),
                                                          ifelse(length(models) == 9,
                                                                 return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[1]])$data$analysisunit)],
                                                                                           aes(x = relyear, y = predict(eval(models[[1]]), level = 0), color = unique(eval(models[[1]])$data$analysisunit)), lwd = 1.2) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[2]])$data$analysisunit)],
                                                                                    aes(x = relyear, y = predict(eval(models[[2]]), level = 0), color = unique(eval(models[[2]])$data$analysisunit)), lwd = 1.2) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[3]])$data$analysisunit)],
                                                                                    aes(x = relyear, y = predict(eval(models[[3]]), level = 0), color = unique(eval(models[[3]])$data$analysisunit)), lwd = 1.2) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[4]])$data$analysisunit)],
                                                                                    aes(x = relyear, y = predict(eval(models[[4]]), level = 0), color = unique(eval(models[[4]])$data$analysisunit)), lwd = 1.2) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[5]])$data$analysisunit)],
                                                                                    aes(x = relyear, y = predict(eval(models[[5]]), level = 0), color = unique(eval(models[[5]])$data$analysisunit)), lwd = 1.2) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[6]])$data$analysisunit)],
                                                                                    aes(x = relyear, y = predict(eval(models[[6]]), level = 0), color = unique(eval(models[[6]])$data$analysisunit)), lwd = 1.2) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[7]])$data$analysisunit)],
                                                                                    aes(x = relyear, y = predict(eval(models[[7]]), level = 0), color = unique(eval(models[[7]])$data$analysisunit)), lwd = 1.2) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[8]])$data$analysisunit)],
                                                                                    aes(x = relyear, y = predict(eval(models[[8]]), level = 0), color = unique(eval(models[[8]])$data$analysisunit)), lwd = 1.2) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[9]])$data$analysisunit)],
                                                                                    aes(x = relyear, y = predict(eval(models[[9]]), level = 0), color = unique(eval(models[[9]])$data$analysisunit)), lwd = 1.2)),
                                                                 ifelse(length(models) == 10,
                                                                        return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[1]])$data$analysisunit)],
                                                                                                  aes(x = relyear, y = predict(eval(models[[1]]), level = 0), color = unique(eval(models[[1]])$data$analysisunit)), lwd = 1.2) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[2]])$data$analysisunit)],
                                                                                           aes(x = relyear, y = predict(eval(models[[2]]), level = 0), color = unique(eval(models[[2]])$data$analysisunit)), lwd = 1.2) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[3]])$data$analysisunit)],
                                                                                           aes(x = relyear, y = predict(eval(models[[3]]), level = 0), color = unique(eval(models[[3]])$data$analysisunit)), lwd = 1.2) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[4]])$data$analysisunit)],
                                                                                           aes(x = relyear, y = predict(eval(models[[4]]), level = 0), color = unique(eval(models[[4]])$data$analysisunit)), lwd = 1.2) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[5]])$data$analysisunit)],
                                                                                           aes(x = relyear, y = predict(eval(models[[5]]), level = 0), color = unique(eval(models[[5]])$data$analysisunit)), lwd = 1.2) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[6]])$data$analysisunit)],
                                                                                           aes(x = relyear, y = predict(eval(models[[6]]), level = 0), color = unique(eval(models[[6]])$data$analysisunit)), lwd = 1.2) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[7]])$data$analysisunit)],
                                                                                           aes(x = relyear, y = predict(eval(models[[7]]), level = 0), color = unique(eval(models[[7]])$data$analysisunit)), lwd = 1.2) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[8]])$data$analysisunit)],
                                                                                           aes(x = relyear, y = predict(eval(models[[8]]), level = 0), color = unique(eval(models[[8]])$data$analysisunit)), lwd = 1.2) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[9]])$data$analysisunit)],
                                                                                           aes(x = relyear, y = predict(eval(models[[9]]), level = 0), color = unique(eval(models[[9]])$data$analysisunit)), lwd = 1.2) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[10]])$data$analysisunit)],
                                                                                           aes(x = relyear, y = predict(eval(models[[10]]), level = 0), color = unique(eval(models[[10]])$data$analysisunit)), lwd = 1.2)),
                                                                        ifelse(length(models) == 11,
                                                                               return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[1]])$data$analysisunit)],
                                                                                                         aes(x = relyear, y = predict(eval(models[[1]]), level = 0), color = unique(eval(models[[1]])$data$analysisunit)), lwd = 1.2) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[2]])$data$analysisunit)],
                                                                                                  aes(x = relyear, y = predict(eval(models[[2]]), level = 0), color = unique(eval(models[[2]])$data$analysisunit)), lwd = 1.2) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[3]])$data$analysisunit)],
                                                                                                  aes(x = relyear, y = predict(eval(models[[3]]), level = 0), color = unique(eval(models[[3]])$data$analysisunit)), lwd = 1.2) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[4]])$data$analysisunit)],
                                                                                                  aes(x = relyear, y = predict(eval(models[[4]]), level = 0), color = unique(eval(models[[4]])$data$analysisunit)), lwd = 1.2) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[5]])$data$analysisunit)],
                                                                                                  aes(x = relyear, y = predict(eval(models[[5]]), level = 0), color = unique(eval(models[[5]])$data$analysisunit)), lwd = 1.2) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[6]])$data$analysisunit)],
                                                                                                  aes(x = relyear, y = predict(eval(models[[6]]), level = 0), color = unique(eval(models[[6]])$data$analysisunit)), lwd = 1.2) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[7]])$data$analysisunit)],
                                                                                                  aes(x = relyear, y = predict(eval(models[[7]]), level = 0), color = unique(eval(models[[7]])$data$analysisunit)), lwd = 1.2) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[8]])$data$analysisunit)],
                                                                                                  aes(x = relyear, y = predict(eval(models[[8]]), level = 0), color = unique(eval(models[[8]])$data$analysisunit)), lwd = 1.2) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[9]])$data$analysisunit)],
                                                                                                  aes(x = relyear, y = predict(eval(models[[9]]), level = 0), color = unique(eval(models[[9]])$data$analysisunit)), lwd = 1.2) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[10]])$data$analysisunit)],
                                                                                                  aes(x = relyear, y = predict(eval(models[[10]]), level = 0), color = unique(eval(models[[10]])$data$analysisunit)), lwd = 1.2) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[11]])$data$analysisunit)],
                                                                                                  aes(x = relyear, y = predict(eval(models[[11]]), level = 0), color = unique(eval(models[[11]])$data$analysisunit)), lwd = 1.2)),
                                                                               
                                                                               return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[1]])$data$analysisunit)],
                                                                                                         aes(x = relyear, y = predict(eval(models[[1]]), level = 0), color = unique(eval(models[[1]])$data$analysisunit)), lwd = 1.2) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[2]])$data$analysisunit)],
                                                                                                  aes(x = relyear, y = predict(eval(models[[2]]), level = 0), color = unique(eval(models[[2]])$data$analysisunit)), lwd = 1.2) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[3]])$data$analysisunit)],
                                                                                                  aes(x = relyear, y = predict(eval(models[[3]]), level = 0), color = unique(eval(models[[3]])$data$analysisunit)), lwd = 1.2) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[4]])$data$analysisunit)],
                                                                                                  aes(x = relyear, y = predict(eval(models[[4]]), level = 0), color = unique(eval(models[[4]])$data$analysisunit)), lwd = 1.2) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[5]])$data$analysisunit)],
                                                                                                  aes(x = relyear, y = predict(eval(models[[5]]), level = 0), color = unique(eval(models[[5]])$data$analysisunit)), lwd = 1.2) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[6]])$data$analysisunit)],
                                                                                                  aes(x = relyear, y = predict(eval(models[[6]]), level = 0), color = unique(eval(models[[6]])$data$analysisunit)), lwd = 1.2) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[7]])$data$analysisunit)],
                                                                                                  aes(x = relyear, y = predict(eval(models[[7]]), level = 0), color = unique(eval(models[[7]])$data$analysisunit)), lwd = 1.2) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[8]])$data$analysisunit)],
                                                                                                  aes(x = relyear, y = predict(eval(models[[8]]), level = 0), color = unique(eval(models[[8]])$data$analysisunit)), lwd = 1.2) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[9]])$data$analysisunit)],
                                                                                                  aes(x = relyear, y = predict(eval(models[[9]]), level = 0), color = unique(eval(models[[9]])$data$analysisunit)), lwd = 1.2) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[10]])$data$analysisunit)],
                                                                                                  aes(x = relyear, y = predict(eval(models[[10]]), level = 0), color = unique(eval(models[[10]])$data$analysisunit)), lwd = 1.2) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[11]])$data$analysisunit)],
                                                                                                  aes(x = relyear, y = predict(eval(models[[11]]), level = 0), color = unique(eval(models[[11]])$data$analysisunit)), lwd = 1.2) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[12]])$data$analysisunit)],
                                                                                                  aes(x = relyear, y = predict(eval(models[[12]]), level = 0), color = unique(eval(models[[12]])$data$analysisunit)), lwd = 1.2)))))))))))))
}

addfits_blacktrendlines <- function(models, plot_i, param){
  aucol <- as.name(names(plot_i$data)[1])
  ifelse(length(models) == 1,
         return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                   aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)),
         ifelse(length(models) == 2,
                return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                          aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                         geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[2]], "$data$", aucol, ")"))), ],
                                   aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)),
                ifelse(length(models) == 3,
                       return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                                 aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[2]], "$data$", aucol, ")"))), ],
                                          aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[3]], "$data$", aucol, ")"))), ],
                                          aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)),
                       ifelse(length(models) == 4,
                              return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                                        aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                       geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[2]], "$data$", aucol, ")"))), ],
                                                 aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                       geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[3]], "$data$", aucol, ")"))), ],
                                                 aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                       geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[4]], "$data$", aucol, ")"))), ],
                                                 aes(x = relyear, y = predict(eval(models[[4]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)),
                              ifelse(length(models) == 5,
                                     return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                                               aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                              geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[2]], "$data$", aucol, ")"))), ],
                                                        aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                              geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[3]], "$data$", aucol, ")"))), ],
                                                        aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                              geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[4]], "$data$", aucol, ")"))), ],
                                                        aes(x = relyear, y = predict(eval(models[[4]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                              geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[5]], "$data$", aucol, ")"))), ],
                                                        aes(x = relyear, y = predict(eval(models[[5]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)),
                                     ifelse(length(models) == 6,
                                            return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                                                      aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[2]], "$data$", aucol, ")"))), ],
                                                               aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[3]], "$data$", aucol, ")"))), ],
                                                               aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[4]], "$data$", aucol, ")"))), ],
                                                               aes(x = relyear, y = predict(eval(models[[4]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[5]], "$data$", aucol, ")"))), ],
                                                               aes(x = relyear, y = predict(eval(models[[5]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[6]], "$data$", aucol, ")"))), ],
                                                               aes(x = relyear, y = predict(eval(models[[6]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)),
                                            ifelse(length(models) == 7,
                                                   return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                                                             aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                            geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[2]], "$data$", aucol, ")"))), ],
                                                                      aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                            geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[3]], "$data$", aucol, ")"))), ],
                                                                      aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                            geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[4]], "$data$", aucol, ")"))), ],
                                                                      aes(x = relyear, y = predict(eval(models[[4]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                            geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[5]], "$data$", aucol, ")"))), ],
                                                                      aes(x = relyear, y = predict(eval(models[[5]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                            geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[6]], "$data$", aucol, ")"))), ],
                                                                      aes(x = relyear, y = predict(eval(models[[6]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                            geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[7]], "$data$", aucol, ")"))), ],
                                                                      aes(x = relyear, y = predict(eval(models[[7]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)),
                                                   ifelse(length(models) == 8,
                                                          return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                                                                    aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                   geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[2]], "$data$", aucol, ")"))), ],
                                                                             aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                   geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[3]], "$data$", aucol, ")"))), ],
                                                                             aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                   geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[4]], "$data$", aucol, ")"))), ],
                                                                             aes(x = relyear, y = predict(eval(models[[4]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                   geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[5]], "$data$", aucol, ")"))), ],
                                                                             aes(x = relyear, y = predict(eval(models[[5]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                   geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[6]], "$data$", aucol, ")"))), ],
                                                                             aes(x = relyear, y = predict(eval(models[[6]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                   geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[7]], "$data$", aucol, ")"))), ],
                                                                             aes(x = relyear, y = predict(eval(models[[7]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                   geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[8]], "$data$", aucol, ")"))), ],
                                                                             aes(x = relyear, y = predict(eval(models[[8]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)),
                                                          ifelse(length(models) == 9,
                                                                 return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                                                                           aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[2]], "$data$", aucol, ")"))), ],
                                                                                    aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[3]], "$data$", aucol, ")"))), ],
                                                                                    aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[4]], "$data$", aucol, ")"))), ],
                                                                                    aes(x = relyear, y = predict(eval(models[[4]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[5]], "$data$", aucol, ")"))), ],
                                                                                    aes(x = relyear, y = predict(eval(models[[5]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[6]], "$data$", aucol, ")"))), ],
                                                                                    aes(x = relyear, y = predict(eval(models[[6]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[7]], "$data$", aucol, ")"))), ],
                                                                                    aes(x = relyear, y = predict(eval(models[[7]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[8]], "$data$", aucol, ")"))), ],
                                                                                    aes(x = relyear, y = predict(eval(models[[8]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[9]], "$data$", aucol, ")"))), ],
                                                                                    aes(x = relyear, y = predict(eval(models[[9]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)),
                                                                 ifelse(length(models) == 10,
                                                                        return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[2]], "$data$", aucol, ")"))), ],
                                                                                           aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[3]], "$data$", aucol, ")"))), ],
                                                                                           aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[4]], "$data$", aucol, ")"))), ],
                                                                                           aes(x = relyear, y = predict(eval(models[[4]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[5]], "$data$", aucol, ")"))), ],
                                                                                           aes(x = relyear, y = predict(eval(models[[5]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[6]], "$data$", aucol, ")"))), ],
                                                                                           aes(x = relyear, y = predict(eval(models[[6]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[7]], "$data$", aucol, ")"))), ],
                                                                                           aes(x = relyear, y = predict(eval(models[[7]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[8]], "$data$", aucol, ")"))), ],
                                                                                           aes(x = relyear, y = predict(eval(models[[8]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[9]], "$data$", aucol, ")"))), ],
                                                                                           aes(x = relyear, y = predict(eval(models[[9]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[10]], "$data$", aucol, ")"))), ],
                                                                                           aes(x = relyear, y = predict(eval(models[[10]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)),
                                                                        ifelse(length(models) == 11,
                                                                               return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                                                                                         aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[2]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[3]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[4]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[4]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[5]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[5]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[6]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[6]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[7]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[7]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[8]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[8]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[9]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[9]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[10]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[10]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[11]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[11]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)),
                                                                               
                                                                               return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                                                                                         aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[2]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[3]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[4]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[4]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[5]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[5]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[6]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[6]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[7]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[7]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[8]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[8]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[9]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[9]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[10]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[10]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[11]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[11]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[12]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[12]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)))))))))))))
}

# Specify what to produce --------------
EDA <- "maps" #Create and export Exploratory Data Analysis plots ("maps and plots" = create all EDA output, 
              #                                                   "maps" = create geographic scope maps only,
              #                                                   "plots" = create data exploration plots only,
              #                                                   "no" (or anything else) = skip all EDA output)

Analyses <- c("none") #Which analyses to run? c("BB_all," "BB_pct", "PC", "PO", and/or "PA") or c("none") for just EDA plotting


if(str_detect(EDA, "maps")){
  # Code source for original rotate sf function: https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/geospatial-data/
  #' Rotate simple features for 3D layers
  #' Rotates a simple features layer using a shear matrix transformation on the 
  #' \code{geometry} column. This can get nice for visualisation and works with
  #' points, lines and polygons.
  #'
  #' @param data an object of class \code{sf}
  #' @param x_add integer; x value to move geometry in space
  #' @param y_add integer; x value to move geometry in space
  #'
  #' #' @importFrom magrittr %>%
  
  rotate_sf <- function(data, x_add = 0, y_add = 0, ma, coast = "Atlantic"){
    
    if(coast == "Atlantic"){
      if(unique(ma) %in% c("Banana River", "Indian River-Malabar to Vero Beach", 
                          "Indian River-Vero Beach to Ft. Pierce", "Jensen Beach to Jupiter Inlet", 
                          "Mosquito Lagoon")){
        shear_matrix <- function (x) { 
          #matrix(c(2, 1.2, 0, 1), 2, 2)
          # matrix(c(0.2, -0.3, 0.5, 0.7), 2, 2)
          # matrix(c(0.2, -0.3, 0, 0.7), 2, 2)
          matrix(c(1, 1.2, 0, 1), 2, 2)
        }
        
        rotate_matrix <- function(x) { 
          matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
        }
        
        data %>% 
          dplyr::mutate(
            geometry = 
              # .$geometry * shear_matrix() * rotate_matrix(pi*0.6) + c(x_add, y_add)
              .$geometry * shear_matrix() * rotate_matrix(pi*0.2) + c(x_add, y_add)
          )
      } else{
        shear_matrix <- function (x) { 
          #matrix(c(2, 1.2, 0, 1), 2, 2)
          matrix(c(2, 1.2, 0, 1), 2, 2) 
        }
        
        rotate_matrix <- function(x) { 
          matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
        }
        
        data %>% 
          dplyr::mutate(
            geometry = 
              .$geometry * shear_matrix() * rotate_matrix(pi/20) + c(x_add, y_add)
          )
      }
      
    } else{
      shear_matrix <- function (x) { 
        #matrix(c(2, 1.2, 0, 1), 2, 2)
        matrix(c(2, -1.2, 0, 1), 2, 2) 
      }
      
      rotate_matrix <- function(x) { 
        matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
      }
      
      data %>% 
        dplyr::mutate(
          geometry = 
            .$geometry * shear_matrix() * rotate_matrix(pi*1.98) + c(x_add, y_add)
        )
    }
  }
  
  
  #Create model objects, tables and plots for all MAs w/ >5 yrs of data-------------------------------------------------
  #Load geospatial data
  GeoDBdate <- "26jan2023"
  locs_pts <- st_read(here::here(paste0("SAV/mapping/SampleLocations", GeoDBdate, "/seacar_dbo_vw_SampleLocation_Point.shp")))
  locs_lns <- st_read(here::here(paste0("SAV/mapping/SampleLocations", GeoDBdate, "/seacar_dbo_vw_SampleLocation_Line.shp")))
  rcp <- st_read(here::here("SAV/mapping/orcp_all_sites/orcp_all_sites/ORCP_Managed_Areas.shp"))
  counties <- st_read(here::here("SAV/mapping/FLCounties/Counties_-_Detailed_Shoreline.shp"))
  corners <- fread(here::here("SAV/mapping/MApolygons_corners.csv"))
  #add 20% of difference (xmax-xmin) to xmax to help prevent year labels from getting cut off map images and 10% to ymax
  corners[, `:=` (xmax = xmax + (xmax-xmin)*0.25, ymax = ymax + (ymax-ymin)*0.1)]
  
  locs_pts <- st_make_valid(locs_pts)
  locs_lns <- st_make_valid(locs_lns)
  rcp <- st_make_valid(rcp)
  counties <- st_make_valid(counties)
  
  locs_pts <- st_transform(locs_pts, crs = 4326)
  locs_lns <- st_transform(locs_lns, crs = 4326)
  rcp <- st_transform(rcp, crs = 4326)
  counties <- st_transform(counties, crs = 4326)
  
  locs_pts_rcp <- locs_pts[rcp, , op = st_intersects]
  locs_lns_rcp <- locs_lns[rcp, , op = st_intersects]
  
  pnames <- distinct(SAV4[, .(ProgramID, ProgramName)])
  locs_pts_rcp <- merge(locs_pts_rcp, pnames, by = "ProgramID", all.x = TRUE)
  locs_lns_rcp <- merge(locs_lns_rcp, pnames, by = "ProgramID", all.x = TRUE)
}

# #BBSAP WBID test
# wbids <- st_read(here::here("SAV/mapping/WBIDS_SP.gdb"))
# wbids <- st_make_valid(wbids)
# wbids <- st_transform(wbids, crs = 4326)
# wbids_rcp <- wbids[rcp, , op = st_intersects]
# bbsap <- subset(locs_pts_rcp, locs_pts_rcp$LocationID %in% SAV4[ManagedAreaName == "Big Bend Seagrasses", unique(LocationID)])
# wbids_bbsap <- wbids[bbsap, , op = st_intersects]
# wbids_bbsap <- subset(wbids_bbsap, wbids_bbsap$WATER_TYPE != "BEACH")
# 
# for(pt in seq_along(bbsap$geometry)){
#   bbsap$wbid[pt] <- wbids_bbsap$WBID[which.min(st_distance(bbsap[pt, ], wbids_bbsap))]
# }
# 
# #check visually that points were sorted properly
# mapview::mapview(bbsap, zcol = "wbid") + mapview::mapview(wbids_bbsap)
# 
# bbsap_dat <- SAV4[ManagedAreaName == "Big Bend Seagrasses", ]
# bbsap_dat[, wbid := bbsap$wbid[which(bbsap$LocationID == LocationID)], by = LocationID]
# bbsap_dat_tt <- bbsap_dat[analysisunit == "Thalassia testudinum", ]
# 
# origmod <- lme(BB_pct ~ relyear,
#              random = list(LocationID = ~relyear),
#              control = list(msMaxIter = 1000, msMaxEval = 1000, sing.tol=1e-20),
#              na.action = na.omit, 
#              data = bbsap_dat_tt)
# bbsap_dat_tt$origfit <- predict(origmod, level = 0)
# 
# wbidmod <- lme(BB_pct ~ relyear + wbid + relyear*wbid,
#              random = list(LocationID = ~relyear),
#              control = list(msMaxIter = 1000, msMaxEval = 1000, sing.tol=1e-20),
#              na.action = na.omit, 
#              data = bbsap_dat_tt)
# bbsap_dat_tt$wbidfit <- predict(wbidmod, level = 0)
# 
# ggplot(data = bbsap_dat_tt, aes(x = relyear, y = BB_pct, group = wbid)) +
#   geom_jitter(aes(color = wbid), width = 1, height = 1) +
#   geom_line(aes(y = origfit), color="black", size=0.75, alpha=0.7) +
#   geom_line(aes(y = wbidfit, color = wbid), size=0.75, alpha=0.7) +
#   theme_bw()
#   
# 
# origmod2 <- brm(BB_pct ~ relyear + (1 + relyear | LocationID), data = bbsap_dat_tt, 
#                 family = gaussian(), cores = 4, chains = 4, warmup = 1000, iter = 5000, 
#                 backend = "cmdstanr", threads = threading(2)) 
# conditional_effects(origmod2, re_formula = NULL)
# 
# wbidmod2 <- brm(BB_pct ~ relyear + (1 + relyear | gr(LocationID, by = wbid)), data = bbsap_dat, 
#                 family = gaussian(), cores = 4, chains = 4, warmup = 1000, iter = 5000, 
#                 backend = "cmdstanr", threads = threading(2))
# conditional_effects(wbidmod2, re_formula = NULL)
# 
# wbidmod2b <- brm(BB_pct ~ relyear + (1 + relyear | LocationID:wbid), data = bbsap_dat, 
#                 family = gaussian(), cores = 4, chains = 4, warmup = 1000, iter = 5000, 
#                 backend = "cmdstanr", threads = threading(2))
# conditional_effects(wbidmod2b, re_formula = NULL)
# 
# wbidmod3 <- brm(BB_pct ~ relyear + (1 + relyear | wbid), data = bbsap_dat, 
#                 family = gaussian(), cores = 4, chains = 4, warmup = 1000, iter = 5000, 
#                 backend = "cmdstanr", threads = threading(2)) 
# spread_draws(wbidmod2, r_wbid[wbid, year])

#Empty data.table to house names of any failed models generated below.
failedmods <- data.table(model = character(),
                         error = character())

#Create a table of the proportion of present SAV types by managed area and year
props_halid <- SAV4 %>% filter(str_detect(analysisunit_halid, "Total|Drift|spp\\.", negate = TRUE), !is.na(PA)) %>% group_by(ManagedAreaName, analysisunit_halid, relyear) %>% summarize(n_P = sum(PA), ntot_PA = n(), prop_P = n_P/ntot_PA)
# props_halcom <- SAV4 %>% filter(str_detect(analysisunit_halcom, "Total", negate = TRUE)) %>% group_by(ManagedAreaName, analysisunit_halcom, relyear) %>% summarize(n_P_halcom = sum(PA), ntot_PA_halcom = n(), prop_P_halcom = n_P_halcom/ntot_PA_halcom)
props <- SAV4 %>% filter(str_detect(analysisunit, "Total|Drift|decipiens|engelmannii|johnsonii|Unidentified", negate = TRUE), !is.na(PA)) %>% group_by(ManagedAreaName, analysisunit, relyear) %>% summarize(n_P = sum(PA), ntot_PA = n(), prop_P = n_P/ntot_PA)
setDT(props_halid)
setDT(props)
for(m in unique(props_halid$ManagedAreaName)){
  props_halid[ManagedAreaName == m, `:=` (n_allsp_P = sum(n_P), sp_prop = n_P/sum(n_P), sp_pct = (n_P/sum(n_P)) * 100), by = c("relyear")]
}
for(m in unique(props$ManagedAreaName)){
  # props_halcom[ManagedAreaName == m, `:=` (n_allsp_P_halcom = sum(n_P_halcom), sp_prop_halcom = n_P_halcom/sum(n_P_halcom), sp_pct_halcom = (n_P_halcom/sum(n_P_halcom)) * 100), by = c("relyear")]
  props[ManagedAreaName == m, `:=` (n_allsp_P = sum(n_P), sp_prop = n_P/sum(n_P), sp_pct = (n_P/sum(n_P)) * 100), by = c("relyear")]
}
setnames(props_halid, "analysisunit_halid", "analysisunit")
props2 <- distinct(rbind(props_halid, props))
setorder(props2, ManagedAreaName, relyear, analysisunit)
props <- props2

# spcollist <- c("#00374f",
#                "#004d68",
#                "#006481",
#                "#007c99",
#                "#0094b0",
#                "#00aec6",
#                "#00c9db",
#                "#00e4ee",
#                "#00ffff")

# spcollist <- sequential_hcl(12, palette = "Mako")

spcollist_a <- sequential_hcl(8, palette = "YlOrRd")
spcollist_b <- sequential_hcl(8, palette = "YlGnBu", rev = TRUE)
spcollist <- append(spcollist_a[4:7], spcollist_b[3:7])
spcollist <- rev(spcollist)

# spcollist <- hcl.colors(n = 8, palette = "Blues 3")
spp <- c("Halodule wrightii", "Halophila decipiens", "Halophila engelmannii", "Halophila johnsonii", 
         "Halophila spp.", "Ruppia maritima", "Syringodium filiforme", "Thalassia testudinum", "Attached algae")

spp_common <- c("Halophila spp.", "Johnson's seagrass", "Manatee grass", "Paddle grass", 
                "Shoal grass", "Star grass", "Turtle grass", "Widgeon grass", "Attached algae")

usenames <- "common" #alternative is "scientific"
if(usenames == "common"){
  spcols <- setNames(spcollist, spp_common)
} else{
  spcols <- setNames(spcollist, spp)
}

spindet_nm <- c("Unidentified Halophila")
spindet_cl <- spcols["Halophila spp."][[1]]
spindet <- setNames(spindet_cl, spindet_nm)
spcols <- append(spcols, spindet, after = which(spcols == spcols["Halophila spp."][[1]]))

if(usenames == "common"){
  SAV4[, `:=` (analysisunit_halid = fcase(analysisunit_halid == "Thalassia testudinum", "Turtle grass",
                                          analysisunit_halid == "Syringodium filiforme", "Manatee grass",
                                          analysisunit_halid == "Halodule wrightii", "Shoal grass",
                                          analysisunit_halid == "Ruppia maritima", "Widgeon grass",
                                          analysisunit_halid == "Halophila decipiens", "Paddle grass",
                                          analysisunit_halid == "Halophila engelmannii", "Star grass",
                                          analysisunit_halid == "Halophila johnsonii", "Johnson's seagrass",
                                          analysisunit_halid == "Unidentified Halophila", "Unidentified Halophila",
                                          analysisunit_halid == "Halophila spp.", "Halophila spp.",
                                          analysisunit_halid == "Total seagrass", "Total seagrass",
                                          analysisunit_halid == "Attached algae", "Attached algae",
                                          analysisunit_halid == "Drift algae", "Drift algae",
                                          analysisunit_halid == "Total SAV", "Total SAV"),
               analysisunit = fcase(analysisunit == "Thalassia testudinum", "Turtle grass",
                                    analysisunit == "Syringodium filiforme", "Manatee grass",
                                    analysisunit == "Halodule wrightii", "Shoal grass",
                                    analysisunit == "Ruppia maritima", "Widgeon grass",
                                    analysisunit == "Halophila decipiens", "Paddle grass",
                                    analysisunit == "Halophila engelmannii", "Star grass",
                                    analysisunit == "Halophila johnsonii", "Johnson's seagrass",
                                    analysisunit == "Unidentified Halophila", "Unidentified Halophila",
                                    analysisunit == "Halophila spp.", "Halophila spp.",
                                    analysisunit == "Total seagrass", "Total seagrass",
                                    analysisunit == "Attached algae", "Attached algae",
                                    analysisunit == "Drift algae", "Drift algae",
                                    analysisunit == "Total SAV", "Total SAV"))]
  
  props[, analysisunit := fcase(analysisunit == "Thalassia testudinum", "Turtle grass",
                                analysisunit == "Syringodium filiforme", "Manatee grass",
                                analysisunit == "Halodule wrightii", "Shoal grass",
                                analysisunit == "Ruppia maritima", "Widgeon grass",
                                analysisunit == "Halophila decipiens", "Paddle grass",
                                analysisunit == "Halophila engelmannii", "Star grass",
                                analysisunit == "Halophila johnsonii", "Johnson's seagrass",
                                analysisunit == "Unidentified Halophila", "Unidentified Halophila",
                                analysisunit == "Halophila spp.", "Halophila spp.",
                                analysisunit == "Attached algae", "Attached algae")]
  
  props <- props[, analysisunit := factor(analysisunit, levels = c("Unidentified Halophila",
                                                                   "Halophila spp.",
                                                                   "Johnson's seagrass",
                                                                   "Manatee grass",
                                                                   "Paddle grass",
                                                                   "Shoal grass",
                                                                   "Star grass",
                                                                   "Turtle grass",
                                                                   "Widgeon grass",
                                                                   "Attached algae"))]
}

if(usenames != "common"){
  props <- props[, analysisunit := factor(analysisunit, levels = c("Halodule wrightii",
                                                                   "Halophila decipiens",
                                                                   "Halophila engelmannii",
                                                                   "Halophila johnsonii",
                                                                   "Unidentified Halophila",
                                                                   "Halophila spp.",
                                                                   "Ruppia maritima",
                                                                   "Syringodium filiforme",
                                                                   "Thalassia testudinum",
                                                                   "Attached algae"))]
}

# prcollist <- hcl.colors(n = length(unique(SAV4$ProgramID)), palette = "viridis")
prcollist_a <- sequential_hcl(length(unique(SAV4$ProgramName)), palette = "YlOrRd")
prcollist_b <- sequential_hcl(length(unique(SAV4$ProgramName)), palette = "YlGnBu", rev = TRUE)
prcollist <- append(prcollist_a[which(seq(1, length(prcollist_a)) %% 2 == 0)], 
                    prcollist_b[which(seq(1, length(prcollist_b)) %% 2 != 0)])
prcollist <- rev(prcollist)
set.seed(4691)
progs <- sample(sort(unique(SAV4$ProgramName)))
prcols <- setNames(prcollist, progs)

parameters <- data.table(column = c(as.name("BB_all"), as.name("BB_pct"), as.name("PC"), as.name("PO"), as.name("PA")),
                         name = c("Braun Blanquet score", "Median percent cover", "Visual percent cover", "Percent occurrence", "Frequency of occurrence"),
                         type = c("BBall", "BBpct", "PC", "PO", "PA"))

plot_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Arial"),
        # title = element_text(face="bold"),
        plot.title = element_text(hjust = 0.5, size = 12, color = "#314963"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "#314963"),
        legend.title = element_text(size = 10),
        legend.text.align = 0,
        axis.title.x = element_text(size = 10, margin = margin(t = 5, r = 0,
                                                               b = 10, l = 0)),
        axis.title.y = element_text(size = 10, margin = margin(t = 0, r = 10,
                                                               b = 0, l = 0)),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = -45, hjust = 0))

#Managed areas that should have Halophila species combined:
ma_halspp <- c("Banana River", "Indian River-Malabar to Vero Beach", "Indian River-Vero Beach to Ft. Pierce", "Jensen Beach to Jupiter Inlet",
               "Loxahatchee River-Lake Worth Creek", "Mosquito Lagoon", "Biscayne Bay", "Florida Keys NMS")

#save summary stats file
stats_pct <- SAV4[ManagedAreaName %in% ma_halspp, ] %>%
  group_by(ManagedAreaName, analysisunit) %>%
  summarize(ParameterName="Median percent cover (from BB scores)",
            N_Programs=length(unique(ProgramID)),
            ProgramIDs=list(sort(unique(ProgramID))),
            N_Data=length(BB_pct[!is.na(BB_pct)]),
            N_Years=length(unique(Year[!is.na(Year) & !is.na(BB_pct)])),
            EarliestYear=min(Year[!is.na(BB_pct)]),
            LatestYear=max(Year[!is.na(BB_pct)]),
            SufficientData=ifelse(N_Data>0 & N_Years>=5, TRUE, FALSE))
stats_pct2 <- SAV4[ManagedAreaName %in% setdiff(unique(SAV4$ManagedAreaName), ma_halspp), ] %>%
  group_by(ManagedAreaName, analysisunit_halid) %>%
  summarize(ParameterName="Median percent cover (from BB scores)",
            N_Programs=length(unique(ProgramID)),
            ProgramIDs=list(sort(unique(ProgramID))),
            N_Data=length(BB_pct[!is.na(BB_pct)]),
            N_Years=length(unique(Year[!is.na(Year) & !is.na(BB_pct)])),
            EarliestYear=min(Year[!is.na(BB_pct)]),
            LatestYear=max(Year[!is.na(BB_pct)]),
            SufficientData=ifelse(N_Data>0 & N_Years>=5, TRUE, FALSE))
setDT(stats_pct2)
setnames(stats_pct2, "analysisunit_halid", "analysisunit")
stats_pct <- distinct(rbind(stats_pct, stats_pct2))
setcolorder(stats_pct, c("ManagedAreaName", "analysisunit"))
setDT(stats_pct)
stats_pct[N_Years == 0, `:=` (EarliestYear = NA, LatestYear = NA)]
# fwrite(stats_pct, here::here("SAV/output/data/SAV_BBpct_Stats.txt"), sep = "|", sep2 = ",")

stats_pa <- SAV4[ManagedAreaName %in% ma_halspp, ] %>%
  group_by(ManagedAreaName, analysisunit) %>%
  summarize(ParameterName="Frequency of occurrence",
            N_Programs=length(unique(ProgramID)),
            ProgramIDs=list(sort(unique(ProgramID))),
            N_Data=length(PA[!is.na(PA)]),
            N_Years=length(unique(Year[!is.na(Year) & !is.na(PA)])),
            EarliestYear=min(Year[!is.na(PA)]),
            LatestYear=max(Year[!is.na(PA)]),
            SufficientData=ifelse(N_Data>0 & N_Years>=5, TRUE, FALSE))
stats_pa2 <- SAV4[ManagedAreaName %in% setdiff(unique(SAV4$ManagedAreaName), ma_halspp), ] %>%
  group_by(ManagedAreaName, analysisunit_halid) %>%
  summarize(ParameterName="Frequency of occurrence",
            N_Programs=length(unique(ProgramID)),
            ProgramIDs=list(sort(unique(ProgramID))),
            N_Data=length(PA[!is.na(PA)]),
            N_Years=length(unique(Year[!is.na(Year) & !is.na(PA)])),
            EarliestYear=min(Year[!is.na(PA)]),
            LatestYear=max(Year[!is.na(PA)]),
            SufficientData=ifelse(N_Data>0 & N_Years>=5, TRUE, FALSE))
setDT(stats_pa2)
setnames(stats_pa2, "analysisunit_halid", "analysisunit")
stats_pa <- distinct(rbind(stats_pa, stats_pa2))
setcolorder(stats_pa, c("ManagedAreaName", "analysisunit"))
setDT(stats_pa)
stats_pa[N_Years == 0, `:=` (EarliestYear = NA, LatestYear = NA)]
# fwrite(stats_pa, here::here("SAV/output/data/SAV_PA_Stats.txt"), sep = "|", sep2 = ",")

# stats2 <- rbind(stats_pct, stats_pa)
# fwrite(stats2, here::here(paste0("SAV/output/data/SAV_BBpct_PA_Stats", Sys.Date(), ".txt")), sep = "|")
statpardat <- list("BB_pct" = stats_pct, "PA" = stats_pa)
openxlsx::write.xlsx(statpardat, here::here("SAV/output/data/SAV_BBpct_PA_Stats.xlsx"), colNames = c(TRUE, TRUE), colWidths = c("auto", "auto"), firstRow = c(TRUE, TRUE))
openxlsx::write.xlsx(statpardat, here::here(paste0("SAV/output/data/SAV_BBpct_PA_Stats_", Sys.Date(), ".xlsx")), colNames = c(TRUE, TRUE), colWidths = c("auto", "auto"), firstRow = c(TRUE, TRUE))

# #subset to run only part of the script------------------------------------------------------
# parameters <- parameters[column == "PA", ]

#Save session info-----------------------------------------------------
session <- sessionInfo()
saveRDS(session, here::here(paste0("SAV/output/SessionInfo_", Sys.Date())))

#start script----------------------------------------------------------------------
tic()
n <- 0
seed <- 352
set.seed(seed)
for(p in parameters$column){
  
  cat(paste0("\nStarting indicator: ", p, "\n"))
  
  #List managed areas with at least 5 years of data
  nyears <- SAV4[!is.na(eval(p)) & !is.na(analysisunit), ] %>% group_by(ManagedAreaName, analysisunit) %>% summarize(type = paste0(p), nyr = length(unique(Year)))
  nyears2 <- SAV4[!is.na(eval(p)) & !is.na(analysisunit_halid), ] %>% group_by(ManagedAreaName, analysisunit_halid) %>% summarize(type = paste0(p), nyr = length(unique(Year)))
  setDT(nyears2)
  setnames(nyears2, "analysisunit_halid", "analysisunit")
  nyears <- distinct(rbind(nyears, nyears2))
  ma_include <- unique(subset(nyears, nyears$nyr >= 5)$ManagedAreaName)
  
  #For each managed area, make sure there are multiple levels of BB scores per species; remove ones that don't from further consideration.
  for(i in ma_include){
    # bbscores_i <- subset(SAV4, SAV4$ManagedAreaName == i) %>%
    #   group_by(analysisunit, BB_all) %>%
    #   summarize(n = length(BB_all))
    
    # species <- c()
    
    # for(s in unique(bbscores_i$analysisunit)){
    #   if(nrow(subset(bbscores_i, bbscores_i$analysisunit == s)) > 1){
    #     species <- append(species, s)
    #   } else {
    #     next
    #   }
    # }
    
    cat(paste0("\nStarting MA: ", i, "\n"))
    
    #create data exploration plots-----------------------------------------------------
    if(EDA %in% c("plots", "maps", "plots and maps")){
      if(str_detect(EDA, "plots")){
        parvYear_bysp <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ],
                                aes(x = Year, y = eval(p), color = analysisunit)) +
          geom_jitter() +
          theme_bw() +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
               y = parameters[column == p, name],
               color = "Species") +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                             aesthetics = c("color", "fill"))
        
        saveRDS(parvYear_bysp, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                 gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                 ifelse(stringr::str_detect(i, "NERR"), paste0("ERR_EDA001_", str_replace(p, "_", ""), "vYear_bysp.rds"), 
                                                        ifelse(stringr::str_detect(i, "NMS"), paste0("MS_EDA001_", str_replace(p, "_", ""), "vYear_bysp.rds"), paste0("AP_EDA001_", str_replace(p, "_", ""), "vYear_bysp.rds"))))))
        
        parvYear_bypr <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = eval(p), color = as.factor(ProgramID))) +
          geom_jitter() +
          theme_bw() +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
               y = parameters[column == p, name],
               color = "Program ID") +
          scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), as.factor(ProgramID)])), 
                             aesthetics = c("color", "fill"))
        
        saveRDS(parvYear_bypr, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                 gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                 ifelse(stringr::str_detect(i, "NERR"), paste0("ERR_EDA002_", str_replace(p, "_", ""), "vYear_bypr.rds"), 
                                                        ifelse(stringr::str_detect(i, "NMS"), paste0("MS_EDA002_", str_replace(p, "_", ""), "vYear_bypr.rds"), paste0("AP_EDA002_", str_replace(p, "_", ""), "vYear_bypr.rds"))))))
        
        spvYear_bypr <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = analysisunit, color = as.factor(ProgramID))) +
          geom_jitter() +
          theme_bw() +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
               y = "Species",
               color = "Program ID") +
          scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), as.factor(ProgramID)])), 
                             aesthetics = c("color", "fill"))
        
        saveRDS(spvYear_bypr, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA003_spvYear_bypr.rds", 
                                                       ifelse(stringr::str_detect(i, "NMS"), "MS_EDA003_spvYear_bypr.rds", "AP_EDA003_spvYear_bypr.rds")))))
        
        qsvYear_bysp <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = QuadSize_m2, color = analysisunit)) +
          geom_jitter() +
          theme_bw() +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
               y = "Quadrat size (m^2)",
               color = "Species") +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                             aesthetics = c("color", "fill"))
        
        saveRDS(qsvYear_bysp, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA004_qsvYear_bysp.rds", 
                                                       ifelse(stringr::str_detect(i, "NMS"), "MS_EDA004_qsvYear_bysp.rds", "AP_EDA004_qsvYear_bysp.rds")))))
        
        qsvYear_bypr <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = QuadSize_m2, color = as.factor(ProgramID))) +
          geom_jitter() +
          theme_bw() +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
               y = "Quadrat size (m^2)",
               color = "Program ID") +
          scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), as.factor(ProgramID)])), 
                             aesthetics = c("color", "fill"))
        
        saveRDS(qsvYear_bypr, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA005_qsvYear_bypr.rds", 
                                                       ifelse(stringr::str_detect(i, "NMS"), "MS_EDA005_qsvYear_bypr.rds", "AP_EDA005_qsvYear_bypr.rds")))))
        
        metvYear_bysp <- ggplot(data = SAV4[ManagedAreaName == i, ], aes(x = Year, y = method, color = analysisunit)) +
          geom_jitter() +
          theme_bw() +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
               y = "Method",
               color = "Species") +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                             aesthetics = c("color", "fill"))
        
        saveRDS(metvYear_bysp, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                 gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                 ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA006_metvYear_bysp.rds", 
                                                        ifelse(stringr::str_detect(i, "NMS"), "MS_EDA006_metvYear_bysp.rds", "AP_EDA006_metvYear_bysp.rds")))))
        
        metvYear_bypr <- ggplot(data = SAV4[ManagedAreaName == i, ], aes(x = Year, y = method, color = as.factor(ProgramID))) +
          geom_jitter() +
          theme_bw() +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
               y = "Method",
               color = "Program ID") +
          scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), as.factor(ProgramID)])), 
                             aesthetics = c("color", "fill"))
        
        saveRDS(metvYear_bypr, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                 gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                 ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA007_metvYear_bypr.rds", 
                                                        ifelse(stringr::str_detect(i, "NMS"), "MS_EDA007_metvYear_bypr.rds", "AP_EDA007_metvYear_bypr.rds")))))
        
        metvqs_bysp <- ggplot(data = SAV4[ManagedAreaName == i, ], aes(x = QuadSize_m2, y = method, color = analysisunit)) +
          geom_jitter() +
          theme_bw() +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
               x = "Quadrat size (m^2)",
               y = "Method",
               color = "Species") +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                             aesthetics = c("color", "fill"))
        
        saveRDS(metvqs_bysp, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                               gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                               ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA008_metvqs_bysp.rds", 
                                                      ifelse(stringr::str_detect(i, "NMS"), "MS_EDA008_metvqs_bysp.rds", "AP_EDA008_metvqs_bysp.rds")))))
        
        metvqs_bypr <- ggplot(data = SAV4[ManagedAreaName == i, ], aes(x = QuadSize_m2, y = method, color = as.factor(ProgramID))) +
          geom_jitter() +
          theme_bw() +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
               x = "Quadrat size (m^2)",
               y = "Method",
               color = "Program ID") +
          scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), as.factor(ProgramID)])), 
                             aesthetics = c("color", "fill"))
        
        saveRDS(metvqs_bypr, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                               gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                               ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA009_metvqs_bypr.rds", 
                                                      ifelse(stringr::str_detect(i, "NMS"), "MS_EDA009_metvqs_bypr.rds", "AP_EDA009_metvqs_bypr.rds")))))
        
        if(length(SAV4[ManagedAreaName == i & !is.na(eval(p)) & !is.na(Grid), Grid]) > 0){              
          grvYear_bysp <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = Grid, color = analysisunit)) +
            geom_jitter() +
            theme_bw() +
            labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                                ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
                 y = "Grid number",
                 color = "Species") +
            scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                               aesthetics = c("color", "fill"))
          
          saveRDS(grvYear_bysp, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                  gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                  ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA010_grvYear_bysp.rds", 
                                                         ifelse(stringr::str_detect(i, "NMS"), "MS_EDA010_grvYear_bysp.rds", "AP_EDA010_grvYear_bysp.rds")))))
          
          grvYear_bypr <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = Grid, color = as.factor(ProgramID))) +
            geom_jitter() +
            theme_bw() +
            labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                                ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
                 y = "Grid number",
                 color = "Program ID") +
            scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), as.factor(ProgramID)])), 
                               aesthetics = c("color", "fill"))
          
          saveRDS(grvYear_bypr, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                  gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                  ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA011_grvYear_bypr.rds", 
                                                         ifelse(stringr::str_detect(i, "NMS"), "MS__EDA011_grvYear_bypr.rds", "AP_EDA011_grvYear_bypr.rds")))))
        }
        
        if(length(SAV4[ManagedAreaName == i & !is.na(eval(p)) & !is.na(Depth_M), Depth_M]) > 0){                
          dpvYear_bysp <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = Depth_M, color = analysisunit)) +
            geom_jitter() +
            theme_bw() +
            labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                                ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), "National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
                 y = "Depth (m)",
                 color = "Species") +
            scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                               aesthetics = c("color", "fill"))
          
          saveRDS(dpvYear_bysp, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                  gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                  ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA012_dpvYear_bysp.rds", 
                                                         ifelse(stringr::str_detect(i, "NMS"), "MS_EDA012_dpvYear_bysp.rds", "AP_EDA012_dpvYear_bysp.rds")))))
          
          dpvYear_bypr <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = Depth_M, color = as.factor(ProgramID))) +
            geom_jitter() +
            theme_bw() +
            labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                                ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
                 y = "Depth (m)",
                 color = "Program ID") +
            scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), as.factor(ProgramID)])), 
                               aesthetics = c("color", "fill"))
          
          saveRDS(dpvYear_bypr, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                  gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                  ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA013_dpvYear_bypr.rds", 
                                                         ifelse(stringr::str_detect(i, "NMS"), "MS_EDA013_dpvYear_bypr.rds", "AP_EDA013_dpvYear_bypr.rds")))))
        }
        
        
        #Generate the legend
        plotall <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, fill = analysisunit)) +
          geom_bar()  +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                             aesthetics = c("color", "fill")) +
          labs(y="Frequency of data", x="Year") +
          theme(legend.title = element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1),
                axis.title = element_text(size = 7),
                axis.text = element_text(size = 7),
                legend.text = element_text(size = 7))
        
        legend = gtable::gtable_filter(ggplotGrob(plotall), "guide-box")
        
        saveRDS(legend, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                          gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                          ifelse(stringr::str_detect(i, "NERR"), "ERR_hist_specieslegend.rds", 
                                                 ifelse(stringr::str_detect(i, "NMS"), "MS_hist_specieslegend.rds", "AP_hist_specieslegend.rds")))))
        
        #Create and save the hist objects---------------------------------------------------
        for(a in setdiff(unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit]), c("Total seagrass", "Attached algae", "Drift algae"))){
          dat <- filter(SAV4[ManagedAreaName == i & !is.na(eval(p)), ], analysisunit == a)
          
          plot <- ggplot(data = dat, aes(x = Year, fill = analysisunit)) +
            geom_bar() +
            scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                               aesthetics = c("color", "fill")) +
            scale_x_continuous(limits = c(min(dat$Year - 1), max(dat$Year + 1))) +
            #scale_y_continuous(limits = c(0, 2600)) +
            labs(y="Frequency of data", x="Year") +
            theme(#legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1),
              #axis.title = element_text(size = 7),
              axis.title = element_blank(),
              axis.text = element_text(size = 7),
              #legend.text = element_text(size = 7),
              legend.position = "none")
          
          saveRDS(plot, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                          gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                          ifelse(stringr::str_detect(i, "NERR"), "ERR_hist_", 
                                                 ifelse(stringr::str_detect(i, "NMS"), "MS_hist_", "AP_hist_")), 
                                          gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', a, perl = TRUE), ".rds")))
        }
        
        dat <- filter(SAV4[ManagedAreaName == i & !is.na(eval(p)), ], analysisunit %in% c("Total seagrass", "Attached algae", "Drift algae"))
        
        plot <- ggplot(data = dat, aes(x = Year, fill = analysisunit)) +
          geom_bar() +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(dat$analysisunit)), 
                             aesthetics = c("color", "fill")) +
          scale_x_continuous(limits = c(min(dat$Year - 1), max(dat$Year + 1))) +
          #scale_y_continuous(limits = c(0, 2600)) +
          #paste0(as_label(BBAP_BB_EDAplots[[1]]$mapping$y))
          labs(y="Frequency of data", x="Year") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                #axis.title = element_text(size = 7),
                #axis.title = element_blank(),
                axis.text = element_text(size = 7),
                #legend.text = element_text(size = 7),
                legend.position = "none",
                legend.title = element_blank())
        
        saveRDS(plot, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                        gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                        ifelse(stringr::str_detect(i, "NERR"), "ERR_hist_SGvMA.rds", 
                                               ifelse(stringr::str_detect(i, "NMS"), "MS_hist_SGvMA.rds", "AP_hist_SGvMA.rds")))))  
        
        #Generate the legend
        plotall <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = eval(p), color = analysisunit)) +
          geom_boxplot() +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                             aesthetics = c("color", "fill")) +
          labs(y = parameters[column == p, name], x = "Year") +
          theme(legend.title = element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1),
                axis.title = element_text(size = 7),
                axis.text = element_text(size = 7),
                legend.text = element_text(size = 7))
        
        legend = gtable::gtable_filter(ggplotGrob(plotall), "guide-box")
        
        saveRDS(legend, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                          gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                          ifelse(stringr::str_detect(i, "NERR"), "ERR_boxplot_specieslegend.rds", 
                                                 ifelse(stringr::str_detect(i, "NMS"), "MS_boxplot_specieslegend.rds", "AP_boxplot_specieslegend.rds")))))
        
        #Create and save the boxplot objects--------------------------------------------------
        for(b in setdiff(unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit]), c("Total seagrass", "Attached algae", "Drift algae"))){
          dat <- filter(SAV4[ManagedAreaName == i & !is.na(eval(p)), ], analysisunit == b)
          
          plot <- ggplot(data = dat, aes(group=Year, x = Year, y = eval(p), color = analysisunit)) +
            geom_boxplot() +
            scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                               aesthetics = c("color", "fill")) +
            scale_x_continuous(limits = c(min(dat$Year - 1), max(dat$Year + 1))) +
            #scale_y_continuous(limits = c(0, 100)) +
            labs(y = parameters[column == p, name], x = "Year") +
            theme(#legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1),
              #axis.title = element_text(size = 7),
              axis.title = element_blank(),
              axis.text = element_text(size = 7),
              #legend.text = element_text(size = 7),
              legend.position = "none")
          
          saveRDS(plot, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                          gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                          ifelse(stringr::str_detect(i, "NERR"), "ERR_boxplot_", 
                                                 ifelse(stringr::str_detect(i, "NMS"), "MS_boxplot_", "AP_boxplot_")), 
                                          gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', b, perl = TRUE), ".rds")))
        }
        
        dat <- filter(SAV4[ManagedAreaName == i & !is.na(eval(p)), ], analysisunit %in% c("Total seagrass", "Attached algae", "Drift algae"))
        
        plot <- ggplot(data = dat, aes(x = as.factor(Year), y = eval(p), color = analysisunit)) +
          geom_boxplot() +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(dat$analysisunit)), 
                             aesthetics = c("color", "fill")) +
          #scale_x_continuous(limits = c(min(dat$Year - 1), max(dat$Year + 1))) +
          #scale_y_continuous(limits = c(0, 100)) +
          labs(y = parameters[column == p, name], x = "Year") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                #axis.title = element_text(size = 7),
                #axis.title = element_blank(),
                axis.text = element_text(size = 7),
                #legend.text = element_text(size = 7),
                legend.position = "none",
                legend.title = element_blank())
        
        saveRDS(plot, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                        gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                        ifelse(stringr::str_detect(i, "NERR"), "ERR_boxplot_SGvMA.rds", 
                                               ifelse(stringr::str_detect(i, "NMS"), "MS_boxplot_SGvMA.rds", "AP_boxplot_SGvMA.rds")))))  
      }
      
      if(str_detect(EDA, "maps")){
      
        #Create map(s) for the managed area-------------------------------------------
        
        fl_i <- st_crop(counties, xmin = corners[ManagedAreaName == i, xmin], xmax = corners[ManagedAreaName == i, xmax], ymin = corners[ManagedAreaName == i, ymin], ymax = corners[ManagedAreaName == i, ymax])
        # fl_i2 <- ggplot() +
        #   geom_sf(data = fl_i, fill = "beige", color = "navajowhite3", lwd = 0.5, inherit.aes = FALSE)
        # fl_i <- fl_i +
        #   annotation_scale(
        #     location = "tl",
        #     bar_cols = c("grey60", "white"),
        #     text_family = "Arial") #+
        #   annotation_north_arrow(
        #     location = "tr", 
        #     which_north = "true",
        #     pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
        #     style = north_arrow_nautical(
        #       fill = c("grey40", "white"),
        #       line_col = "grey20",
        #       text_family = "Arial"
        #     )
        #   )
        
        rcp_i <- subset(rcp, rcp$LONG_NAME == ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                                                     ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), 
                                                            ifelse(str_detect(i, "Fort Clinch|Fort Pickens|Rocky Bayou|St. Andrews"), paste0(i, " State Park Aquatic Preserve"), paste0(i, " Aquatic Preserve")))))
        
        
        #create scalebar and north arrow (https://stackoverflow.com/questions/34183049/plot-circle-with-a-certain-radius-around-point-on-a-map-in-ggplot2)
        if(corners[ManagedAreaName == i, Coast[1]] == "Atlantic"){
          wkm <- (abs(st_bbox(fl_i)[[4]]) - abs(st_bbox(fl_i)[[2]])) * (40075 / 360) #* cos((st_bbox(fl_i)[[2]] + (abs(st_bbox(fl_i)[[4]]) - abs(st_bbox(fl_i)[[2]]))/2)/57.3)
          lonkm <- ifelse(wkm < 20, 3 / (40075 / 360), ifelse(wkm < 50, 5 / (40075 / 360),
                          10 / (40075 / 360)))
        } else{
          wkm <- (abs(st_bbox(fl_i)[[1]]) - abs(st_bbox(fl_i)[[3]])) * (40075 / 360) * cos((st_bbox(fl_i)[[2]] + (abs(st_bbox(fl_i)[[4]]) - abs(st_bbox(fl_i)[[2]]))/2)/57.3)
          lonkm <- ifelse(wkm < 20, 3 / ((40075 / 360) * cos((st_bbox(fl_i)[[2]] + (abs(st_bbox(fl_i)[[4]]) - abs(st_bbox(fl_i)[[2]]))/2)/57.3)), ifelse(wkm < 50, 5 / ((40075 / 360) * cos((st_bbox(fl_i)[[2]] + (abs(st_bbox(fl_i)[[4]]) - abs(st_bbox(fl_i)[[2]]))/2)/57.3)),
                          10 / ((40075 / 360) * cos((st_bbox(fl_i)[[2]] + (abs(st_bbox(fl_i)[[4]]) - abs(st_bbox(fl_i)[[2]]))/2)/57.3))))
        }
        
        # sbar <- st_linestring(x = matrix(c(st_bbox(fl_i)[[1]], st_bbox(fl_i)[[1]] + lon5km, st_bbox(fl_i)[[2]], st_bbox(fl_i)[[2]]), 2, 2), dim = "XYZ")
        # sbar <- sfheaders::sf_linestring(obj = matrix(c(st_bbox(fl_i)[[1]], st_bbox(fl_i)[[1]] + lon5km, st_bbox(fl_i)[[2]], st_bbox(fl_i)[[2]]), 2, 2), x = 1, y = 2)
        fl_i_bbox <- st_bbox(fl_i)
        rcp_i_bbox <- st_bbox(rcp_i)
        min_x <- min(fl_i_bbox$xmin, rcp_i_bbox$xmin)
        max_x <- max(fl_i_bbox$xmax, rcp_i_bbox$xmax)
        min_y <- min(fl_i_bbox$ymin, rcp_i_bbox$ymin)
        max_y <- max(fl_i_bbox$ymax, rcp_i_bbox$ymax)
        
        if(corners[ManagedAreaName == i, Coast[1]] == "Atlantic"){
          sbar <- data.table(x = max_x,
                             y = c(min_y, min_y + lonkm))#c(min_y - (abs(max_y) - abs(min_y)) * 0.15, min_y - (abs(max_y) - abs(min_y)) * 0.15))
          
          x_sbarpos1 <- (abs(min_x) - abs(max_x)) * 0.6 
          x_sbarpos2 <- (abs(min_x) - abs(max_x)) * 0.2 
          y_sbarpos1 <- ifelse(((abs(max_y) - abs(min_y)) * 0.6) >= lonkm, 
                               (abs(max_y) - abs(min_y)) * 0.6,
                               (lonkm) + ((abs(max_y) - abs(min_y)) * 0.1)) 
          y_sbarpos2 <- ifelse(((abs(max_y) - abs(min_y)) * 0.1) >= lonkm, 
                               (abs(max_y) - abs(min_y)) * 0.1,
                               (lonkm) + ((abs(max_y) - abs(min_y)) * 0.1)) 
          
        } else{
          sbar <- data.table(x = c(min_x, min_x + lonkm),
                             y = min_y)#c(min_y - (abs(max_y) - abs(min_y)) * 0.15, min_y - (abs(max_y) - abs(min_y)) * 0.15))
        
          x_sbarpos1 <- ifelse(((abs(min_x) - abs(max_x)) * 0.6) >= lonkm, 
                               ((abs(min_x) - abs(max_x)) * 0.6),
                               (lonkm) + ((abs(min_x) - abs(max_x)) * 0.1))
          x_sbarpos2 <- ifelse(((abs(min_x) - abs(max_x)) * 0.2) >= lonkm, 
                               ((abs(min_x) - abs(max_x)) * 0.2),
                               (lonkm) + ((abs(min_x) - abs(max_x)) * 0.1))
          y_sbarpos1 <- (abs(max_y) - abs(min_y)) * 0.6
          y_sbarpos2 <- (abs(max_y) - abs(min_y)) * 0.1
            
        }
        
        
        # x_sbarpos1 <- ifelse(((abs(min_x) - abs(max_x)) * 0.6) >= lonkm, 
        #                      ((abs(min_x) - abs(max_x)) * 0.6),
        #                      (lonkm) + ((abs(min_x) - abs(max_x)) * 0.1)) #/(lonkm/(max_x - min_x))
        # x_sbarpos2 <- ifelse(((abs(min_x) - abs(max_x)) * 0.2) >= lonkm, 
        #                      ((abs(min_x) - abs(max_x)) * 0.2),
        #                      (lonkm) + ((abs(min_x) - abs(max_x)) * 0.1)) #/(lonkm/(max_x - min_x))
        # y_sbarpos1 <- ifelse(((abs(max_y) - abs(min_y)) * 0.6) >= lonkm, 
        #                      (abs(max_y) - abs(min_y)) * 0.6,
        #                      (lonkm) + ((abs(max_y) - abs(min_y)) * 0.1)) #/(lonkm/(max_y - min_y))
        # y_sbarpos2 <- ifelse(((abs(max_y) - abs(min_y)) * 0.1) >= lonkm, 
        #                      (abs(max_y) - abs(min_y)) * 0.1,
        #                      (lonkm) + ((abs(max_y) - abs(min_y)) * 0.1)) #/(lonkm/(max_y - min_y))
        
        sbar[, `:=` (x = fcase(corners[ManagedAreaName == i, Coast[1]] == "Gulf", x + x_sbarpos2, #(abs(min_x) - abs(max_x)) * ((lonkm/(max_x - min_x)) * x_sbarpos2),
                               corners[ManagedAreaName == i, Coast[1]] == "Panhandle", x + x_sbarpos1, #(abs(min_x) - abs(max_x)) * ((lonkm/(max_x - min_x)) * x_sbarpos1),
                               i %in% c("Banana River", "Indian River-Malabar to Vero Beach", "Indian River-Vero Beach to Ft. Pierce", "Jensen Beach to Jupiter Inlet", 
                                        "Mosquito Lagoon"), x + x_sbarpos2, #(abs(min_x) - abs(max_x)) * ((lonkm/(max_x - min_x)) * x_sbarpos2),
                               corners[ManagedAreaName == i, Coast[1]] == "Atlantic", x + x_sbarpos2), #(abs(min_x) - abs(max_x)) * ((lonkm/(max_x - min_x)) * x_sbarpos2)),
                     y = fcase(corners[ManagedAreaName == i, Coast[1]] == "Gulf", y - y_sbarpos2, #(abs(max_y) - abs(min_y)) * ((lonkm/(max_y - min_y)) * y_sbarpos2),
                               corners[ManagedAreaName == i, Coast[1]] == "Panhandle", y + y_sbarpos2, #(abs(max_y) - abs(min_y)) * ((lonkm/(max_y - min_y)) * y_sbarpos2),
                               i %in% c("Banana River", "Indian River-Malabar to Vero Beach", "Indian River-Vero Beach to Ft. Pierce", "Jensen Beach to Jupiter Inlet", 
                                        "Mosquito Lagoon"), y + y_sbarpos1, #(abs(max_y) - abs(min_y)) * ((lonkm/(max_y - min_y)) * y_sbarpos1),
                               corners[ManagedAreaName == i, Coast[1]] == "Atlantic", y + y_sbarpos1)),#(abs(max_y) - abs(min_y)) * ((lonkm/(max_y - min_y)) * y_sbarpos1))), 
             by = list(row.names(sbar))]

        sbar <- st_as_sf(sbar, coords = c("x", "y"), crs = 4326)
        sbar <- st_combine(sbar)
        sbar <- st_cast(sbar, "LINESTRING")
        sbar <- st_sf(sbar)
        st_geometry(sbar) <- "geometry"
        
        if(corners[ManagedAreaName == i, Coast[1]] == "Atlantic"){
          sbarlab <- data.table(x = st_bbox(sbar)$xmin + (abs(min_x) - abs(max_x)) * 0.15,
                                y = st_bbox(sbar)$ymin + lonkm/3) #min_y)
        } else {
          sbarlab <- data.table(x = st_bbox(sbar)$xmin + lonkm/2,
                                y = st_bbox(sbar)$ymin - (abs(max_y) - abs(min_y)) * 0.1) #min_y)
        }
        
        # sbarlab[, `:=` (x = fcase(corners[ManagedAreaName == i, Coast[1]] == "Gulf", x + x_sbarpos2, #(abs(min_x) - abs(max_x)) * ((lonkm/(max_x - min_x)) * x_sbarpos2),
        #                           corners[ManagedAreaName == i, Coast[1]] == "Panhandle", x + x_sbarpos1, #(abs(min_x) - abs(max_x)) * ((lonkm/(max_x - min_x)) * x_sbarpos1),
        #                           i %in% c("Banana River", "Indian River-Malabar to Vero Beach", "Indian River-Vero Beach to Ft. Pierce", "Jensen Beach to Jupiter Inlet", 
        #                                    "Mosquito Lagoon"), x + x_sbarpos2, #(abs(min_x) - abs(max_x)) * ((lonkm/(max_x - min_x)) * x_sbarpos2),
        #                           corners[ManagedAreaName == i, Coast[1]] == "Atlantic", x + x_sbarpos2), #(abs(min_x) - abs(max_x)) * ((lonkm/(max_x - min_x)) * x_sbarpos2)),
        #                 y = fcase(corners[ManagedAreaName == i, Coast[1]] == "Gulf", y - y_sbarpos2, #(abs(max_y) - abs(min_y)) * ((lonkm/(max_y - min_y)) * y_sbarpos2),
        #                           corners[ManagedAreaName == i, Coast[1]] == "Panhandle", y - y_sbarpos2, #(abs(max_y) - abs(min_y)) * ((lonkm/(max_y - min_y)) * y_sbarpos2),
        #                           i %in% c("Banana River", "Indian River-Malabar to Vero Beach", "Indian River-Vero Beach to Ft. Pierce", "Jensen Beach to Jupiter Inlet", 
        #                                    "Mosquito Lagoon"), y + y_sbarpos1, #(abs(max_y) - abs(min_y)) * ((lonkm/(max_y - min_y)) * y_sbarpos1),
        #                           corners[ManagedAreaName == i, Coast[1]] == "Atlantic", y + y_sbarpos1))] #(abs(max_y) - abs(min_y)) * ((lonkm/(max_y - min_y)) * y_sbarpos1)))]
        # 
        sbarlab <- st_as_sf(sbarlab, coords = c("x", "y"), crs = 4326)
        
        
        if(corners[ManagedAreaName == i, Coast[1]] == "Atlantic"){
          narrow <- data.table(x = c(st_bbox(sbar)$xmin,
                                     st_bbox(sbar)$xmin,
                                     st_bbox(sbar)$xmin,
                                     st_bbox(sbar)$xmin + (abs(min_x) - abs(max_x)) * 0.055,
                                     st_bbox(sbar)$xmin,
                                     st_bbox(sbar)$xmin - (abs(min_x) - abs(max_x)) * 0.055),
                               y = c((st_bbox(sbar)$ymin - ((abs(max_y) - abs(min_y)) * 0.15)),
                                     (st_bbox(sbar)$ymin - ((abs(max_y) - abs(min_y)) * 0.075)),
                                     (st_bbox(sbar)$ymin - ((abs(max_y) - abs(min_y)) * 0.075)),
                                     (st_bbox(sbar)$ymin - ((abs(max_y) - abs(min_y)) * 0.075)) - (abs(min_x) - abs(max_x)) * 0.065,
                                     (st_bbox(sbar)$ymin - ((abs(max_y) - abs(min_y)) * 0.075)),
                                     (st_bbox(sbar)$ymin - ((abs(max_y) - abs(min_y)) * 0.075)) - (abs(min_x) - abs(max_x)) * 0.065))

        } else{
          narrow <- data.table(x = c((st_bbox(sbar)$xmin - ((abs(min_x) - abs(max_x)) * 0.075)), 
                                     (st_bbox(sbar)$xmin - ((abs(min_x) - abs(max_x)) * 0.075)), 
                                     (st_bbox(sbar)$xmin - ((abs(min_x) - abs(max_x)) * 0.075)), 
                                     (st_bbox(sbar)$xmin - ((abs(min_x) - abs(max_x)) * 0.075)) - (abs(max_y) - abs(min_y)) * 0.05, 
                                     (st_bbox(sbar)$xmin - ((abs(min_x) - abs(max_x)) * 0.075)), 
                                     (st_bbox(sbar)$xmin - ((abs(min_x) - abs(max_x)) * 0.075)) + (abs(max_y) - abs(min_y)) * 0.05),
                               y = c(st_bbox(sbar)$ymin, 
                                     # min_y - (abs(max_y) - abs(min_y)) * 0.15, 
                                     st_bbox(sbar)$ymin + (abs(max_y) - abs(min_y)) * 0.15,
                                     st_bbox(sbar)$ymin + (abs(max_y) - abs(min_y)) * 0.15,
                                     # min_y,
                                     (st_bbox(sbar)$ymin + (abs(max_y) - abs(min_y)) * 0.15) - (abs(max_y) - abs(min_y)) * 0.065,
                                     # min_y - (abs(max_y) - abs(min_y)) * 0.065, 
                                     st_bbox(sbar)$ymin + (abs(max_y) - abs(min_y)) * 0.15,
                                     # min_y, 
                                     # min_y - (abs(max_y) - abs(min_y)) * 0.065,
                                     (st_bbox(sbar)$ymin + (abs(max_y) - abs(min_y)) * 0.15) - (abs(max_y) - abs(min_y)) * 0.065))
          
        }

        narrow <- st_as_sf(narrow, coords = c("x", "y"), crs = 4326)
        narrow <- st_combine(narrow)
        narrow <- st_cast(narrow, "LINESTRING")
        narrow <- st_sf(narrow)
        st_geometry(narrow) <- "geometry"
        
        if(corners[ManagedAreaName == i, Coast[1]] == "Atlantic"){
          narlab <- data.table(x = st_bbox(sbarlab)$xmin,
                               y = st_bbox(narrow)$ymin) #+ (abs(st_bbox(narrow)$ymax) - abs(st_bbox(narrow)$ymin)) / 5)
        
        } else{
          narlab <- data.table(x = st_bbox(narrow)$xmin + (abs(st_bbox(narrow)$xmin) - abs(st_bbox(narrow)$xmax)) / 2,
                               y = st_bbox(sbarlab)$ymin)
          
        }
        # narlab[, `:=` (x = ifelse(corners[ManagedAreaName == i, Coast[1]] == "Gulf", x + (abs(min_x) - abs(max_x)) * 0.05,
        #                           x + (abs(min_x) - abs(max_x)) * 0.91),
        #                y = ifelse(corners[ManagedAreaName == i, Coast[1]] == "Gulf", y + (abs(max_y) - abs(min_y)) * 0.005,
        #                           y + (abs(max_y) - abs(min_y)) * 0.005))]
        narlab <- st_as_sf(narlab, coords = c("x", "y"), crs = 4326)
        
        
        locs_pts_rcp_i <- locs_pts_rcp[rcp_i, , op = st_intersects]
        locs_lns_rcp_i <- locs_lns_rcp[rcp_i, , op = st_intersects]
        
        yadd <- 0
        startyear <- min(SAV4[ManagedAreaName == i & !is.na(eval(p)), Year])
        base <- ggplot() +
          geom_sf(data = rotate_sf(fl_i, ma = i, coast = corners[ManagedAreaName == i, Coast[1]]), fill = "beige", color = "navajowhite3", lwd = 0.5, inherit.aes = FALSE) +
          geom_sf(data = rotate_sf(rcp_i, ma = i, coast = corners[ManagedAreaName == i, Coast[1]]), color = "grey50", fill = "powderblue", alpha = 0.35, lwd = 0.5, inherit.aes = FALSE) +
          geom_sf(data = rotate_sf(sbar, ma = i, coast = corners[ManagedAreaName == i, Coast[1]]), color = "grey50", linewidth = 1.25, inherit.aes = FALSE) +
          geom_sf(data = rotate_sf(narrow, ma = i, coast = corners[ManagedAreaName == i, Coast[1]]), color = "grey50", linewidth = 1, inherit.aes = FALSE) +
          geom_sf_text(data = rotate_sf(sbarlab, ma = i, coast = corners[ManagedAreaName == i, Coast[1]]), label = ifelse(wkm < 20, "3 km", ifelse(wkm < 50, "5 km", "10 km")), hjust = 0.5, angle = 4, color = "grey50", size = 3.5, inherit.aes = FALSE) +
          geom_sf_text(data = rotate_sf(narlab, ma = i, coast = corners[ManagedAreaName == i, Coast[1]]), label = "N", hjust = 0.7, angle = 4, color = "grey50", size = 3.5, inherit.aes = FALSE) +
          # geom_text(data = sbarlab, 
          #           aes(geometry = geometry, stat(X), stat(Y)),
          #           label = "5 km",
          #           stat = StatSfCoordinates,
          #           # fun.geometry = rotate_sf,
          #           size = 10) +
          # annotation_scale(
          #   location = "tl",
          #   bar_cols = c("grey60", "white"),
          #   text_family = "Arial") +
          # annotation_north_arrow(
          #   location = "tl", 
          #   which_north = "true",
          #   pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
          #   style = north_arrow_nautical(
          #     fill = c("grey40", "white"),
          #     line_col = "grey20",
          #     text_family = "Arial"
          #   )
          # ) +
          scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), ProgramName])), 
                             aesthetics = c("color", "fill")) +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))), 
               fill = "Program name", color = "Program name") +
          theme(panel.grid.major = element_line(colour = NA),
                panel.grid.minor = element_line(colour = NA),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                axis.title = element_blank(),
                panel.background = element_rect(fill = NA),
                plot.background = element_rect(colour = NA),
                legend.position = "bottom",
                legend.direction = "vertical")
        ystart <- ifelse(corners[ManagedAreaName == i, Coast[1]] == "Atlantic", attributes(base$layers[[2]]$data$geometry)$bbox$ymax[[1]], attributes(base$layers[[2]]$data$geometry)$bbox$ymin[[1]])
        xlab <- attributes(base$layers[[2]]$data$geometry)$bbox$xmax[[1]] + (attributes(base$layers[[2]]$data$geometry)$bbox$xmax[[1]] - attributes(base$layers[[2]]$data$geometry)$bbox$xmin[[1]])/50
        MAcoords <- setDT(as.data.frame(st_coordinates(rcp_i)))
        maxdist <- max(st_distance(st_as_sf(MAcoords[X == min(X), ], coords = c("X", "Y"), crs = 4326), st_as_sf(MAcoords[Y == max(Y), ], coords = c("X", "Y"), crs = 4326)),
                       st_distance(st_as_sf(MAcoords[X == max(X), ], coords = c("X", "Y"), crs = 4326), st_as_sf(MAcoords[Y == min(Y), ], coords = c("X", "Y"), crs = 4326)),
                       st_distance(st_as_sf(MAcoords[X == min(X), ], coords = c("X", "Y"), crs = 4326), st_as_sf(MAcoords[X == max(X), ], coords = c("X", "Y"), crs = 4326)),
                       st_distance(st_as_sf(MAcoords[Y == min(Y), ], coords = c("X", "Y"), crs = 4326), st_as_sf(MAcoords[Y == max(Y), ], coords = c("X", "Y"), crs = 4326)))
        area <- st_area(rcp_i)
        xyratio <- as.numeric((area/maxdist)/maxdist)
        
        MApolycoords <- setDT(as.data.frame(st_coordinates(base$layers[[2]]$data)))
        xmax_y <- MApolycoords[X == max(X), Y]
        base <- base + annotate("text", x = xlab, y = xmax_y, label = paste0(startyear), hjust = "left")
        
        MApolycoords[, Xrnd := round(X, 3)][, ydists := max(Y) - min(Y), by = Xrnd]
        maxydist <- max(MApolycoords$ydists) + ((max(MApolycoords$ydists)/25) / xyratio) 
        
        if(length(subset(locs_pts_rcp_i, locs_pts_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year == startyear, LocationID]))$LocationID) > 0){
          base <- base +
            geom_sf(data = rotate_sf(subset(locs_pts_rcp_i, locs_pts_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year == startyear, LocationID])),
                                     ma = i, coast = corners[ManagedAreaName == i, Coast[1]]),
                    aes(fill = droplevels(as.factor(ProgramName))), shape = 21, color = "black")
        }
        
        if(length(subset(locs_lns_rcp_i, locs_lns_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year == startyear, LocationID]))$LocationID) > 0){
          base <- base +
            geom_sf(data = rotate_sf(subset(locs_lns_rcp_i, locs_lns_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year == startyear, LocationID])),
                                     ma = i, coast = corners[ManagedAreaName == i, Coast[1]]),
                    aes(color = droplevels(as.factor(ProgramName))), shape = 21)
        }
        
        for(y in sort(unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year != startyear, Year]))){
          base <- base +
            geom_sf(data = rotate_sf(rcp_i, y_add = yadd + maxydist, ma = i, coast = corners[ManagedAreaName == i, Coast[1]]), 
                    color = "grey50", fill = "powderblue", alpha = 0.65, lwd = 0.5, inherit.aes = FALSE) +
            annotate("text", x = xlab, y = xmax_y + yadd + maxydist, label = y, hjust = "left")
          
          if(length(subset(locs_pts_rcp_i, locs_pts_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year == y, LocationID]))$LocationID) > 0){
            base <- base +
              geom_sf(data = rotate_sf(subset(locs_pts_rcp_i, locs_pts_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year == y, LocationID])),
                                       y_add = yadd + maxydist, ma = i, coast = corners[ManagedAreaName == i, Coast[1]]), 
                      aes(fill = droplevels(as.factor(ProgramName))), shape = 21, color = "black")
          }
          
          if(length(subset(locs_lns_rcp_i, locs_lns_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year == startyear, LocationID]))$LocationID) > 0){
            base <- base +
              geom_sf(data = rotate_sf(subset(locs_lns_rcp_i, locs_lns_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year == startyear, LocationID])),
                                       y_add = yadd + maxydist, ma = i, coast = corners[ManagedAreaName == i, Coast[1]]),
                      aes(color = droplevels(as.factor(ProgramName))), shape = 21)
          }
          
          yadd <- yadd + maxydist
          startyear <- startyear + 1
          ystart <- ystart + maxydist
        }        
        
        saveRDS(base, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                        gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE),
                                        ifelse(stringr::str_detect(i, "NERR"), "ERR_map_bypr.rds", 
                                               ifelse(stringr::str_detect(i, "NMS"), "MS_map_bypr.rds", "AP_map_bypr.rds")))))
        
        #Save image file versions of the maps
        # nlayers <- 0
        # for(k in seq_along(base$layers)){
        #   class_k <- class(base$layers[[k]])[2]
        #   if(class_k == 'Layer'){
        #     nlayers <- nlayers + 1
        #   } else{
        #     next
        #   }
        # }
        
        base <- base +
          theme(legend.position='bottom', 
                legend.justification='left',
                legend.direction='vertical')
        
        plotbuild <- ggplot_build(base)
        hwratio <- (plotbuild$layout$panel_scales_y[[1]]$range$range[2] - plotbuild$layout$panel_scales_y[[1]]$range$range[1]) / (plotbuild$layout$panel_scales_x[[1]]$range$range[2] - plotbuild$layout$panel_scales_x[[1]]$range$range[1])
        pwidth <- 6
        
        ggsave(filename = here::here(paste0("SAV/output/Figures/BB/img/SAV_", parameters[column == p, type], "_", 
                                            gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE),
                                            ifelse(stringr::str_detect(i, "NERR"), "ERR_map_bypr.jpg", 
                                                   ifelse(stringr::str_detect(i, "NMS"), "MS_map_bypr.jpg", "AP_map_bypr.jpg")))), 
               plot = base,
               width = pwidth, #6.1,
               #height = 8 + nlayers - 5,
               height = pwidth * hwratio, #yadd/maxydist,
               units = "in",
               dpi = 300,
               limitsize = FALSE)
      }  
    }
    
    if("none" %in% Analyses){
      if(p == parameters$column[length(parameters$column)] & i == ma_include[length(ma_include)]){
        toc()
      }
      next
    } 
    
    if(i %in% ma_halspp){
      species <- subset(nyears, nyears$ManagedAreaName == i & nyears$nyr >= 5 & analysisunit %in% c("Attached algae", "Drift algae", "Halophila spp.", "Manatee grass", 
                                                                                                    "Shoal grass", "Total seagrass", "Total SAV", "Turtle grass", 
                                                                                                    "Widgeon grass"))$analysisunit
    } else{
      species <- subset(nyears, nyears$ManagedAreaName == i & nyears$nyr >= 5 & analysisunit %in% c("Attached algae", "Drift algae", "Unidentified Halophila", 
                                                                                                    "Johnson's seagrass", "Manatee grass", "Paddle grass", 
                                                                                                    "Shoal grass", "Star grass", "Total seagrass", "Total SAV", 
                                                                                                    "Turtle grass", "Widgeon grass"))$analysisunit
    }
    
    models <- c()
    
    #Create data.tables to hold model results for managed area i----------------------------------------------------
    lmemodresults <- data.table(managed_area = character(),
                                species = character(),
                                filename = character(),
                                effect = character(),
                                group = character(),
                                term = character(),
                                estimate = numeric(),
                                std.error = numeric(),
                                df = numeric(),
                                statistic = numeric(),
                                p.value = numeric())
    
    # olrmodresults <- data.table(managed_area = character(),
    #                             species = character(),
    #                             filename = character(),
    #                             effect = character(),
    #                             component = character(),
    #                             group = character(),
    #                             term = character(),
    #                             estimate = numeric(),
    #                             std.error = numeric(),
    #                             conf.low = numeric(),
    #                             conf.high = numeric())
    # 
    # blrmodresults <- data.table(managed_area = character(),
    #                             species = character(),
    #                             filename = character(),
    #                             effect = character(),
    #                             component = character(),
    #                             group = character(),
    #                             term = character(),
    #                             estimate = numeric(),
    #                             std.error = numeric(),
    #                             conf.low = numeric(),
    #                             conf.high = numeric())
    # 
    # belrmodresults <- data.table(managed_area = character(),
    #                              species = character(),
    #                              filename = character(),
    #                              effect = character(),
    #                              component = character(),
    #                              group = character(),
    #                              term = character(),
    #                              estimate = numeric(),
    #                              std.error = numeric(),
    #                              conf.low = numeric(),
    #                              conf.high = numeric())
    
    #In case model doesn't converge on the first try, attempt each model up to 5 times before moving on
    for(j in species){
      
      cat(paste0("\n  Starting species: ", j, "\n"))
      
      if(paste0(p) %in% c("BB_pct", "PC") & ("BB_pct" %in% Analyses | "PC" %in% Analyses)){
        
        formula_j <- as.formula(paste0(p, " ~ relyear"))
        
        set.seed(seed + n)
        if(j %in% setdiff(unique(SAV4$analysisunit_halid), unique(SAV4$analysisunit))){
          model_j <- try(lme(formula_j,
                             random = list(SiteIdentifier = ~relyear),
                             control = list(msMaxIter = 1000, msMaxEval = 1000, sing.tol=1e-20),
                             na.action = na.omit, 
                             data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit_halid == j, ]), 
                         silent = TRUE)
          n <- n + 1
          x <- 0
          
          while(class(model_j) == "try-error" & x < 5){
            if(x %% 25 == 0) print(paste0("    Model failed, starting attempt ", x, " of 5"))
            
            set.seed(seed + n)
            model_j <- try(lme(formula_j,
                               random = list(SiteIdentifier = ~relyear),
                               control = list(msMaxIter = 1000, msMaxEval = 1000, sing.tol=1e-20),
                               na.action = na.omit, 
                               data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit_halid == j, ]),
                           silent = TRUE)
            n <- n + 1
            x <- x + 1
          }
        } else{
          model_j <- try(lme(formula_j,
                             random = list(SiteIdentifier = ~relyear),
                             control = list(msMaxIter = 1000, msMaxEval = 1000, sing.tol=1e-20),
                             na.action = na.omit, 
                             data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == j, ]), 
                         silent = TRUE)
          n <- n + 1
          x <- 0
          
          while(class(model_j) == "try-error" & x < 5){
            if(x %% 25 == 0) print(paste0("    Model failed, starting attempt ", x, " of 5"))
            
            set.seed(seed + n)
            model_j <- try(lme(formula_j,
                               random = list(SiteIdentifier = ~relyear),
                               control = list(msMaxIter = 1000, msMaxEval = 1000, sing.tol=1e-20),
                               na.action = na.omit, 
                               data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == j, ]),
                           silent = TRUE)
            n <- n + 1
            x <- x + 1
          }
        }
        
        
        #Individual model objects are needed for plotting all species together
        eval(call("<-", as.name(paste0(gsub('\\b(\\pL)\\pL{2,}|.', '\\U\\1', i, perl = TRUE), 
                                       "_", 
                                       gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE))), 
                  model_j))
        
        #Save the model object as .rds
        saveRDS(model_j, here::here(paste0("SAV/output/models/SAV_", parameters[column == p, type], "_", 
                                           gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                           ifelse(stringr::str_detect(i, "NERR"), "ERR_lme_", 
                                                  ifelse(stringr::str_detect(i, "NMS"), "MS_lme_", "AP_lme_")), 
                                           gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE), 
                                           ".rds")))
        
        print(paste0("  Model object saved: ", 
                     gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                     "_", 
                     gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE)))
        
        #record lme model results------------------------------------------------------
        if(class(try(eval(as.name(paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "_", gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE)))), silent = TRUE)) != "try-error"){
          models <- append(models, as.name(paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "_", gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE))))
          modj_i <- setDT(broom.mixed::tidy(eval(as.name(paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "_", gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE))))))
          modj_i[, `:=` (managed_area = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                                               ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
                         species = j,
                         filename = paste0("SAV_", parameters[column == p, type], "_", gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                           ifelse(stringr::str_detect(i, "NERR"), "ERR_lme_", 
                                                  ifelse(stringr::str_detect(i, "NMS"), "MS_lme_", "AP_lme_")), 
                                           gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE), ".rds"))]
          lmemodresults <- rbind(lmemodresults, modj_i)
          
        } else{
          failedmod <- data.table(model = paste0("SAV_", parameters[column == p, type], "_",
                                                 gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE),
                                                 ifelse(stringr::str_detect(i, "NERR"), "ERR_lme_", 
                                                        ifelse(stringr::str_detect(i, "NMS"), "MS_lme_", "AP_lme_")), 
                                                 gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE), ".rds"),
                                  error = model_j[1])
          
          failedmods <- rbind(failedmods, failedmod)
          
          modj_i <- data.table(managed_area = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                                                     ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
                               species = j,
                               filename = paste0("SAV_", parameters[column == p, type], "_", gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                 ifelse(stringr::str_detect(i, "NERR"), "ERR_lme_", 
                                                        ifelse(stringr::str_detect(i, "NMS"), "MS_lme_", "AP_lme_")), 
                                                 gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE), ".rds"),
                               effect = NA,
                               group = NA,
                               term = NA,
                               estimate = NA,
                               std.error = NA,
                               df = NA,
                               statistic = NA,
                               p.value = NA)
          lmemodresults <- rbind(lmemodresults, modj_i)
        }
      }
      
      #Indicator == "BB_all"------------------------------------------------------
      if(paste0(p) == "BB_all" & "BB_all" %in% Analyses) next
      
      #Indicator == "PO"--------------------------------------------------------
      if(paste0(p) == "PO" & "PO" %in% Analyses) next #Temporarily blocking the percent occurrence analyses because the binomial model doesn't seem to fit the data very well. Will probably have to figure something else out.
      # if(paste0(p) == "PO"){
      #   #I think binomial logistic regression is the best fit for the percent cover data (0/1 outcomes x 100 "trials" for each quad)
      # 
      #   #set.seed(seed + n)
      #   # datlist <- split(POdat[ManagedAreaName == i & analysisunit == j, ], by = "Ind250")
      # 
      #   # ppctest <- try(brm_multiple(formula = CoverObs ~ relyear + (1 | LocationID), data = datlist,
      #   #                    family = bernoulli, prior = set_prior("normal(0,1)", class = "b"), cores = 4,
      #   #                    control = list(adapt_delta = 0.8, max_treedepth = 10), iter = 9000, warmup = 3000,
      #   #                    chains = 5, inits = 0, thin = 3, sample_prior = "only",
      #   #                    file = here::here(paste0("SAV/output/models/SAV_", parameters[column == p, type], "_",
      #   #                                             gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE),
      #   #                                             ifelse(stringr::str_detect(i, "NERR"), "ERR_blr_priorpc_", 
      #   #                                                    ifelse(stringr::str_detect(i, "NMS"), "MS_blr_priorpc_", "AP_blr_priorpc_")),
      #   #                                             gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),                                         gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1))),
      #   #                                             ".rds"))),
      #   #                silent = TRUE)
      #   
      #   SAV4[, Grid := as.integer(Grid)]
      #   SAV4[, CoverObs := as.integer((PO/100)*Grid)]
      #   #SAV4[, LocMaYrMoQiCi := paste0(LocationID, gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', ManagedAreaName, perl = TRUE), Year, Month, QuadIdentifier, gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', CommonIdentifier, perl = TRUE))]
      #   
      #   #Prior doesn't seem to make a difference for some reason; I am still investigating, but left this in here anyway to catch failed models.
      #   ppctest <- try(brm(formula = CoverObs | trials(Grid) ~ relyear + (1 + relyear | LocationID), 
      #                      data = SAV4[ManagedAreaName == i & analysisunit == j & ProgramID != 10001, ], 
      #                      family = binomial, prior = c(set_prior("normal(0, 200)")), cores = 4, chains = 4, 
      #                      control = list(adapt_delta = 0.8, max_treedepth = 10), iter = 3000, warmup = 1000, inits = 0, 
      #                      thin = 3, seed = seed + n, sample_prior = "only", backend = "cmdstanr", threads = threading(2),
      #                      file = here::here(paste0("SAV/output/models/SAV_", parameters[column == p, type], "_", 
      #                                               gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
      #                                               ifelse(stringr::str_detect(i, "NERR"), "ERR_blr_priorpc_", 
      #                                                      ifelse(stringr::str_detect(i, "NMS"), "MS_blr_priorpc_", "AP_blr_priorpc_")), 
      #                                               gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),                                         gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1))), 
      #                                               ".rds"))), 
      #                  silent = TRUE)
      #   
      #   n <- n + 1
      #   
      #   if(class(ppctest) == "try-error"){
      #     failedmod <- data.table(model = paste0("SAV_", parameters[column == p, type], "_", 
      #                                            gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
      #                                            ifelse(stringr::str_detect(i, "NERR"), "ERR_blr_priorpc_", 
      #                                                   ifelse(stringr::str_detect(i, "NMS"), "MS_blr_priorpc_", "AP_blr_priorpc_")), 
      #                                            gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),                                         gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1))), 
      #                                            ".rds"),
      #                             error = ppctest[1])
      #     
      #     failedmods <- rbind(failedmods, failedmod)
      #     
      #     blrmodj_i <- data.table(managed_area = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
      #                                                   ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
      #                             species = j,
      #                             filename = paste0("SAV_", parameters[column == p, type], "_", gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), ifelse(stringr::str_detect(i, "NERR"), paste0("ERR_blr_", gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),                                         gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1))), ".rds"), paste0("AP_blr_", gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),                                         gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1))), ".rds"))),
      #                             effect = NA,
      #                             component = NA,
      #                             group = NA,
      #                             term = NA,
      #                             estimate = NA,
      #                             std.error = NA,
      #                             conf.low = NA,
      #                             conf.high = NA)
      #     blrmodresults <- rbind(blrmodresults, blrmodj_i)
      #     
      #   } else{
      #     set.seed(seed + n)
      #     # priorpc_plot <- ppc_dens_overlay(y = datlist[[1]]$CoverObs, 
      #     #                                  yrep = posterior_predict(ppctest, ndraws=100))
      #     
      #     priorpc_plot <- ppc_dens_overlay(y = SAV4[ManagedAreaName == i & analysisunit == j, CoverObs], 
      #                                      yrep = posterior_predict(ppctest, ndraws=100))
      #     
      #     priorpc_plot <- priorpc_plot +
      #       labs(title = paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
      #                           ifelse(stringr::str_detect(i, "NERR"), "ERR_blr_priorpcplot_", 
      #                                  ifelse(stringr::str_detect(i, "NMS"), "MS_blr_priorpcplot_", "AP_blr_priorpcplot_")), 
      #                           gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),                                         gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1)))))
      #     
      #     saveRDS(priorpc_plot, here::here(paste0("diagnostics/SAV_", parameters[column == p, type], "_", 
      #                                             gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
      #                                             ifelse(stringr::str_detect(i, "NERR"), "ERR_blr_priorpcplot_", 
      #                                                    ifelse(stringr::str_detect(i, "NMS"), "MS_blr_priorpcplot_", "AP_blr_priorpcplot_")), 
      #                                             gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),                                         gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1))), 
      #                                             ".rds")))
      #     n <- n + 1
      #     
      #     #binomial logistic regression model
      #     #originally tried this as a bernoulli model, but the data get very unwieldy so it is better as a binomial
      #     # tic()
      #     # plan(multisession, workers = availableCores(omit = 1))
      #     # brm_i <- brm_multiple(formula =  CoverObs ~ relyear + (1 | LocationID), data = datlist,
      #     #                       family = bernoulli, prior = c(set_prior("normal(0,1)", class = "b")), 
      #     #                       cores = 15, control = list(adapt_delta = 0.99, max_treedepth = 10), 
      #     #                       iter = 9000, warmup = 3000, chains = 5, inits = 0, thin = 3, seed = seed + n,
      #     #                       file = here::here(paste0("SAV/output/models/SAV_", parameters[column == p, type], "_", 
      #     #                                                gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
      #     #                                                ifelse(stringr::str_detect(i, "NERR"), "ERR_blr_", 
      #     #                                                       ifelse(stringr::str_detect(i, "NMS"), "MS_blr_", "AP_blr_"), 
      #     #                                                gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),                                         gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1))), 
      #     #                                                ".rds")))
      #     # toc()
      #     
      #     # tic()
      #     # brm_i <- brm(formula =  CoverObs | trials(Grid) ~ relyear + (1 + relyear | LocationID), data = SAV4[ManagedAreaName == i & analysisunit == j, ],
      #     #              family = binomial, cores = 6, control = list(adapt_delta = 0.8, max_treedepth = 10), iter = 9000, 
      #     #              warmup = 3000, chains = 6, inits = 0, thin = 3, seed = seed + n, backend = "cmdstanr", threads = threading(2),
      #     #              file = here::here(paste0("SAV/output/models/SAV_", parameters[column == p, type], "_", 
      #     #                                       gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
      #     #                                       ifelse(stringr::str_detect(i, "NERR"), "ERR_blr_", 
      #     #                                              ifelse(stringr::str_detect(i, "NMS"), "MS_blr_", "AP_blr_")), 
      #     #                                       gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),                                         gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1))), 
      #     #                                       ".rds")))
      #     # toc()
      #     
      #     brm_i <- update(ppctest, cores = 6, iter = 9000, warmup = 3000, chains = 6, seed = seed + n, sample_prior = "no", 
      #                     file = here::here(paste0("SAV/output/models/SAV_", parameters[column == p, type], "_", 
      #                                              gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
      #                                              ifelse(stringr::str_detect(i, "NERR"), "ERR_blr_", 
      #                                                     ifelse(stringr::str_detect(i, "NMS"), "MS_blr_", "AP_blr_")), 
      #                                              gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),                                         gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1))), 
      #                                              ".rds")))
      #     
      #     n <- n + 1
      #     
      #     #diagnostic plots
      #     diag <- plot(brm_i, plot = FALSE)
      #     
      #     #add title
      #     title <- textGrob(paste0(j, ", ", ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
      #                                              ifelse(stringr::str_detect(i, "NMS), paste0(str_sub(i, 1, -5), "National Marine Sanctuary"), paste0(i, " Aquatic Preserve")))),
      #                       just = "left",
      #                       gp=gpar(fontsize=12))
      #     
      #     diag[[1]] <- gtable_add_rows(
      #       diag[[1]],
      #       heights = grobHeight(title) + unit(5, "mm"),
      #       pos = 0
      #     )
      #     
      #     diag[[1]] <- gtable_add_grob(
      #       diag[[1]],
      #       title,
      #       clip = "off",
      #       1, 1, 1, 1)
      #     
      #     if(class(try(diag[[2]])) != "try-error"){
      #       diag[[2]] <- gtable_add_rows(
      #         diag[[2]],
      #         heights = grobHeight(title) + unit(5, "mm"),
      #         pos = 0
      #       )
      #     }
      #     
      #     #save diagnostic plots
      #     saveRDS(diag, here::here(paste0("diagnostics/SAV_", parameters[column == p, type], "_", 
      #                                     gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
      #                                     ifelse(stringr::str_detect(i, "NERR"), "ERR_chainsplots_", 
      #                                            ifelse(stringr::str_detect(i, "NMS), "MS_chainsplots_", "AP_chainsplots_")), 
      #                                     gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),                                         gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1))), 
      #                                     ".rds")))
      #     
      #     #Model results table
      #     blrmodj_i <- setDT(broom.mixed::tidy(brm_i))
      #     blrmodj_i[, `:=` (managed_area = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), paste0(i, " Aquatic Preserve")),
      #                       species = j,
      #                       filename = paste0("SAV_", parameters[column == p, type], "_", 
      #                                         gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
      #                                         ifelse(stringr::str_detect(i, "NERR"), "ERR_blr_", 
      #                                                ifelse(stringr::str_detect(i, "NMS"), "MS_blr_", "AP_blr_")), 
      #                                         gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),                                         gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1))), ".rds"))]
      #     blrmodresults <- rbind(blrmodresults, blrmodj_i)
      #     
      #     #posterior predictive check
      #     set.seed(seed + n)
      #     postpc_plot <- try(pp_check(brm_i))
      #     x <- 1
      #     
      #     while(class(postpc_plot) == "try-error" & x < 1000){
      #       print(paste0("x = ", x))
      #       set.seed(seed + n)
      #       postpc_plot <- try(pp_check(brm_i))
      #       x <- x + 1
      #       n <- n + 1
      #     }
      #     
      #     postpc_plot <- postpc_plot +
      #       labs(title = paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
      #                           ifelse(stringr::str_detect(i, "NERR"), "ERR_blr_postpcplot_", 
      #                                  ifelse(stringr::str_detect(i, "NMS"), "MS_blr_postpcplot_", "AP_blr_postpcplot_")), 
      #                           gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),                                         gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1)))))
      #     
      #     saveRDS(postpc_plot, here::here(paste0("diagnostics/SAV_", parameters[column == p, type], "_", 
      #                                            gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
      #                                            ifelse(stringr::str_detect(i, "NERR"), "ERR_blr_postpcplot_", 
      #                                                   ifelse(stringr::str_detect(i, "NMS"), "MS_blr_postpcplot_", "AP_blr_postpcplot_")), 
      #                                            gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),                                         gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1))), 
      #                                            ".rds")))
      #
      #     n <- n + 1
      #     
      #     #conditional effects plot
      #     ceplot_i <- plot(conditional_effects(brm_i, categorical = TRUE), plot = FALSE)[[1]]
      #     
      #     nyrs <- (max(SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == j, relyear]) + 1) - (min(SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == j, relyear]) + 1)
      #     minyr <- min(SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == j, relyear]) + 1
      #     breaks <- c(round(minyr + nyrs/5),
      #                 round(minyr + 2*(nyrs/5)),
      #                 round(minyr + 3*(nyrs/5)),
      #                 round(minyr + 4*(nyrs/5)))
      #     yrlist <- sort(unique(SAV4$Year))
      #     
      #     ceplot_i <- ceplot_i +
      #       geom_hline(yintercept = 0, color = "grey10") +
      #       scale_x_continuous(breaks = breaks, labels = c(yrlist[breaks[1]], yrlist[breaks[2]], yrlist[breaks[3]], yrlist[breaks[4]])) +
      #       theme_bw() +
      #       labs(title = paste0(j, ", ", ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
      #                                           ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve")))), 
      #            color = "Species", 
      #            y = "Percent occurrence", 
      #            x = "Year") +
      #       scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
      #                          aesthetics = c("color", "fill"))
      #     
      #     saveRDS(ceplot_i, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_",
      #                                         gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE),
      #                                         ifelse(stringr::str_detect(i, "NERR"), "ERR_blrplot_", 
      #                                                ifelse(stringr::str_detect(i, "NMS"), "MS_blrplot_", "AP_blrplot_")),
      #                                         gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),                                         gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1))),
      #                                         ".rds")))
      #   }
      # }
      
      #Indicator == "PA"------------------------------------------------------------
      if(paste0(p) == "PA" & "PA" %in% Analyses) next
    }
    
    #Final results tables and plots--------------------------------------------------------------------
    if(paste0(p) %in% c("BB_pct", "PC") & ("BB_pct" %in% Analyses | "PC" %in% Analyses)){
      #Summarize # points per category
      # if(TRUE %in% str_detect(models, "_HaSpIn|_JoSe|_PaGr|_StGr")){
      if(i %in% ma_halspp){
        plotdat <- SAV4[ManagedAreaName == i & !is.na(eval(p)), ] %>% group_by(analysisunit, Year, relyear, eval(p)) %>% summarise(npt = n())
      } else{
        plotdat <- SAV4[ManagedAreaName == i & !is.na(eval(p)), ] %>% group_by(analysisunit_halid, Year, relyear, eval(p)) %>% summarise(npt = n())
      }
      setDT(plotdat)
      setnames(plotdat, "eval(p)", "data")
      # if(usenames == "common"){
      #   plotdat[, analysisunit_ord := analysisunit][, analysisunit_ord := ordered(analysisunit_ord, levels = c("Turtle grass",
      #                                                                                                          "Shoal grass",
      #                                                                                                          "Widgeon grass",
      #                                                                                                          "Manatee grass",
      #                                                                                                          "Tape grasses",
      #                                                                                                          "Total seagrass",
      #                                                                                                          "Drift algae",
      #                                                                                                          "Attached algae",
      #                                                                                                          "Total SAV"))]
      #   plotdat[, analysisunit := factor(analysisunit, levels = c("Turtle grass",
      #                                                              "Shoal grass",
      #                                                              "Widgeon grass",
      #                                                              "Manatee grass",
      #                                                              "Tape grasses",
      #                                                              "Total seagrass",
      #                                                              "Drift algae",
      #                                                              "Attached algae",
      #                                                              "Total SAV"))]
      # } else{
      #   plotdat[, analysisunit_ord := analysisunit][, analysisunit_ord := factor(analysisunit_ord, ordered = TRUE, levels = c("Thalassia testudinum",
      #                                                                                                                         "Halodule wrightii",
      #                                                                                                                         "Ruppia maritima",
      #                                                                                                                         "Syringodium filiforme",
      #                                                                                                                         "Halophila spp.",
      #                                                                                                                         "Total seagrass",
      #                                                                                                                         "Drift algae",
      #                                                                                                                         "Attached algae",
      #                                                                                                                         "Total SAV"))]
      # }
      
      #split modeled vs unmodeled data
      modeledsp <- c()
      for(u in seq_along(models)){
        if(usenames == "common"){
          name_u <- fcase(str_detect(models[[u]], "_ShGr"), "Shoal grass",
                          str_detect(models[[u]], "_TuGr"), "Turtle grass",
                          str_detect(models[[u]], "_MaGr"), "Manatee grass",
                          str_detect(models[[u]], "_WiGr"), "Widgeon grass",
                          str_detect(models[[u]], "_PaGr"), "Paddle grass",
                          str_detect(models[[u]], "_StGr"), "Star grass",
                          str_detect(models[[u]], "_JoSe"), "Johnson's seagrass",
                          str_detect(models[[u]], "_UnHa"), "Unidentified Halophila",
                          str_detect(models[[u]], "_HaSp"), "Halophila spp.",
                          str_detect(models[[u]], "_ToSe"), "Total seagrass",
                          str_detect(models[[u]], "_AtAl"), "Attached algae",
                          str_detect(models[[u]], "_DrAl"), "Drift algae",
                          str_detect(models[[u]], "_To"), "Total SAV")
          modeledsp <- append(modeledsp, name_u)
          
        } else{
          name_u <- fcase(str_detect(models[[u]], "_ThTe"), "Thalassia testudinum",
                          str_detect(models[[u]], "_SyFi"), "Syringodium filiforme",
                          str_detect(models[[u]], "_HaWr"), "Halodule wrightii",
                          str_detect(models[[u]], "_RuMa"), "Ruppia maritima",
                          str_detect(models[[u]], "_HaDe"), "Halophila decipiens",
                          str_detect(models[[u]], "_HaEn"), "Halophila engelmannii",
                          str_detect(models[[u]], "_HaJo"), "Halophila johnsonii",
                          str_detect(models[[u]], "_UnHa"), "Unidentified Halophila",
                          str_detect(models[[u]], "_HaSp"), "Halophila spp.",
                          str_detect(models[[u]], "_ToSe"), "Total seagrass",
                          str_detect(models[[u]], "_AtAl"), "Attached algae",
                          str_detect(models[[u]], "_DrAl"), "Drift algae",
                          str_detect(models[[u]], "_To"), "Total SAV")
          modeledsp <- append(modeledsp, name_u)
          
        }
      }
      # plotdat <- SAV4[ManagedAreaName == i & !is.na(eval(p)), ]
      # plotdat[ , modeled := fcase(analysisunit %in% modeledsp, "Modeled", 
      #                             default = "Not modeled")]
      
      #setDT(plotdat)
      #plotdat[, sqrt_npt := sqrt(npt)]
      
      miny <- c()
      for(v in seq_along(models)){
        miny_v <- try(predict(eval(models[[v]]), level = 0), silent = TRUE)
        if(class(miny_v) == "try-error") next
        miny <- append(miny, min(miny_v))
      }
      miny <- ifelse(floor(min(miny)) < 0, floor(min(miny)), 0)
      
      #create base plot of seagrass percent cover data over time for managed area i
      plot_i <- ggplot(data = droplevels(plotdat),
                       aes(x = relyear, y = data, fill = npt)) + #SAV4[ManagedAreaName == i & !is.na(eval(p)), ]    size = npt   fill = analysisunit,   y = eval(p)
        # geom_hline(yintercept = 0, color = "grey20") +
        geom_point(shape = 21, #fill = "grey90", #stroke = 2,
                   alpha = 0.9, color = "grey50") + #fill = analysisunit  shape = 21, color = "#333333", , color = analysisunit   size = 0.5, width = 0.001, height = 0.3
        geom_hline(yintercept = 0, color = "grey10", lwd = 0.5) +
        labs(title = parameters[column == p, name], subtitle = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"),
                            ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
             x = "Year",
             y = parameters[column == p, name],
             #color = "Species",
             #fill = "Species",
             fill = "Number of\nobservations") +
        plot_theme +
        ylim(miny, 100) +
        # scale_size_area(limits = c(1, max(plotdat$npt))) +
        # scale_color_manual(values = subset(spcols, names(spcols) %in% unique(plotdat[, analysisunit])),
        #                    aesthetics = c("color", "fill")) +
        scale_fill_continuous_sequential(palette = "YlGnBu") +
        scale_x_continuous(breaks = c(seq(from = min(SAV4[ManagedAreaName == i & !is.na(eval(p)), relyear]),
                                          to = max(SAV4[ManagedAreaName == i & !is.na(eval(p)), relyear]),
                                          by = 3)),
                           labels = c(seq(from = min(SAV4[ManagedAreaName == i & !is.na(eval(p)), Year]),
                                          to = max(SAV4[ManagedAreaName == i & !is.na(eval(p)), Year]),
                                          by = 3)))
      
      if(length(models) > 0){
        #make sure that no failed models slipped through
        classes <- lapply(models, function(x) class(eval(x)))
        models <- models[classes != "try-error"]
        
        aucol <- as.name(names(plot_i$data)[1])
        
        plot_i <- addfits_blacktrendlines(models, plot_i, p) + 
          {if(usenames == "common"){
            facet_wrap(~factor(eval(aucol), levels = c("Total SAV",
                                                       "Total seagrass",
                                                       "Unidentified Halophila",
                                                       "Halophila spp.",
                                                       "Johnson's seagrass",
                                                       "Manatee grass",
                                                       "Paddle grass",
                                                       "Shoal grass",
                                                       "Star grass",
                                                       "Turtle grass",
                                                       "Widgeon grass",
                                                       "Attached algae",
                                                       "Drift algae")),
                       labeller = labeller(eval(aucol) = c("Total SAV",
                                                           "Total seagrass",
                                                           "Halophila spp.",
                                                           "Halophila spp.",
                                                           "Johnson's seagrass",
                                                           "Manatee grass",
                                                           "Paddle grass",
                                                           "Shoal grass",
                                                           "Star grass",
                                                           "Turtle grass",
                                                           "Widgeon grass",
                                                           "Attached algae",
                                                           "Drift algae")),
                       ncol = 3, strip.position = "top")
          } else{
            facet_wrap(~factor(eval(aucol), levels = c("Total SAV",
                                                       "Total seagrass",
                                                       "Halodule wrightii",
                                                       "Halophila decipiens",
                                                       "Halophila engelmannii",
                                                       "Halophila johnsonii",
                                                       "Unidentified Halophila",
                                                       "Halophila spp.",
                                                       "Ruppia maritima",
                                                       "Syringodium filiforme",
                                                       "Thalassia testudinum",
                                                       "Attached algae",
                                                       "Drift algae")),
                       labeller = labeller(eval(aucol) = c("Total SAV",
                                                           "Total seagrass",
                                                           "Halodule wrightii",
                                                           "Halophila decipiens",
                                                           "Halophila engelmannii",
                                                           "Halophila johnsonii",
                                                           "Halophila spp.",
                                                           "Halophila spp.",
                                                           "Ruppia maritima",
                                                           "Syringodium filiforme",
                                                           "Thalassia testudinum",
                                                           "Attached algae",
                                                           "Drift algae")), 
                       ncol = 3, strip.position = "top")
          }}
      }
      #plot_i
      
      # #create base plot of seagrass percent cover data over time for managed area i
      # plot_i <- ggplot(data = plotdat,
      #                  aes(x = relyear, y = eval(p))) + #SAV4[ManagedAreaName == i & !is.na(eval(p)), ]     y = data    size = npt
      #   geom_jitter(data = plotdat[modeled == "Not modeled", ], aes(fill = analysisunit), shape = 21, #stroke = 2,
      #               alpha = 0.9, color = "#333333", size = 3) + #fill = analysisunit  shape = 21, color = "#333333", , color = analysisunit   size = 0.5, width = 0.001, height = 0.3
      #   labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
      #                       ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
      #        x = "Year",
      #        y = parameters[column == p, name],
      #        color = "Species",
      #        fill = "Species",
      #        size = "Number of\nobservations") +
      #   plot_theme +
      #   ylim(0, 100) +
      #   # scale_size_area(limits = c(1, max(plotdat$npt))) +
      #   scale_color_manual(values = subset(spcols, names(spcols) %in% unique(plotdat[, analysisunit])), 
      #                      aesthetics = c("color", "fill")) +
      #   scale_x_continuous(breaks = c(seq(from = min(SAV4[ManagedAreaName == i & !is.na(eval(p)), relyear]),
      #                                     to = max(SAV4[ManagedAreaName == i & !is.na(eval(p)), relyear]),
      #                                     by = 3)),
      #                      labels = c(seq(from = min(SAV4[ManagedAreaName == i & !is.na(eval(p)), Year]),
      #                                     to = max(SAV4[ManagedAreaName == i & !is.na(eval(p)), Year]),
      #                                     by = 3))) +
      #   theme_bw()
      # 
      # if(length(models) > 0){
      #   plot_i <- plot_i + theme(legend.position = "none") + labs(title = "Unmodeled species")
      #   
      #   plot_i_mod <- ggplot(data = plotdat,
      #                        aes(x = relyear, y = eval(p))) + #SAV4[ManagedAreaName == i & !is.na(eval(p)), ]     y = data    size = npt
      #     # geom_jitter(data = plotdat[modeled == "Not modeled", ], aes(fill = analysisunit), shape = 21, #stroke = 2,
      #     #             alpha = 0.9, color = "#333333", size = 3) + #fill = analysisunit  shape = 21, color = "#333333", , color = analysisunit   size = 0.5, width = 0.001, height = 0.3
      #     labs(title = "Modeled species",
      #          x = "Year",
      #          y = parameters[column == p, name],
      #          color = "Species",
      #          fill = "Species",
      #          size = "Number of\nobservations") +
      #     plot_theme +
      #     ylim(0, 100) +
      #     # scale_size_area(limits = c(1, max(plotdat$npt))) +
      #     scale_color_manual(values = subset(spcols, names(spcols) %in% unique(plotdat[, analysisunit])), 
      #                        aesthetics = c("color", "fill")) +
      #     scale_x_continuous(breaks = c(seq(from = min(SAV4[ManagedAreaName == i & !is.na(eval(p)), relyear]),
      #                                       to = max(SAV4[ManagedAreaName == i & !is.na(eval(p)), relyear]),
      #                                       by = 3)),
      #                        labels = c(seq(from = min(SAV4[ManagedAreaName == i & !is.na(eval(p)), Year]),
      #                                       to = max(SAV4[ManagedAreaName == i & !is.na(eval(p)), Year]),
      #                                       by = 3))) +
      #     theme_bw()
      #   plot_i_mod <- addfits(models, plot_i_mod, p)
      # }
      # 
      # 
      # plot_i2 <- plot_i_mod / plot_i + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A", title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
      #                                                                                                          ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))))
      
      #Save the plot object as .rds
      saveRDS(plot_i, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                        ifelse(stringr::str_detect(i, "NERR"), 
                                               paste0(str_sub(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 1, -2), "NERR_trendplot.rds"),
                                               ifelse(stringr::str_detect(i, "NMS"), 
                                                      paste0(str_sub(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 1, -2), "NMS_trendplot.rds"),
                                                      paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "AP_trendplot.rds"))))))
      
      #Save the results table objects as .rds
      saveRDS(lmemodresults, here::here(paste0("SAV/output/tables/SAV_", parameters[column == p, type], "_", 
                                               ifelse(stringr::str_detect(i, "NERR"), 
                                                      paste0(str_sub(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 1, -2), "NERR_lmeresults.rds"), 
                                                      ifelse(stringr::str_detect(i, "NMS"),
                                                             paste0(str_sub(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 1, -2), "NMS_lmeresults.rds"),
                                                             paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "AP_lmeresults.rds"))))))
    }
    
    if(paste0(p) == "BB_all" & "BB_all" %in% Analyses) next #{
    #   saveRDS(olrmodresults, here::here(paste0("SAV/output/tables/SAV_", parameters[column == p, type], "_", 
    #                                            ifelse(stringr::str_detect(i, "NERR"), 
    #                                                   paste0(str_sub(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 1, -2), "NERR_olrresults.rds"),
    #                                                   ifelse(stringr::str_detect(i, "NMS"),
    #                                                          paste0(str_sub(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 1, -2), "NMS_olrresults.rds"),
    #                                                          paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "AP_olrresults.rds"))))))
    # }
    
    if(paste0(p) == "PO" & "PO" %in% Analyses) next #{
    #   saveRDS(blrmodresults, here::here(paste0("SAV/output/tables/SAV_", parameters[column == p, type], "_", 
    #                                            ifelse(stringr::str_detect(i, "NERR"), 
    #                                                   paste0(str_sub(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 1, -2), "NERR_blrresults.rds"), 
    #                                                   ifelse(stringr::str_detect(i, "NMS"),
    #                                                          paste0(str_sub(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 1, -2), "NMS_blrresults.rds"),
    #                                                          paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "AP_blrresults.rds"))))))
    # }
    
    if(paste0(p) == "PA" & "PA" %in% Analyses){
      #Bar chart of proportions by analysisunit
      breaks <- c(seq(min(SAV4[ManagedAreaName == i & !is.na(PA), relyear]),
                      max(SAV4[ManagedAreaName == i & !is.na(PA), relyear]),
                      by = 2))
      yrlist <- sort(unique(SAV4$Year))
      
      labels <- c()
      for(b in breaks){
        labels <- append(labels, yrlist[b + 1])
      }
      
      if(i %in% ma_halspp){
        bpdat <- props[ManagedAreaName == i & !is.na(analysisunit) & str_detect(analysisunit, "decipiens|engelmannii|johnsonii|Unidentified|Star|Paddle|Johnson", negate = TRUE), ]
        
        barplot_sp <- ggplot(data = bpdat, aes(x = relyear, y = sp_pct, fill = analysisunit)) +
          geom_col(color = "grey20") +
          scale_x_continuous(breaks = breaks, labels = labels) +
          plot_theme +
          labs(title = parameters[column == p, name], subtitle = paste0(ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                                     ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve")))), 
               fill = "Species", 
               x = "Year", 
               y = "Occurrence frequency (%)") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(bpdat$analysisunit)),
                             labels = str_replace(names(spcols), "Unidentified Halophila", "Halophila spp."), 
                             aesthetics = c("color", "fill"))
        
      } else{
        bpdat <- props[ManagedAreaName == i & !is.na(analysisunit) & analysisunit != "Halophila spp.", ]
        
        barplot_sp <- ggplot(data = bpdat, aes(x = relyear, y = sp_pct, fill = analysisunit)) +
          geom_col(color = "grey20") +
          scale_x_continuous(breaks = breaks, labels = labels) +
          plot_theme +
          labs(title = parameters[column == p, name], subtitle = paste0(ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                                     ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve")))), 
               fill = "Species", 
               x = "Year", 
               y = "Occurrence frequency (%)") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(bpdat$analysisunit)),
                             labels = str_replace(names(spcols), "Unidentified Halophila", "Halophila spp."),
                             aesthetics = c("color", "fill"))
        
      }
      
      saveRDS(barplot_sp, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                            gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                            ifelse(stringr::str_detect(i, "NERR"), "ERR_barplot_sp", 
                                                   ifelse(stringr::str_detect(i, "NMS"), "MS_barplot_sp", "AP_barplot_sp")), 
                                            ".rds")))
      
      # saveRDS(belrmodresults, here::here(paste0("SAV/output/tables/SAV_", parameters[column == p, type], "_", 
      #                                           ifelse(stringr::str_detect(i, "NERR"), 
      #                                                  paste0(str_sub(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 1, -2), "NERR_belrresults.rds"),
      #                                                  ifelse(stringr::str_detect(i, "NMS"),
      #                                                         paste0(str_sub(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 1, -2), "NMS_belrresults.rds"),
      #                                                         paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "AP_belrresults.rds"))))))
    }
    
    print(paste0("  Plot objects and results tables saved: ", 
                 gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                 "_", 
                 gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE)))
  }
}

#Save failedmodslist-----------------------------------------------------
saveRDS(failedmods, here::here("SAV/output/models/failedmodslist.rds"))


#Get rid of eval(p)'s from plot file mappings---------------------------------------
files <- list.files(here::here("SAV/output/Figures/BB/")) #get file list
files <- str_subset(files, ".rds") #exclude non-.RDS files

filesupdated <- list()
for(f in seq_along(files)){
  file_f <- readRDS(here::here(paste0("SAV/output/Figures/BB/", files[f])))
  if(paste0(as_label(file_f$mapping$y)) == "eval(p)"){
    file_f$mapping$y <- parameters[name %in% file_f$labels$y, column][[1]]
    saveRDS(file_f, here::here(paste0("SAV/output/Figures/BB/", files[f])))
    rm(file_f)
    filesupdated <- append(filesupdated, files[f])
  } else {
    rm(file_f)
  }
  if(round((f/length(files))*100, 1) %% 10 == 0){
    print(paste0(round((f/length(files))*100), "% done!"))
  }
}


#Save .png versions of specified "trendplot" .rds files --------------------------------------------------
files <- list.files(here::here("SAV/output/Figures/BB/")) #get file list
plots <- stringr::str_subset(files, "_trendplot") #identify map file
mods <- list.files(here::here("SAV/output/models/"))
#models2 <- str_subset(mods, paste0(str_sub(plots[1], 1, str_locate_all(plots[1], "_")[[1]][2])))
models2 <- str_subset(mods, "_lme")

malist <- c()
for(pl in plots){
  ma_p <- str_sub(pl, str_locate_all(pl, "_")[[1]][2], str_locate_all(pl, "_")[[1]][3])
  malist <- append(malist, ma_p)
}

failedmodslist <- readRDS(here::here("SAV/output/models/failedmodslist.rds"))

for(m in malist){
  mods_m <- str_subset(models2, m)
  mods_m <- setdiff(mods_m, failedmodslist$model)
  mods_m <- setdiff(mods_m, subset(mods_m, str_detect(mods_m, "_PC_")))
  params <- unique(str_sub(mods_m, 1, str_locate_all(mods_m, "_")[[1]][2] - 1))
  
  for(param in params){
    mods_m_p <- str_subset(mods_m, param)
    plot_m <- try(str_subset(plots, unique(str_sub(mods_m_p, 1, str_locate_all(mods_m_p, "_")[[1]][3] - 1))), silent = TRUE)
    
    if(class(plot_m) != "try-error"){
      for(w in mods_m_p){
        aucol <- ifelse(str_sub(w, str_locate_all(w, "_")[[1]][2],
                                ifelse(str_detect(w, "AP_"), str_locate(w, "AP_")[2],
                                       ifelse(str_detect(w, "NERR_"), 
                                              str_locate(w, "NERR_")[2], 
                                              str_locate(w, "NMS_")[2]))) %in% c("_BRAP_", "_IRMVBAP_", "_IRVBPAP_", "_JBJIAP_", "_LRLWCAP_", 
                                                                                 "_MLAP_", "_BBAP_", "_FKNMS_"), as.name("analysisunit"), as.name("analysisunit_halid"))
        
        eval(call("<-", as.name(paste0(str_sub(w, str_locate_all(w, "_")[[1]][2] + 1, 
                                               ifelse(str_detect(w, "AP_"), str_locate(w, "AP_") - 1, 
                                                      ifelse(str_detect(w, "NERR_"), str_locate(w, "NERR_"), str_locate(w, "NMS_")))), 
                                       "_", 
                                       str_sub(w, -8, -5))), 
                  readRDS(here::here(paste0("SAV/output/models/", w)))))
      }
      
      plot_m2 <- readRDS(here::here(paste0("SAV/output/Figures/BB/", plot_m)))
      plot_m2 <- plot_m2 +
        plot_theme
      
      wid <- ifelse(length(unique(plot_m2$data$analysisunit)) == 1, 4,
                    ifelse(length(unique(plot_m2$data$analysisunit)) == 2, 6, 8))
      hei <- ifelse(length(unique(plot_m2$data$analysisunit)) <= 3, 3,
                    ifelse(length(unique(plot_m2$data$analysisunit)) <= 6, 6, 9))
      
      png(here::here(paste0("SAV/output/website/images/trendplots/", str_sub(plot_m, 1, -5), ".png")),
          width = wid,
          height = hei,
          units = "in",
          res = 300)
      # jpeg(here::here(paste0("SAV/output/Figures/BB/img/", str_sub(plot_m, 1, -5), ".jpg")),
      #      width = 10,
      #      height = 6,
      #      units = "in",
      #      res = 300)
      print(plot_m2)
      dev.off()
      
    } else{
      next
    }
  }
}

#Save .png versions of "barplot" .rds files --------------------------------------------------
plots2 <- stringr::str_subset(files, "_barplot") #identify map file

for(pl2 in plots2){
  plot_pl2 <- readRDS(here::here(paste0("SAV/output/Figures/BB/", pl2)))
  plot_pl2 <- plot_pl2 +
    plot_theme
  
  png(here::here(paste0("SAV/output/website/images/barplots/", str_sub(pl2, 1, -5), ".png")),
      width = 8,
      height = 4,
      units = "in",
      res = 200)
  
  print(plot_pl2)
  dev.off()
}


#Crop geographic scope figure images & add metadata stamp ----------------------------
gsfigs <- list.files(here::here("SAV/output/Figures/BB/img/"), full.names = TRUE)
gsfigs <- gsfigs[which(str_detect(gsfigs, "map"))]
GeoDBdate2 <- paste0(str_sub(GeoDBdate, -4, -1), "-", fcase(str_detect(GeoDBdate, "jan"), "01",
                                                            str_detect(GeoDBdate, "feb"), "02",
                                                            str_detect(GeoDBdate, "mar"), "03",
                                                            str_detect(GeoDBdate, "apr"), "04",
                                                            str_detect(GeoDBdate, "may"), "05",
                                                            str_detect(GeoDBdate, "jun"), "06",
                                                            str_detect(GeoDBdate, "jul"), "07",
                                                            str_detect(GeoDBdate, "aug"), "08",
                                                            str_detect(GeoDBdate, "sep"), "09",
                                                            str_detect(GeoDBdate, "oct"), "10",
                                                            str_detect(GeoDBdate, "nov"), "11",
                                                            str_detect(GeoDBdate, "dec"), "12"), "-", str_sub(GeoDBdate, 1, 2))

for(gs in gsfigs){
  fig_gs <- image_read(gs)
  fig_gs <- image_trim(fig_gs)
  iminfo <- image_info(fig_gs)
  fig_gs <- image_extent(fig_gs, paste0(iminfo$width + 100, "x", iminfo$height + 100, "-50"), gravity = "north", color = "white")
  this <- this.path::this.path()
  fig_gs <- image_annotate(
    fig_gs, text = paste0("Date: ", Sys.Date(), ";  Script: ", str_sub(this, max(str_locate_all(this, "/")[[1]]) + 1, -1), ";  Geodatabase ver.: ", GeoDBdate2), size = 20, 
    color = "grey40", gravity = "southeast", location = "+10+0", font = "Arial" #location = paste0("+", iminfo$width, "+", iminfo$height - 10), 
  )
  #paste0(iminfo$width, iminfo$height - 10)
  
  image_write(fig_gs, here::here(paste0("SAV/output/Figures/BB/img/", str_sub(gs, str_locate_all(gs, "/Figures/BB/img/")[[1]][2] + 1, -5), "_", Sys.Date(), ".jpg")),
              format = "jpeg", quality = 100)
}

toc()

