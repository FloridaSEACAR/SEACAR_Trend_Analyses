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

#Load and wrangle data------------------------------------------------------------
data_dir <- here::here("SAV/data")

file_in <- list.files(data_dir, pattern="Braun_Blanquet", full=TRUE)
file_in <- file_in[-grep("Modified", file_in)]
BB <- fread(file_in, sep = "|", header = TRUE, stringsAsFactors = FALSE,
            na.strings = "")
BB$QuadIdentifier <- as.character(BB$QuadIdentifier)
BB$BB <- BB$ResultValue

file_in <- list.files(data_dir, pattern="Modified_Braun_Blanquet", full=TRUE)
mBB <- fread(file_in, sep = "|", header = TRUE, stringsAsFactors = FALSE,
             na.strings = "")
mBB$QuadIdentifier <- as.character(mBB$QuadIdentifier)
mBB$mBB <- mBB$ResultValue

file_in <- list.files(data_dir, pattern="Percent_Cover", full=TRUE)
PC <- fread(file_in, sep = "|", header = TRUE, stringsAsFactors = FALSE,
            na.strings = "")
PC$QuadIdentifier <- as.character(PC$QuadIdentifier)
PC$PC <- PC$ResultValue

file_in <- list.files(data_dir, pattern="Percent_Occurrence", full=TRUE)
PO <- fread(file_in, sep = "|", header = TRUE, stringsAsFactors = FALSE,
            na.strings = "")
PO$QuadIdentifier <- as.character(PO$QuadIdentifier)
PO$PO <- PO$ResultValue

file_in <- list.files(data_dir, pattern="Shoot_Count", full=TRUE)
SC <- fread(file_in, sep = "|", header = TRUE, stringsAsFactors = FALSE,
            na.strings = "")
SC$QuadIdentifier <- as.character(SC$QuadIdentifier)
SC$SC <- SC$ResultValue

file_in <- list.files(data_dir, pattern="Presence_Absence", full=TRUE)
PA <- fread(file_in, sep = "|", header = TRUE, stringsAsFactors = FALSE,
            na.strings = "")
PA$QuadIdentifier <- as.character(PA$QuadIdentifier)
PA$PA <- PA$ResultValue

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
SAV[ SAV == "NA" ] <- NA

SAV_sum <- SAV %>% group_by(ManagedAreaName) %>% summarize(n_yr = length(unique(Year)), yrs = list(sort(unique(Year))))

SAV2 <- subset(SAV, !is.na(SAV$BB) | !is.na(SAV$mBB) | !is.na(SAV$PC) | !is.na(SAV$PO))
SAV2 <- SAV2 %>% filter(BB >= 0 & BB <= 5 | is.na(BB))
SAV2 <- SAV2 %>% filter(mBB >= 0 & mBB <= 5 | is.na(mBB))
SAV2 <- SAV2 %>% filter(PC >= 0 & PC <= 100 | is.na(PC))
SAV2 <- SAV2 %>% filter(PO >= 0 & PO <= 100 | is.na(PO))
SAV2 <- SAV2 %>% filter(Month %in% c(4:10))

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

species_reject <- c("All", "NA",
                    "Vallisneria americana", "Najas guadalupensis",
                    "Hydrilla verticillata", "Potamogeton pusillus",
                    "Zannichellia palustris")
SAV3[, analysisunit := ifelse(CommonIdentifier %in% species_reject, NA, 
                              ifelse(str_detect(CommonIdentifier, "Halophila"), "Halophila spp.", 
                                     ifelse(SpeciesGroup1 == "Seagrass", CommonIdentifier, Drift_Attached)))]
SAV3[!is.na(Drift_Attached), analysisunit := paste0(analysisunit, " algae")]
SAV3$analysisunit[SAV3$analysisunit=="Total_SAV"] <- "Total SAV"
SAV4 <- subset(SAV3, !is.na(SAV3$analysisunit))

#Temporary fix to programs 570 and 571 - Group 1 should be "Total seagrass" instead of "Total_SAV"
SAV4[ProgramID %in% c(570, 571) & SpeciesGroup1 == "Total SAV", SpeciesGroup1 := "Total seagrass"]

stats <- SAV4 %>%
  group_by(ManagedAreaName, analysisunit) %>%
  summarize(ParameterName="BB_pct",
            N_Data=length(BB_pct[!is.na(BB_pct)]),
            N_Years=length(unique(Year[!is.na(Year) & !is.na(BB_pct)])),
            EarliestYear=min(Year[!is.na(BB_pct)]),
            LatestYear=max(Year[!is.na(BB_pct)]),
            SufficientData=ifelse(N_Data>0 & N_Years>=5, TRUE, FALSE))
fwrite(stats, here::here("SAV/output/data/SAV_BBpct_Stats.txt"), sep = "|")

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
addfits <- function(models, plot_i, param){
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
                                                               aes(x = relyear, y = predict(eval(models[[7]]), level = 0), color = unique(eval(models[[7]])$data$analysisunit)), lwd = 1.2))))))))
}

addfits_blacktrendlines <- function(models, plot_i, param){
  ifelse(length(models) == 1,
         return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[1]])$data$analysisunit)],
                                   aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=1.2, alpha=0.7)),
         ifelse(length(models) == 2,
                return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[1]])$data$analysisunit)],
                                          aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=1.2, alpha=0.7) +
                         geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[2]])$data$analysisunit)],
                                   aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=1.2, alpha=0.7)),
                ifelse(length(models) == 3,
                       return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[1]])$data$analysisunit)],
                                                 aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=1.2, alpha=0.7) +
                                geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[2]])$data$analysisunit)],
                                          aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=1.2, alpha=0.7) +
                                geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[3]])$data$analysisunit)],
                                          aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=1.2, alpha=0.7)),
                       ifelse(length(models) == 4,
                              return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[1]])$data$analysisunit)],
                                                        aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=1.2, alpha=0.7) +
                                       geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[2]])$data$analysisunit)],
                                                 aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=1.2, alpha=0.7) +
                                       geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[3]])$data$analysisunit)],
                                                 aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=1.2, alpha=0.7) +
                                       geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[4]])$data$analysisunit)],
                                                 aes(x = relyear, y = predict(eval(models[[4]]), level = 0)), color="#000099", size=1.2, alpha=0.7)),
                              ifelse(length(models) == 5,
                                     return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[1]])$data$analysisunit)],
                                                               aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=1.2, alpha=0.7) +
                                              geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[2]])$data$analysisunit)],
                                                        aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=1.2, alpha=0.7) +
                                              geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[3]])$data$analysisunit)],
                                                        aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=1.2, alpha=0.7) +
                                              geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[4]])$data$analysisunit)],
                                                        aes(x = relyear, y = predict(eval(models[[4]]), level = 0)), color="#000099", size=1.2, alpha=0.7) +
                                              geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[5]])$data$analysisunit)],
                                                        aes(x = relyear, y = predict(eval(models[[5]]), level = 0)), color="#000099", size=1.2, alpha=0.7)),
                                     ifelse(length(models) == 6,
                                            return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[1]])$data$analysisunit)],
                                                                      aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=1.2, alpha=0.7) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[2]])$data$analysisunit)],
                                                               aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=1.2, alpha=0.7) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[3]])$data$analysisunit)],
                                                               aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=1.2, alpha=0.7) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[4]])$data$analysisunit)],
                                                               aes(x = relyear, y = predict(eval(models[[4]]), level = 0)), color="#000099", size=1.2, alpha=0.7) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[5]])$data$analysisunit)],
                                                               aes(x = relyear, y = predict(eval(models[[5]]), level = 0)), color="#000099", size=1.2, alpha=0.7) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[6]])$data$analysisunit)],
                                                               aes(x = relyear, y = predict(eval(models[[6]]), level = 0)), color="#000099", size=1.2, alpha=0.7)),
                                            
                                            return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[1]])$data$analysisunit)],
                                                                      aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=1.2, alpha=0.7) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[2]])$data$analysisunit)],
                                                               aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=1.2, alpha=0.7) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[3]])$data$analysisunit)],
                                                               aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=1.2, alpha=0.7) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[4]])$data$analysisunit)],
                                                               aes(x = relyear, y = predict(eval(models[[4]]), level = 0)), color="#000099", size=1.2, alpha=0.7) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[5]])$data$analysisunit)],
                                                               aes(x = relyear, y = predict(eval(models[[5]]), level = 0)), color="#000099", size=1.2, alpha=0.7) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[6]])$data$analysisunit)],
                                                               aes(x = relyear, y = predict(eval(models[[6]]), level = 0)), color="#000099", size=1.2, alpha=0.7) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == unique(eval(models[[7]])$data$analysisunit)],
                                                               aes(x = relyear, y = predict(eval(models[[7]]), level = 0)), color="#000099", size=1.2, alpha=0.7))))))))
}

EDA <- FALSE #Create and export Exploratory Data Analysis plots (TRUE = yes, FALSE = no)

if(EDA){
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
  
  rotate_sf <- function(data, x_add = 0, y_add = 0, coast = "Atlantic") {
    
    if(coast == "Atlantic"){
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
  locs_pts <- st_read(here::here("SAV/mapping/SEACAR_SampleLocations_07oct21/seacar_dbo_SampleLocation_Point.shp"))
  locs_lns <- st_read(here::here("SAV/mapping/SEACAR_SampleLocations_07oct21/seacar_dbo_SampleLocation_Line.shp"))
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
}

tic()
#Empty data.table to house names of any failed models generated below.
failedmods <- data.table(model = character(),
                         error = character())

#Create a table of the proportion of present SAV types by managed area and year
props <- SAV4 %>% group_by(ManagedAreaName, analysisunit, relyear) %>% summarize(n1 = sum(PA), ntot = n(), prop = n1/ntot)
setDT(props)
for(m in unique(props$ManagedAreaName)){
  props[ManagedAreaName == m, sp_prop := n1/sum(n1), by = c("relyear")]
}

spcollist <- c("#00374f",
               "#004d68",
               "#006481",
               "#007c99",
               "#0094b0",
               "#00aec6",
               "#00c9db",
               "#00e4ee",
               "#00ffff")

# spcollist <- hcl.colors(n = 8, palette = "Blues 3")
spp <- c("Thalassia testudinum", "Syringodium filiforme", "Halodule wrightii", "Ruppia maritima", "Halophila spp.", "Total seagrass", 
         "Attached algae", "Drift algae", "Total SAV")
spp_common <- c("Turtle grass", "Manatee grass", "Shoal grass", "Widgeon grass", "Tape grasses", "Total seagrass", 
                "Attached algae", "Drift algae", "Total SAV")

usenames <- "common" #alternative is "scientific"
if(usenames == "common"){
  spcols <- setNames(spcollist, spp_common)
} else{
  spcols <- setNames(spcollist, spp)
}

if(usenames == "common"){
  SAV4[, analysisunit := fcase(analysisunit == "Thalassia testudinum", "Turtle grass",
                               analysisunit == "Syringodium filiforme", "Manatee grass",
                               analysisunit == "Halodule wrightii", "Shoal grass",
                               analysisunit == "Ruppia maritima", "Widgeon grass",
                               analysisunit == "Halophila spp.", "Tape grasses",
                               analysisunit == "Total seagrass", "Total seagrass",
                               analysisunit == "Attached algae", "Attached algae",
                               analysisunit == "Drift algae", "Drift algae",
                               analysisunit == "Total SAV", "Total SAV")]
  
  props[, analysisunit := fcase(analysisunit == "Thalassia testudinum", "Turtle grass",
                                analysisunit == "Syringodium filiforme", "Manatee grass",
                                analysisunit == "Halodule wrightii", "Shoal grass",
                                analysisunit == "Ruppia maritima", "Widgeon grass",
                                analysisunit == "Halophila spp.", "Tape grasses",
                                analysisunit == "Total seagrass", "Total seagrass",
                                analysisunit == "Attached algae", "Attached algae",
                                analysisunit == "Drift algae", "Drift algae",
                                analysisunit == "Total SAV", "Total SAV")]
}

prcollist <- hcl.colors(n = 21, palette = "viridis")
progs <- sort(unique(SAV4$ProgramID))
prcols <- setNames(prcollist, progs)

parameters <- data.table(column = c(as.name("BB_all"), as.name("BB_pct"), as.name("PC"), as.name("PO"), as.name("PA")),
                         name = c("Braun Blanquet score", "Median percent cover", "Visual percent cover", "Percent occurrence", "Frequency of occurrence"),
                         type = c("BBall", "BBpct", "PC", "PO", "PA"))

plot_theme <- theme_bw() +
  theme(text=element_text(family="Segoe UI"),
        title=element_text(face="bold"),
        plot.title=element_text(hjust=0.5, size=14, color="#314963"),
        plot.subtitle=element_text(hjust=0.5, size=10, color="#314963"),
        axis.title.x = element_text(margin = margin(t = 5, r = 0,
                                                    b = 10, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10,
                                                    b = 0, l = 0)),
        axis.text=element_text(size=10),
        axis.text.x=element_text(face="bold", angle = 60, hjust = 1),
        axis.text.y=element_text(face="bold"))

# #subset to run only part of the script------------------------------------------------------
# parameters <- parameters[column == "PA", ]

#start script----------------------------------------------------------------------
n <- 0
seed <- 352
set.seed(seed)
for(p in parameters$column){
  
  cat(paste0("\nStarting indicator: ", p, "\n"))
  
  #List managed areas with at least 5 years of data
  nyears <- SAV4[!is.na(eval(p)) & !is.na(analysisunit), ] %>% group_by(ManagedAreaName, analysisunit) %>% summarize(type = paste0(p), nyr = length(unique(Year)))
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
    if(EDA){ 
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
      
      
      #Create map(s) for the managed area-------------------------------------------
      
      fl_i <- st_crop(counties, xmin = corners[ManagedAreaName == i, xmin], xmax = corners[ManagedAreaName == i, xmax], ymin = corners[ManagedAreaName == i, ymin], ymax = corners[ManagedAreaName == i, ymax])
      rcp_i <- subset(rcp, rcp$LONG_NAME == ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                                                   ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), 
                                                          ifelse(str_detect(i, "Fort Clinch|Fort Pickens|Rocky Bayou|St. Andrews"), paste0(i, " State Park Aquatic Preserve"), paste0(i, " Aquatic Preserve")))))
      
      locs_pts_rcp_i <- locs_pts_rcp[rcp_i, , op = st_intersects]
      locs_lns_rcp_i <- locs_lns_rcp[rcp_i, , op = st_intersects]
      
      yadd <- 0
      startyear <- min(SAV4[ManagedAreaName == i & !is.na(eval(p)), Year])
      base <- ggplot() +
        geom_sf(data = rotate_sf(fl_i, coast = corners[ManagedAreaName == i, Coast[1]]), fill = "beige", color = "navajowhite3", inherit.aes = FALSE) +
        geom_sf(data = rotate_sf(rcp_i, coast = corners[ManagedAreaName == i, Coast[1]]), color = "grey50", fill = "mediumaquamarine", alpha = 0.4, lwd = 1, inherit.aes = FALSE) +
        #scale_fill_brewer(palette = "Dark2") +
        scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), ProgramID])), 
                           aesthetics = c("color", "fill")) +
        labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                            ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))), 
             fill = "Program ID") +
        theme(panel.grid.major = element_line(colour = NA),
              panel.grid.minor = element_line(colour = NA),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              panel.background = element_rect(fill = NA),
              plot.background = element_rect(colour = NA))
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
                                   coast = corners[ManagedAreaName == i, Coast[1]]),
                  aes(fill = droplevels(as.factor(ProgramID))), shape = 21, color = "black")
      }
      
      if(length(subset(locs_lns_rcp_i, locs_lns_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year == startyear, LocationID]))$LocationID) > 0){
        base <- base +
          geom_sf(data = rotate_sf(subset(locs_lns_rcp_i, locs_lns_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year == startyear, LocationID])),
                                   coast = corners[ManagedAreaName == i, Coast[1]]),
                  aes(color = droplevels(as.factor(ProgramID))), shape = 21)
      }
      
      for(y in sort(unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year != startyear, Year]))){
        base <- base +
          geom_sf(data = rotate_sf(rcp_i, y_add = yadd + maxydist, coast = corners[ManagedAreaName == i, Coast[1]]), 
                  color = "grey50", fill = "mediumaquamarine", alpha = 0.85, lwd = 1, inherit.aes = FALSE) +
          annotate("text", x = xlab, y = xmax_y + yadd + maxydist, label = y, hjust = "left")
        
        if(length(subset(locs_pts_rcp_i, locs_pts_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year == y, LocationID]))$LocationID) > 0){
          base <- base +
            geom_sf(data = rotate_sf(subset(locs_pts_rcp_i, locs_pts_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year == y, LocationID])),
                                     y_add = yadd + maxydist, coast = corners[ManagedAreaName == i, Coast[1]]), 
                    aes(fill = droplevels(as.factor(ProgramID))), shape = 21, color = "black")
        }
        
        if(length(subset(locs_lns_rcp_i, locs_lns_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year == startyear, LocationID]))$LocationID) > 0){
          base <- base +
            geom_sf(data = rotate_sf(subset(locs_lns_rcp_i, locs_lns_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year == startyear, LocationID])),
                                     y_add = yadd + maxydist, coast = corners[ManagedAreaName == i, Coast[1]]),
                    aes(color = droplevels(as.factor(ProgramID))), shape = 21)
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
        theme(legend.position='right', 
              legend.justification='top',
              legend.direction='vertical')
      
      ggsave(filename = here::here(paste0("SAV/output/Figures/BB/img/SAV_", parameters[column == p, type], "_", 
                                          gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE),
                                          ifelse(stringr::str_detect(i, "NERR"), "ERR_map_bypr.jpg", 
                                                 ifelse(stringr::str_detect(i, "NMS"), "MS_map_bypr.jpg", "AP_map_bypr.jpg")))), 
             plot = base,
             width = 6,
             #height = 8 + nlayers - 5,
             height = yadd/maxydist,
             limitsize = FALSE)
    }  
    
    
    species <- subset(nyears, nyears$ManagedAreaName == i & nyears$nyr >= 5)$analysisunit
    
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
      
      if(paste0(p) %in% c("BB_pct", "PC")){
        
        formula_j <- as.formula(paste0(p, " ~ relyear"))
        
        set.seed(seed + n)
        model_j <- try(lme(formula_j,
                           random = list(LocationID = ~relyear),
                           na.action = na.omit, 
                           data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == j, ]), 
                       silent = TRUE)
        n <- n + 1
        x <- 0
        
        while(class(model_j) == "try-error" & x < 5){
          if(x %% 25 == 0) print(paste0("    Model failed, starting attempt ", x, " of 5"))
          
          set.seed(seed + n)
          model_j <- try(lme(formula_j,
                             random = list(LocationID = ~relyear),
                             na.action = na.omit, 
                             data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == j, ]),
                         silent = TRUE)
          n <- n + 1
          x <- x + 1
        }
        
        #Individual model objects are needed for plotting all species together
        eval(call("<-", as.name(paste0(gsub('\\b(\\pL)\\pL{2,}|.', '\\U\\1', i, perl = TRUE), 
                                       "_", 
                                       gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE), 
                                       gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1))))), 
                  model_j))
        
        #Save the model object as .rds
        saveRDS(model_j, here::here(paste0("SAV/output/models/SAV_", parameters[column == p, type], "_", 
                                           gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                           ifelse(stringr::str_detect(i, "NERR"), "ERR_lme_", 
                                                  ifelse(stringr::str_detect(i, "NMS"), "MS_lme_", "AP_lme_")), 
                                           gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),
                                           gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1))), 
                                           ".rds")))
        
        print(paste0("  Model object saved: ", 
                     gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                     "_", 
                     gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),
                     gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1)))))
        
        #record lme model results------------------------------------------------------
        if(class(try(eval(as.name(paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "_", gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),                                         gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1)))))), silent = TRUE)) != "try-error"){
          models <- append(models, as.name(paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "_", gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),                                         gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1))))))
          modj_i <- setDT(broom.mixed::tidy(eval(as.name(paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "_", gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),                                         gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1))))))))
          modj_i[, `:=` (managed_area = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                                               ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
                         species = j,
                         filename = paste0("SAV_", parameters[column == p, type], "_", gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                           ifelse(stringr::str_detect(i, "NERR"), "ERR_lme_", 
                                                  ifelse(stringr::str_detect(i, "NMS"), "MS_lme_", "AP_lme_")), 
                                           gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),
                                           gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1))), ".rds"))]
          lmemodresults <- rbind(lmemodresults, modj_i)
          
        } else{
          failedmod <- data.table(model = paste0("SAV_", parameters[column == p, type], "_",
                                                 gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE),
                                                 ifelse(stringr::str_detect(i, "NERR"), "ERR_lme_", 
                                                        ifelse(stringr::str_detect(i, "NMS"), "MS_lme_", "AP_lme_")), 
                                                 gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),
                                                 gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1))), ".rds"),
                                  error = model_j[1])
          
          failedmods <- rbind(failedmods, failedmod)
          
          modj_i <- data.table(managed_area = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                                                     ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
                               species = j,
                               filename = paste0("SAV_", parameters[column == p, type], "_", gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                 ifelse(stringr::str_detect(i, "NERR"), "ERR_lme_", 
                                                        ifelse(stringr::str_detect(i, "NMS"), "MS_lme_", "AP_lme_")), 
                                                 gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),
                                                 gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1))), ".rds"),
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
      if(paste0(p) == "BB_all") next
      
      #Indicator == "PO"--------------------------------------------------------
      if(paste0(p) == "PO") next #Temporarily blocking the percent occurrence analyses because the binomial model doesn't seem to fit the data very well. Will probably have to figure something else out.
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
      if(paste0(p) == "PA") next
    }
    
    #Final results tables and plots--------------------------------------------------------------------
    if(paste0(p) %in% c("BB_pct", "PC")){
      #Summarize # points per category
      plotdat <- SAV4[ManagedAreaName == i & !is.na(eval(p)), ] %>% group_by(analysisunit, relyear) %>% summarise(npt = n(), data = unique(eval(p)))
      setDT(plotdat)
      # if(usenames == "common"){
      #   plotdat[, analysisunit_ord := analysisunit][, analysisunit_ord := ordered(analysisunit_ord, levels = c("Turtle grass",
      #                                                                                                          "Shoal grass",
      #                                                                                                          "Widgeon grass",
      #                                                                                                          "Manatee grass",
      #                                                                                                          "Tape grasses",
      #                                                                                                          "Total seagrass",
      #                                                                                                          "Drift algae",
      #                                                                                                          "Attached algae",
      #                                                                                                          "Total_SAV"))]
      #   plotdat[, analysisunit := factor(analysisunit, levels = c("Turtle grass",
      #                                                              "Shoal grass",
      #                                                              "Widgeon grass",
      #                                                              "Manatee grass",
      #                                                              "Tape grasses",
      #                                                              "Total seagrass",
      #                                                              "Drift algae",
      #                                                              "Attached algae",
      #                                                              "Total_SAV"))]
      # } else{
      #   plotdat[, analysisunit_ord := analysisunit][, analysisunit_ord := factor(analysisunit_ord, ordered = TRUE, levels = c("Thalassia testudinum",
      #                                                                                                                         "Halodule wrightii",
      #                                                                                                                         "Ruppia maritima",
      #                                                                                                                         "Syringodium filiforme",
      #                                                                                                                         "Halophila spp.",
      #                                                                                                                         "Total seagrass",
      #                                                                                                                         "Drift algae",
      #                                                                                                                         "Attached algae",
      #                                                                                                                         "Total_SAV"))]
      # }
        
      #split modeled vs unmodeled data
      modeledsp <- c()
      for(u in seq_along(models)){
        if(usenames == "common"){
          name_u <- fcase(str_detect(models[[u]], "_ShGr"), "Shoal grass",
                          str_detect(models[[u]], "_TuGr"), "Turtle grass",
                          str_detect(models[[u]], "_MaGr"), "Manatee grass",
                          str_detect(models[[u]], "_WiGr"), "Widgeon grass",
                          str_detect(models[[u]], "_TaGr"), "Tape grasses",
                          str_detect(models[[u]], "_ToSe"), "Total seagrass",
                          str_detect(models[[u]], "_AtAl"), "Attached algae",
                          str_detect(models[[u]], "_DrAl"), "Drift algae",
                          str_detect(models[[u]], "_ToSA"), "Total SAV")
          modeledsp <- append(modeledsp, name_u)

        } else{
          name_u <- fcase(str_detect(models[[u]], "_ThTe"), "Thalassia testudinum",
                          str_detect(models[[u]], "_SyFi"), "Syringodium filiforme",
                          str_detect(models[[u]], "_HaWr"), "Halodule wrightii",
                          str_detect(models[[u]], "_RuMa"), "Ruppia maritima",
                          str_detect(models[[u]], "_HaSp"), "Halophila spp.",
                          str_detect(models[[u]], "_ToSe"), "Total seagrass",
                          str_detect(models[[u]], "_AtAl"), "Attached algae",
                          str_detect(models[[u]], "_DrAl"), "Drift algae",
                          str_detect(models[[u]], "_ToSA"), "Total SAV")
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
        miny_v <- predict(eval(models[[v]]), level = 0)
        miny <- append(miny, miny_v)
      }
      miny <- ifelse(floor(min(miny)) < 0, floor(min(miny)), 0)
      
      #create base plot of seagrass percent cover data over time for managed area i
      plot_i <- ggplot(data = droplevels(plotdat),
                       aes(x = relyear, y = data, size = npt)) + #SAV4[ManagedAreaName == i & !is.na(eval(p)), ]    size = npt   fill = analysisunit,   y = eval(p)
        geom_hline(yintercept = 0, color = "grey20") +
        geom_point(shape = 21, fill = "grey90", #stroke = 2,
                    alpha = 0.9, color = "grey50") + #fill = analysisunit  shape = 21, color = "#333333", , color = analysisunit   size = 0.5, width = 0.001, height = 0.3
        labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                            ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
             x = "Year",
             y = parameters[column == p, name],
             #color = "Species",
             #fill = "Species",
             size = "Number of\nobservations") +
        plot_theme +
        ylim(miny, 100) +
        scale_size_area(limits = c(1, max(plotdat$npt))) +
        # scale_color_manual(values = subset(spcols, names(spcols) %in% unique(plotdat[, analysisunit])), 
        #                    aesthetics = c("color", "fill")) +
        scale_x_continuous(breaks = c(seq(from = min(SAV4[ManagedAreaName == i & !is.na(eval(p)), relyear]),
                                          to = max(SAV4[ManagedAreaName == i & !is.na(eval(p)), relyear]),
                                          by = 3)),
                           labels = c(seq(from = min(SAV4[ManagedAreaName == i & !is.na(eval(p)), Year]),
                                          to = max(SAV4[ManagedAreaName == i & !is.na(eval(p)), Year]),
                                          by = 3)))
      
      if(length(models) > 0){
        plot_i <- addfits_blacktrendlines(models, plot_i, p) + 
          {if(usenames == "common"){
            facet_wrap(~factor(analysisunit, levels = c("Turtle grass",
                                                        "Shoal grass",
                                                        "Widgeon grass",
                                                        "Manatee grass",
                                                        "Tape grasses",
                                                        "Total seagrass",
                                                        "Drift algae",
                                                        "Attached algae",
                                                        "Total SAV")), 
                       ncol = 3, strip.position = "top")
          } else{
            facet_wrap(~factor(analysisunit, levels = c("Thalassia testudinum",
                                                        "Halodule wrightii",
                                                        "Ruppia maritima",
                                                        "Syringodium filiforme",
                                                        "Halophila spp.",
                                                        "Total seagrass",
                                                        "Drift algae",
                                                        "Attached algae",
                                                        "Total SAV")), 
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
    
    if(paste0(p) == "BB_all") next #{
    #   saveRDS(olrmodresults, here::here(paste0("SAV/output/tables/SAV_", parameters[column == p, type], "_", 
    #                                            ifelse(stringr::str_detect(i, "NERR"), 
    #                                                   paste0(str_sub(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 1, -2), "NERR_olrresults.rds"),
    #                                                   ifelse(stringr::str_detect(i, "NMS"),
    #                                                          paste0(str_sub(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 1, -2), "NMS_olrresults.rds"),
    #                                                          paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "AP_olrresults.rds"))))))
    # }
    
    if(paste0(p) == "PO") next #{
    #   saveRDS(blrmodresults, here::here(paste0("SAV/output/tables/SAV_", parameters[column == p, type], "_", 
    #                                            ifelse(stringr::str_detect(i, "NERR"), 
    #                                                   paste0(str_sub(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 1, -2), "NERR_blrresults.rds"), 
    #                                                   ifelse(stringr::str_detect(i, "NMS"),
    #                                                          paste0(str_sub(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 1, -2), "NMS_blrresults.rds"),
    #                                                          paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "AP_blrresults.rds"))))))
    # }
    
    if(paste0(p) == "PA"){
      #Bar chart of proportions by analysisunit
      breaks <- c(seq(min(SAV4[ManagedAreaName == i & !is.na(PA), relyear]),
                      max(SAV4[ManagedAreaName == i & !is.na(PA), relyear]),
                      by = 2))
      yrlist <- sort(unique(SAV4$Year))
      
      labels <- c()
      for(b in breaks){
        labels <- append(labels, yrlist[b + 1])
      }
      
      barplot_sp <- ggplot(data = props[ManagedAreaName == i, ], aes(x = relyear, y = sp_prop, fill = analysisunit)) +
        geom_col(color = "grey20") +
        scale_x_continuous(breaks = breaks, labels = labels) +
        plot_theme +
        labs(title = paste0(ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                                   ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve")))), 
             fill = "Species", 
             x = "Year", 
             y = "Proportion") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_color_manual(values = subset(spcols, names(spcols) %in% unique(props[ManagedAreaName == i, analysisunit])), 
                           aesthetics = c("color", "fill"))
      
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
                 gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', j, perl = TRUE),                                         gsub(x = gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), pattern = substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1), replacement = str_to_upper(substr(gsub('\\b(\\p{Ll}\\p{Ll})|.', '\\1', j, perl = TRUE), 1, 1)))))
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

toc()

#Save session info-----------------------------------------------------
session <- sessionInfo()
saveRDS(session, here::here(paste0("SAV/output/SessionInfo_", Sys.Date())))

