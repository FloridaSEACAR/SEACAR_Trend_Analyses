#This script is designed to read the file names for the LME results of the BBpct analysis,
#import each one, extract the intercept, slope, and p values, then write them to a pipe-delimited file
library(data.table)
library(dplyr)

#List all of the files in the "tables" directory that are LME results
files <- list.files("SAV/output/tables", pattern="lmeresults", full.names=TRUE)

#Include only those that are BBpct
files <- files[grep("BBpct", files)]

#For loop cycles through each file name
for (i in 1:length(files)) {
   #Get filename from list
   filename <- files[i]
   
   #Read in file
   table <- readRDS(filename)
   
   #Keep only rows that are values with "fixed" in the effect column
   table <- table[table$effect=="fixed" & !is.na(table$effect),]
   
   #For each managed area and species, get the LME intercept, slope, and p values
   table <- table %>%
      group_by(managed_area, species) %>%
      summarise(LME_Intercept = estimate[term == "(Intercept)"],
                LME_Slope = estimate[term == "relyear"],
                p = p.value[term == "relyear"])
   
   #If this is the first file, the table from above is stored as the output table
   #If not the first file, the table is added to the end of the output table
   if(i==1) {
      output <- table
   } else {
      output <- bind_rows(output, table)
   }
}

#Change column names to better match other outputs
setnames(output, c("managed_area", "species"), c("ManagedAreaName", "Species"))

output$ManagedAreaName[output$ManagedAreaName=="Fort Pickens Aquatic Preserve"] <-
   "Fort Pickens State Park Aquatic Preserve"

output$ManagedAreaName[output$ManagedAreaName=="St. Andrews Aquatic Preserve"] <-
   "St. Andrews State Park Aquatic Preserve"

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
                na.strings = "")
MA_All[, MAName := ManagedAreaName]

# stats <- fread("SAV/output/data/SAV_BBpct_Stats.txt", sep = "|", header = TRUE, stringsAsFactors = FALSE,
#                na.strings = "")

stats <- openxlsx::read.xlsx(here::here("SAV/output/data/SAV_BBpct_PA_Stats.xlsx"), sheet = 1)

setnames(stats, c("ManagedAreaName", "analysisunit"), c("ShortName","Species"))

stats <- merge.data.frame(MA_All[,c("AreaID", "ManagedAreaName", "ShortName")],
                          stats, by="ShortName", all=TRUE)

stats$ShortName <- NULL

stats <-  merge.data.frame(stats, output,
                              by=c("ManagedAreaName", "Species"), all=TRUE)


stats <- merge.data.frame(MA_All[,c("AreaID", "ManagedAreaName")],
                         stats, by=c("AreaID", "ManagedAreaName"), all=TRUE)

stats <- as.data.table(stats[order(stats$ManagedAreaName, stats$Species), ])

stats$EarliestYear[stats$EarliestYear=="Inf"] <- NA
stats$LatestYear[stats$LatestYear=="-Inf"] <- NA

sp_abbrevs <- data.table(name = c("Shoal grass", "Turtle grass", "Manatee grass", "Widgeon grass", "Paddle grass", "Star grass", "Johnson's seagrass", 
                                  "Unidentified Halophila", "Halophila spp.", "Total seagrass", "Attached algae", "Drift algae","Total SAV", 
                                  "Thalassia testudinum", "Syringodium filiforme", "Halodule wrightii", "Ruppia maritima", "Halophila decipiens", 
                                  "Halophila engelmannii", "Halophila johnsonii"),
                         abbrev = c("_ShGr", "_TuGr", "_MaGr", "_WiGr", "_PaGr", "_StGr", "_JoSe", "_UnHa", "_HaSp", "_ToSe", "_AtAl", "_DrAl", "_To", 
                                    "_ThTe", "_SyFi", "_HaWr", "_RuMa", "_HaDe", "_HaEn", "_HaJo"))

setDT(stats)
for(row in seq(1:nrow(stats))){
  stats$Model_Failed[row] <- ifelse(TRUE %in% str_detect(failedmodslist[str_detect(model, "_BBpct_"), model], 
                                                         paste0(MA_All[MAName == stats[row, ManagedAreaName], Filenames_SAVscript], "_lme", sp_abbrevs[name == stats[row, Species], abbrev])), 
                                    TRUE, FALSE)
}
stats[is.na(ParameterName), Model_Failed := NA]

#Write output table to a pipe-delimited txt file
fwrite(stats, paste0("SAV/output/website/SAV_BBpct_LMEresults_All_", Sys.Date(), ".txt"), sep="|")

#Save failedmodslist to the same location
fwrite(failedmodslist, paste0("SAV/output/website/SAV_Failedmodslist_All_", Sys.Date(), ".txt"), sep="|")

#Save xlsx version
sheets <- list("SAV LME results" = stats, "Failed models" = failedmodslist[str_detect(model, "_BBpct_"), ])
openxlsx::write.xlsx(sheets, here::here(paste0("SAV/output/website/SAV_BBpct_LMEresults_All_", Sys.Date(), ".xlsx")), colNames = c(TRUE, TRUE), colWidths = c("auto", "auto"), firstRow = c(TRUE, TRUE))
