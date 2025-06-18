#The purpose of this script is to read the file names for the LME results of the BBpct analysis,
#import each one, extract the intercept, slope, and p values, then write them to a pipe-delimited file
library(data.table)
library(dplyr)
options(scipen = 999)

#List all of the files in the "tables" directory that are LME results
file_list <- list.files("output", pattern="ModelResults", full.names=TRUE)

#Include only those that are txt
file_in <- file_list[grep("csv", file_list)]

#Set output directory
out_dir <- "output"

#Read in file
data <- fread(file_in, sep = ",", header = TRUE, stringsAsFactors = FALSE, na.strings = "")

#Keep only rows that are values with "fixed" in the effect column
data <- data[data$effect=="fixed" & !is.na(data$effect),]

#For each managed area and species, get the LME intercept, slope, and p values
table <- data %>%
      group_by(managed_area, indicator, live_date_qual, size_class, habitat_class) %>%
      dplyr::reframe(
        Intercept = estimate[term == "(Intercept)"],
        ModelEstimate = estimate[term == "RelYear" | 
                                   term == "meRelYearSampleAge_StdevgrEQQuadIdentifier"],
        StandardError = std.error[term == "RelYear" | 
                                    term == "meRelYearSampleAge_StdevgrEQQuadIdentifier"],
        LowerConfidence = conf.low[term == "RelYear" | 
                                     term == "meRelYearSampleAge_StdevgrEQQuadIdentifier"],
        UpperConfidence = conf.high[term == "RelYear" | 
                                      term == "meRelYearSampleAge_StdevgrEQQuadIdentifier"])

#Change column names to better match other outputs
setnames(table, c("managed_area", "indicator", "size_class", "live_date_qual", "habitat_class"),
         c("ManagedAreaName", "ParameterName", "SizeClass", "ShellType", "HabitatType"))

table$ShellType[table$ShellType=="Exact"] <- "Live Oysters"
table$ShellType[table$ShellType=="Estimate"] <- "Dead Oyster Shells"

table$ParameterName[table$ParameterName=="Size class"] <- "Shell Height"
table$ParameterName[table$ParameterName=="Percent live"] <- "Percent Live"

# Load data summaries
# List of parameter folder names
param_dirs <- c("Percent_Live", "Shell_Height", "Density")
# Loops through each parameter directory and gets list of Overall stats files
for(i in 1:length(param_dirs)){
  param <- paste0("output/",param_dirs[i])
  if(i==1){
    files<-list.files(param, pattern="Overall", full.name=TRUE)
  } else{
    temp <- list.files(param, pattern="Overall", full.name=TRUE)
    files <- append(files, temp)
  }
}


for(i in 1:length(files)){
  if(i==1){
    data_summ <- fread(files[i], sep="|", header=TRUE, stringsAsFactors=FALSE,
                  na.strings="")
  } else{
    data_summ <- bind_rows(data_summ,
                      fread(files[i], sep="|", header=TRUE,
                            stringsAsFactors=FALSE, na.strings=""))
  }
}

data_summ$ParameterName[data_summ$ParameterName=="ShellHeight_mm"] <- "Shell Height"
data_summ$ParameterName[data_summ$ParameterName=="Density_m2"] <- "Density"
data_summ$ParameterName[data_summ$ParameterName=="PercentLive_pct"] <- "Percent Live"
data_summ$ShellType[data_summ$ShellType=="Live Oyster Shells"] <- "Live Oysters"


finalTable <- merge.data.frame(data_summ, table, by=c("ManagedAreaName", "ParameterName",
                                                 "ShellType", "SizeClass", "HabitatType"),
                             all=TRUE)

finalTable <- as.data.table(finalTable[order(finalTable$ManagedAreaName, finalTable$ParameterName,
                                             finalTable$ShellType, finalTable$SizeClass,
                                             finalTable$HabitatType), ])
finalTable <- finalTable %>% dplyr::select(AreaID, everything())

# Import results from back-transformation of models
source("model_backtransformation.R")
# Read in resulting .csv from the above transformation
backtrans_results <- fread("output/model_transformations.csv")

replace_vals <- function(ma, param, hab_type, ret){
  subset <- backtrans_results[ManagedAreaName==ma & ParameterName==param & 
                                HabitatType==hab_type, ]
  est_val <- subset$Estimate
  se_val <- subset$StandardError
  lc_val <- subset$LowerConfidence
  uc_val <- subset$UpperConfidence
  int_val <- subset$Intercept
  return(get(ret))
}

mod_subset <- finalTable[SufficientData==TRUE & ParameterName %in% c("Density", "Percent Live"), ]
unmod_subset <- setdiff(finalTable, mod_subset)
# Replace values where needed
mod_subset <- mod_subset %>% rowwise() %>% mutate(
  ModelEstimate = replace_vals(ManagedAreaName, ParameterName, HabitatType, "est_val"),
  StandardError = replace_vals(ManagedAreaName, ParameterName, HabitatType, "se_val"),
  LowerConfidence = replace_vals(ManagedAreaName, ParameterName, HabitatType, "lc_val"),
  UpperConfidence = replace_vals(ManagedAreaName, ParameterName, HabitatType, "uc_val"),
  Intercept = replace_vals(ManagedAreaName, ParameterName, HabitatType, "int_val")
)

finalTable <- rbind(mod_subset, unmod_subset)

finalTable <- as.data.table(finalTable[order(finalTable$ManagedAreaName, finalTable$ParameterName,
                                             finalTable$ShellType, finalTable$SizeClass,
                                             finalTable$HabitatType), ])
finalTable <- finalTable %>% dplyr::select(AreaID, everything())

#Write output table to a csv and pipe-delimited txt file
fwrite(finalTable, "output/Oyster_All_GLMM_Stats.txt", sep="|")
fwrite(finalTable, "output/Oyster_All_GLMM_Stats.csv", sep=",")

###### Compile data used for plots
# Date of latest script run (to ensure the proper data is collected)
runDate <- "2025-05-02"
#List all of the files in the "Tables" directory that are Shell Heights
file_list <- list.files("output/model_results/data", pattern="_sh", full.names=TRUE)
file_list <- str_subset(file_list, runDate)

for(i in 1:length(file_list)){
  file_name <- tail(str_split_1(file_list[i], "/"),1)
  if(i==1){
    data <- readRDS(file_list[i])
    data$ProgramID <- as.character(data$ProgramID)
    data$sourceRDS <- file_name
  } else{
    temp_data <- readRDS(file_list[i])
    temp_data$ProgramID <- as.character(temp_data$ProgramID)
    temp_data$sourceRDS <- file_name
    data <- bind_rows(data, temp_data)
  }
}

#Write output table to a csv and pipe-delimited txt file
fwrite(data, "output/Shell_Height/Oyster_SH_plotdata.txt", sep="|")
fwrite(data, "output/Shell_Height/Oyster_SH_plotdata.csv", sep=",")


#List all of the files in the "tables" directory that are Density
file_list <- list.files("output/model_results/data", pattern="_density_", full.names=TRUE)
file_list <- str_subset(file_list, runDate)

for(i in 1:length(file_list)){
  file_name <- tail(str_split_1(file_list[i], "/"),1)
  if(i==1){
    data <- readRDS(file_list[i])
    data$ProgramID <- as.character(data$ProgramID)
    data$sourceRDS <- file_name
  } else{
    temp_data <- readRDS(file_list[i])
    temp_data$ProgramID <- as.character(temp_data$ProgramID)
    temp_data$sourceRDS <- file_name
    data <- bind_rows(data, temp_data)
  }
}

#Write output table to a csv and pipe-delimited txt file
fwrite(data, "output/Density/Oyster_Den_plotdata.txt", sep="|")
fwrite(data, "output/Density/Oyster_Den_plotdata.csv", sep=",")

#List all of the files in the "tables" directory that are Density
file_list <- list.files("output/model_results/data", pattern="_PrcLive_", full.names=TRUE)
file_list <- str_subset(file_list, runDate)
file_list <- str_subset(file_list, "_binom_", negate=TRUE)

for(i in 1:length(file_list)){
  file_name <- tail(str_split_1(file_list[i], "/"),1)
  if(i==1){
    data <- readRDS(file_list[i])
    data$ProgramID <- as.character(data$ProgramID)
    data$sourceRDS <- file_name
  } else{
    temp_data <- readRDS(file_list[i])
    temp_data$ProgramID <- as.character(temp_data$ProgramID)
    temp_data$sourceRDS <- file_name
    data <- bind_rows(data, temp_data)
  }
}

#Write output table to a csv and pipe-delimited txt file
fwrite(data, "output/Percent_Live/Oyster_Pct_plotdata.txt", sep="|")
fwrite(data, "output/Percent_Live/Oyster_Pct_plotdata.csv", sep=",")
