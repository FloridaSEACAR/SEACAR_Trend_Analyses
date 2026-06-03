# Model back-transformation procedures are no longer needed (06-02-2026)
# Estimates are pulled directly from model draws during model plotting procedures
library(data.table)
library(dplyr)
library(brms)
library(tidybayes)
library(stringr)
options(scipen = 999)

# Function to apply TrendIcon and TrendText for display on the Atlas
oyster_apply_trend_icon <- function(suffData, modelEstimate, lowConfidence, upConfidence, sizeClass, shellType){
  increasing <- modelEstimate > 0
  trendPresent <- (lowConfidence < 0 & upConfidence < 0) | (lowConfidence > 0 & upConfidence >0)
  TrendIcon <- 0
  TrendText <- "No detectable trend"
  if(isTRUE(trendPresent)){
    TrendIcon <- ifelse(increasing, 1, -1)
    TrendText <- ifelse(increasing, "Increasing trend", "Decreasing trend")
  }
  if(!suffData){
    TrendIcon <- 2
    TrendText <- "Insufficient data"
  } else if(suffData & is.na(modelEstimate) & is.na(sizeClass)){
    TrendIcon <- 3
    TrendText <- "Model did not fit the available data"
    # if(shellType=="Dead Oyster Shells"){
    #   TrendIcon <- 4
    #   TrendText <- "Model not run on dead oyster shell"
    # }
  }
  return(list(
    "TrendIcon" = TrendIcon,
    "TrendText" = TrendText
  ))
}

# Perform results compilation for both MA and OIMMP results, where available
# for(analysis_column in c("ManagedAreaName", "OIMMP")){
for(analysis_column in c("ManagedAreaName")){
  out_path <- paste0("output/", analysis_column, "/")
  #List all of the files in the "tables" directory that are LME results
  file_list <- list.files(out_path, pattern="ModelResults", full.names=TRUE)
  # Determine which analyses have been performed (skip if unavailable)
  if(length(file_list)==0) next
  #Include only those that are txt
  file_in <- file_list[grep("csv", file_list)]
  #Read in file
  data <- fread(file_in, sep = ",", header = TRUE, stringsAsFactors = FALSE, na.strings = "")
  
  # Load data summaries
  sum_files <- list.files(out_path, pattern = "Overall", full.name=TRUE)
  data_summ <- bind_rows(lapply(sum_files, fread))
  
  data_summ$ParameterName[data_summ$ParameterName=="ShellHeight_mm"] <- "Shell Height"
  data_summ$ParameterName[data_summ$ParameterName=="Density_m2"] <- "Density"
  data_summ$ParameterName[data_summ$ParameterName=="PercentLive_pct"] <- "Percent Live"
  # Combine results
  if(analysis_column=="ManagedAreaName"){
    merge_by_cols <- c("ManagedAreaName", "AreaID", "ParameterName", "ShellType", "SizeClass", "HabitatType")
  } else {
    merge_by_cols <- c("OIMMP", "ParameterName", "ShellType", "SizeClass", "HabitatType")
  }
  
  finalTable <- data %>% merge(data_summ, by = merge_by_cols, all=T) %>% 
    select(!!sym(analysis_column), ParameterName, SizeClass, HabitatType, ShellType,
           N_Years, SufficientData, EarliestLiveDate, LatestLiveDate, LastSampleDate,
           N_Data, Min, Max, Median, Mean, StandardDeviation, Programs, ProgramIDs,
           everything()) %>% 
    # mutate(SizeClass = ifelse(SizeClass=="", NA, SizeClass)) %>%
    arrange(get(analysis_column), ParameterName, ShellType, SizeClass, HabitatType) %>%
    as.data.table()
  
  if(analysis_column=="ManagedAreaName"){
    finalTable <- finalTable %>% select(AreaID, everything()) # Put AreaID first
  }
  
  #### Add icon-based trends + trend text columns
  finalTable <- finalTable %>% rowwise() %>%
    mutate(TrendIcon = oyster_apply_trend_icon(SufficientData, ModelEstimate, 
                                               LowerConfidence, UpperConfidence, 
                                               SizeClass, ShellType)$TrendIcon,
           TrendText = oyster_apply_trend_icon(SufficientData, ModelEstimate, 
                                               LowerConfidence, UpperConfidence, 
                                               SizeClass, ShellType)$TrendText)
  
  #Write output table to a csv and pipe-delimited txt file
  fwrite(finalTable, paste0(out_path, "Oyster_All_GLMM_Stats.txt"), sep="|")
  fwrite(finalTable, paste0(out_path, "Oyster_All_GLMM_Stats.csv"), sep=",")
}

#   ###### Compile data used for plots
#   # Date of latest script run (to ensure the proper data is collected)
#   runDate <- "2025-05-02"
#   #List all of the files in the "Tables" directory that are Shell Heights
#   file_list <- list.files("output/model_results/data", pattern="_sh", full.names=TRUE)
#   file_list <- str_subset(file_list, runDate)
#   
#   for(i in 1:length(file_list)){
#     file_name <- tail(str_split_1(file_list[i], "/"),1)
#     if(i==1){
#       data <- readRDS(file_list[i])
#       data$ProgramID <- as.character(data$ProgramID)
#       data$sourceRDS <- file_name
#     } else{
#       temp_data <- readRDS(file_list[i])
#       temp_data$ProgramID <- as.character(temp_data$ProgramID)
#       temp_data$sourceRDS <- file_name
#       data <- bind_rows(data, temp_data)
#     }
#   }
#   
#   #Write output table to a csv and pipe-delimited txt file
#   fwrite(data, "output/Shell_Height/Oyster_SH_plotdata.txt", sep="|")
#   fwrite(data, "output/Shell_Height/Oyster_SH_plotdata.csv", sep=",")
#   
#   
#   #List all of the files in the "tables" directory that are Density
#   file_list <- list.files("output/model_results/data", pattern="_density_", full.names=TRUE)
#   file_list <- str_subset(file_list, runDate)
#   
#   for(i in 1:length(file_list)){
#     file_name <- tail(str_split_1(file_list[i], "/"),1)
#     if(i==1){
#       data <- readRDS(file_list[i])
#       data$ProgramID <- as.character(data$ProgramID)
#       data$sourceRDS <- file_name
#     } else{
#       temp_data <- readRDS(file_list[i])
#       temp_data$ProgramID <- as.character(temp_data$ProgramID)
#       temp_data$sourceRDS <- file_name
#       data <- bind_rows(data, temp_data)
#     }
#   }
#   
#   #Write output table to a csv and pipe-delimited txt file
#   fwrite(data, "output/Density/Oyster_Den_plotdata.txt", sep="|")
#   fwrite(data, "output/Density/Oyster_Den_plotdata.csv", sep=",")
#   
#   #List all of the files in the "tables" directory that are Density
#   file_list <- list.files("output/model_results/data", pattern="_PrcLive_", full.names=TRUE)
#   file_list <- str_subset(file_list, runDate)
#   file_list <- str_subset(file_list, "_binom_", negate=TRUE)
#   
#   for(i in 1:length(file_list)){
#     file_name <- tail(str_split_1(file_list[i], "/"),1)
#     if(i==1){
#       data <- readRDS(file_list[i])
#       data$ProgramID <- as.character(data$ProgramID)
#       data$sourceRDS <- file_name
#     } else{
#       temp_data <- readRDS(file_list[i])
#       temp_data$ProgramID <- as.character(temp_data$ProgramID)
#       temp_data$sourceRDS <- file_name
#       data <- bind_rows(data, temp_data)
#     }
#   }
#   
#   #Write output table to a csv and pipe-delimited txt file
#   fwrite(data, "output/Percent_Live/Oyster_Pct_plotdata.txt", sep="|")
#   fwrite(data, "output/Percent_Live/Oyster_Pct_plotdata.csv", sep=",")