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
  } else if(suffData & is.na(modelEstimate) & sizeClass!=""){
    TrendIcon <- 3
    TrendText <- "Model did not fit the available data"
    if(shellType=="Dead Oyster Shells"){
      TrendIcon <- 4
      TrendText <- "Model not run on dead oyster shell"
    }
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
  #Keep only rows that are values with "fixed" in the effect column
  data <- data[data$effect=="fixed" & !is.na(data$effect),]
  #For each managed area and species, get the LME intercept, slope, and p values
  table <- data %>%
    group_by(areaName, indicator, live_date_qual, size_class, habitat_class) %>%
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
  setnames(table, c("areaName", "indicator", "size_class", "live_date_qual", "habitat_class"),
           c(eval(analysis_column), "ParameterName", "SizeClass", "ShellType", "HabitatType"))
  # Ensure proper display of parameter names
  table$ShellType[table$ShellType=="Exact"] <- "Live Oysters"
  table$ShellType[table$ShellType=="Estimate"] <- "Dead Oyster Shells"
  table$ParameterName[table$ParameterName=="Size class"] <- "Shell Height"
  table$ParameterName[table$ParameterName=="Percent live"] <- "Percent Live"
  # Load data summaries
  sum_files <- list.files(out_path, pattern = "Overall", full.name=TRUE)
  data_summ <- bind_rows(lapply(sum_files, fread))
  
  data_summ$ParameterName[data_summ$ParameterName=="ShellHeight_mm"] <- "Shell Height"
  data_summ$ParameterName[data_summ$ParameterName=="Density_m2"] <- "Density"
  data_summ$ParameterName[data_summ$ParameterName=="PercentLive_pct"] <- "Percent Live"
  data_summ$ShellType[data_summ$ShellType=="Live Oyster Shells"] <- "Live Oysters"
  # Combine results
  finalTable <- merge.data.frame(data_summ, table, by=c(eval(analysis_column), "ParameterName", 
                                                        "ShellType", "SizeClass", "HabitatType"), 
                                 all=TRUE) %>% 
    arrange(get(analysis_column), ParameterName, ShellType, SizeClass, HabitatType) %>% as.data.table()
  
  ##### Model back-transformation procedures (previous model_backtransformation.R)
  # Model results from all 3 parameters are processed using "model_extracts" 
  # These contain the prior draws + the data used to create the model representations on the plot
  # Perform percent change calculation
  all_oyster_results <- fread(paste0(out_path, "GLMM_AllDates_ModelResults.csv"))
  all_models <- all_oyster_results[, unique(filename)]
  # Find "model_extract" files to be paired with each model
  all_model_results <- list.files(paste0(out_path, "model_results/model_extracts/"), full=T)
  
  m_results <- data.table()
  for(model in all_models){
    ma_abrev <- str_split_1(tail(str_split_1(model, "/"),1),"_")[1]
    hab_type <- str_split_1(str_split_1(tail(str_split_1(model, "/"),1),"_")[4], ".rds")[1]
    indicator <- str_split_1(tail(str_split_1(model, "/"),1),"_")[[2]]
    if(str_detect(indicator, "sh25|sho")){
      ind_pattern <- "SH"
      parameter_name <- "Shell Height"
      size_class <- ifelse(str_detect(indicator, "sh25"), "25-75mm", ">75mm")
      model_extract_pattern <- paste0(ind_pattern, "_", ma_abrev, "_", hab_type, "_", gsub("sh", "", indicator))
    } else if(str_detect(indicator, "den")){
      ind_pattern <- "Dens"
      parameter_name <- "Density"
      size_class <- NA
      model_extract_pattern <- paste0(ind_pattern, "_", ma_abrev, "_", hab_type)
    } else {
      ind_pattern <- "PrcLive"
      parameter_name <- "Percent Live"
      size_class <- NA
      model_extract_pattern <- paste0(ind_pattern, "_", ma_abrev, "_", hab_type)
    }
    model_results <- readRDS(str_subset(all_model_results, model_extract_pattern))
    # Extract trend draws from model outputs
    model_summary <- setDT(model_results$summary)
    trend_draws <- model_results$draws
    rel_values <- model_results$rel_values
    
    # Ensure no Inf results
    finite_draws <- apply(trend_draws, 1, function(x) all(is.finite(x)))
    trend_draws <- trend_draws[finite_draws, , drop = FALSE]
    
    ## Calculate overall average change per year
    # Calculate the change separately for each posterior draw
    rate_draws <- (trend_draws[, ncol(trend_draws)] - trend_draws[, 1]) / (max(rel_values) - min(rel_values))
    rate_summary <- data.table(
      Estimate = mean(rate_draws, na.rm = TRUE),
      StandardError = sd(rate_draws, na.rm = TRUE),
      LowerConfidence = unname(quantile(rate_draws, 0.025, na.rm = TRUE)),
      UpperConfidence = unname(quantile(rate_draws, 0.975, na.rm = TRUE))
    )
    
    # Gather intercept from model data
    intercept <- model_summary[RelYear==min(RelYear), estimate__]
    
    out <- data.table("Abbreviation" = ma_abrev,
                      "ParameterName" = parameter_name,
                      "SizeClass" = size_class,
                      "HabitatType" = hab_type,
                      "Intercept" = intercept,
                      "ModelEstimate" = rate_summary$Estimate,
                      "StandardError" = rate_summary$StandardError,
                      "LowerConfidence" = rate_summary$LowerConfidence,
                      "UpperConfidence" = rate_summary$UpperConfidence)
    
    m_results <- bind_rows(m_results, out)
  }
  
  if(analysis_column=="ManagedAreaName"){
    backtrans_results <- merge(m_results, MA_All[, c("ManagedAreaName", "Abbreviation", "AreaID")])
  } else {
    backtrans_results <- m_results[, `:=` (ManagedAreaName = Abbreviation)]
  }
  
  # Select only entries with model results
  mod_subset <- finalTable[!is.na(ModelEstimate), ]
  # Split out entries which will not be modified
  unmod_subset <- setdiff(finalTable, mod_subset)
  
  mod_subset <- merge(
    mod_subset %>% select(-c(Intercept, ModelEstimate, StandardError, LowerConfidence, UpperConfidence)),
    backtrans_results %>% select(-Abbreviation)
  )
  
  finalTable <- rbind(mod_subset, unmod_subset) %>% as.data.frame() %>%
    arrange(get(analysis_column), ParameterName, ShellType, SizeClass, HabitatType)
  
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