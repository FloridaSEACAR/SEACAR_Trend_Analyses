#The purpose of this script is to read the file names for the LME results of the BBpct analysis,
#import each one, extract the intercept, slope, and p values, then write them to a pipe-delimited file
library(data.table)
library(dplyr)
library(brms)
library(tidybayes)
library(stringr)
options(scipen = 999)

# Perform results compilation for both MA and OIMMP results, where available
for(analysis in c("ManagedAreaName", "OIMMP")){
  out_path <- paste0("output/", analysis, "/")
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
           c(eval(analysis), "ParameterName", "SizeClass", "ShellType", "HabitatType"))
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
  finalTable <- merge.data.frame(data_summ, table, by=c(eval(analysis), "ParameterName", 
                                                        "ShellType", "SizeClass", "HabitatType"), 
                                 all=TRUE) %>% 
    arrange(get(analysis), ParameterName, ShellType, SizeClass, HabitatType) %>% as.data.table()
  # Remove AreaID from OIMMP output
  if(analysis=="OIMMP"){
    finalTable <- finalTable %>% select(-AreaID)
  }
  
  ##### Model back-transformation procedures (previous model_backtransformation.R)
  # Function to perform percent change calculation
  get_percent_change <- function(mod, var = "full"){
    multi <- ifelse("LiveSuccess" %in% names(mod$data), 100, 1)
    pctplots <- plot(conditional_effects(mod, re_formula=NULL), plot=FALSE)
    df <- setDT(pctplots$RelYear$data)
    df$estimate_pct <- multi*df$estimate__
    delta_x <- diff(df$RelYear)
    delta_y <- diff(df$estimate_pct)
    delta_upper <- diff(df$upper__*multi)
    delta_lower <- diff(df$lower__*multi)
    delta_se <- diff(df$se__*multi)
    
    slopes <- delta_y / delta_x
    avg_slope <- mean(slopes)
    avg_upper <- mean(delta_upper / delta_x)
    avg_lower <- mean(delta_lower / delta_x)
    avg_se <- mean(delta_se / delta_x)
    
    out <- data.table("Abbreviation" = ma_abrev,
                      "ParameterName" = ind,
                      "HabitatType" = hab_type,
                      "Estimate" = avg_slope,
                      "StandardError" = mean(df$se__*multi),
                      "LowerConfidence" = avg_lower,
                      "UpperConfidence" = avg_upper,
                      "Intercept" = df[RelYear==min(mod$data$RelYear)]$estimate__*multi)
    return(out)
  }
  
  all_oyster_results <- fread(paste0(out_path, "GLMM_AllDates_ModelResults.csv"))
  model_list <- unique(all_oyster_results$filename)
  model_list <- str_subset(model_list, "_sh", negate = T) # shell height already in response scale, exclude here
  
  m_results <- data.table()
  for(mod in model_list){
    ma_abrev <- str_split_1(tail(str_split_1(mod, "/"),1),"_")[1]
    ind <- str_split_1(tail(str_split_1(mod, "/"),1),"_")[2]
    hab_type <- str_split_1(str_split_1(tail(str_split_1(mod, "/"),1),"_")[4], ".rds")[1]
    mod_i <- readRDS(mod)
    m_results <- bind_rows(m_results, get_percent_change(mod_i))
  }
  
  if(analysis=="ManagedAreaName"){
    backtrans_results <- merge(m_results, MA_All[, c("ManagedAreaName", "Abbreviation", "AreaID")])
  } else {
    backtrans_results <- m_results[, `:=` (ManagedAreaName = Abbreviation)]
  }
  backtrans_results$ParameterName <- case_when(backtrans_results$ParameterName=="den" ~ "Density",
                                               backtrans_results$ParameterName=="pct" ~ "Percent Live")
  
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
    ModelEstimate = replace_vals(get(analysis), ParameterName, HabitatType, "est_val"),
    StandardError = replace_vals(get(analysis), ParameterName, HabitatType, "se_val"),
    LowerConfidence = replace_vals(get(analysis), ParameterName, HabitatType, "lc_val"),
    UpperConfidence = replace_vals(get(analysis), ParameterName, HabitatType, "uc_val"),
    Intercept = replace_vals(get(analysis), ParameterName, HabitatType, "int_val")
  )
  
  finalTable <- rbind(mod_subset, unmod_subset) %>%
    arrange(get(analysis), ParameterName, ShellType, SizeClass, HabitatType)
  
  #Write output table to a csv and pipe-delimited txt file
  fwrite(finalTable, paste0(out_path, "Oyster_All_GLMM_Stats.txt"), sep="|")
  fwrite(finalTable, paste0(out_path, "Oyster_All_GLMM_Stats.csv"), sep=",")
}

# 
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