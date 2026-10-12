# The purpose of this script is to wrangle and analyze continuous WC data using Seasonal Kendall tau.
# Created by J.E. Panzik (jepanzik@usf.edu) for SEACAR
# Modified by T.G. Hill in September, 2023

## WHEN RUNNING IN RSTUDIO:
## Set working directory to "Source File Location" in "Session" menu
Start_time <- Sys.time()

#Load libraries
library(data.table)
library(knitr)
library(readr)
library(dplyr)
library(lubridate)
library(rstudioapi)
library(tictoc)
library(ggplot2)
library(ggpubr)
library(scales)
library(EnvStats)
library(tidyr)
library(kableExtra)
library(stringr)
library(polars)

source("../SEACAR_data_location.R")

tic()

#Set output directory
out_dir <- "output"

out_dir_tables <- "output/tables/cont"

#Set number of unique years a location must have to be considered for analysis
suff_years <- 5

#Sets list of parameter abbreviation names # updated 03-17-2026
all_params_short <- c(
  "Dissolved Oxygen" = "DO",
  "Dissolved Oxygen Saturation" = "DOS",
  "pH" = "pH",
  "Salinity" = "Sal",
  "Turbidity" = "Turb",
  "Water Temperature" = "TempW",
  "Fluorescent Dissolved Organic Matter" = "FDOM",
  "Specific Conductivity" = "SpCond",
  "Chlorophyll a, Uncorrected for Pheophytin" = "Chla"
)

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- SEACAR::ManagedAreas

# Creates folders for outputs
folder_paths <- c("output/tables","output/tables/cont", "output/models/cont")
for(path in folder_paths) {if(!dir.exists(path)){dir.create(path, recursive = T)}}

cont_files_short <- data.frame()
cont_stations <- data.frame()
coordinates_df <- data.frame()
raw_cont_models <- list() # store raw model outputs for model aggregation

# Use the below line for most recent exports
file_list <- list.files(seacar_data_location, full.names = T)
cont_file_list <- str_subset(file_list, "_cont_")

# Creates function to check monitoring location for at least 2 years of
# continuous consecutive data
ContinuousConsecutiveCheck_polars <- function(con_data){
  empty_ids <- con_data$MonitoringID[0]
  
  base <- as_polars_lf(con_data)$
    filter(pl$col("Include") == TRUE)$
    select("MonitoringID", "Year", "Month")$
    unique()
  
  y1 <- base$
    select(
      "MonitoringID",
      "Month",
      pl$col("Year")$alias("Year1")
    )
  
  y2 <- base$
    select(
      "MonitoringID",
      "Month",
      pl$col("Year")$alias("Year2")
    )
  
  result <- y1$
    join(
      y2,
      on = c("MonitoringID", "Month"),
      how = "inner"
    )$
    filter(pl$col("Year2") == pl$col("Year1") + 1)$
    group_by("MonitoringID", "Year1", "Year2")$
    agg(
      pl$col("Month")$count()$alias("n_shared_months")
    )$
    filter(pl$col("n_shared_months") >= 2)$
    select("MonitoringID")$
    unique()$
    collect() |>
    as.data.frame()
  
  if(nrow(result) == 0){
    return(empty_ids)
  }
  result$MonitoringID
}

# Loop through all available continuous files
for(file in cont_file_list[1:8]){
  # Read in data
  data <- fread(file, sep = "|", na.strings = "NULL")
  # Gather full parameter name
  p <- unique(data$ParameterName)
  
  # Gather units
  unit <- unique(data$ParameterUnits)
  # Gather abbreviated parameter name (for file outputs)
  param_abrev <- all_params_short[[p]]
  cat(paste0("Starting parameter: ", p, "  \n\n"))
  # Shortened filename for use within reports
  file_short <- tail(str_split_1(file, "/"), 1)
  cont_files_short <- bind_rows(cont_files_short, data.frame("ParameterName" = p,
                                                             "file_short" = file_short))
  cat("Using file", file_short, "\n")
  
  ############################
  ### FILTERING & CLEANING ###
  ############################
  
  # Separate reference file to prevent having to de-concatenate ManagedAreaName on entire continuous dataset
  # This is a crosswalk of individual MonitoringID and ProgramLocationID associations by ManagedAreaName
  ma_ref <- data %>% group_by(AreaID, ManagedAreaName, ProgramID, ProgramName, ProgramLocationID) %>%
    reframe(MonitoringID = cur_group_id())
  # Subset for stations with NA ManagedAreaName to keep them in analysis
  ma_ref_subset <- ma_ref %>% filter(is.na(ManagedAreaName))
  # De-concatenate to make ManagedAreaName associations & recombine
  ma_ref <- rbind(ma_ref_subset, SEACAR::clean_managed_areas(ma_ref, "ma")) %>% as.data.table()
  rm(ma_ref_subset)
  # Rename NA managed areas to character-based NA
  ma_ref[is.na(ManagedAreaName), `:=` (ManagedAreaName = "NA")]
  ma_ref[is.na(AreaID), `:=` (AreaID = "NA")]
  # Ensure ma_ref only has unique keys to prevent row duplication
  ma_unique <- unique(ma_ref[, .(ProgramLocationID, MonitoringID)])
  # Merge MonitoringID values back into dataset (uses update-on-join syntax for better memory efficiency)
  data[
    ma_unique,
    on = .(ProgramLocationID),
    MonitoringID := i.MonitoringID
  ]
  
  # Converts Include to be a logical either TRUE or FALSE
  data[, Include := as.logical(Include)]
  # Removes any data rows that do not have Include set to TRUE
  data <- data[Include == TRUE]
  
  # Removes rows that have missing ResultValues
  # Removes rows that have missing RelativeDepth
  # Removes rows that have an ActivityType with Blank
  data <- data[!is.na(ResultValue) & !is.na(RelativeDepth) & !grepl("Blank", ActivityType)]
  
  # Stores the MonitoringID that pass the consecutive year check
  consMonthIDs <- ContinuousConsecutiveCheck_polars(data)
  # Creates data frame with summary for each monitoring location.
  median_na_rm <- function(x) ifelse(length(x) > 0, median(x, na.rm = TRUE), NA_real_)
  
  Mon_Summ <- data[, .(
    RelativeDepth = unique(RelativeDepth),
    N_Data = sum(Include == TRUE & !is.na(ResultValue)),
    N_Years = uniqueN(Year[Include == TRUE & !is.na(Year)]),
    EarliestYear = min(Year[Include == TRUE]),
    LatestYear = max(Year[Include == TRUE]),
    EarliestSampleDate = min(SampleDate[Include == TRUE]),
    LastSampleDate = max(SampleDate[Include == TRUE]),
    Median = median_na_rm(ResultValue)
  ), by = .(MonitoringID, AreaID, ManagedAreaName, ProgramID, ProgramName, ProgramLocationID, ParameterName)]
  
  Mon_Summ[, ConsecutiveMonths := MonitoringID %in% consMonthIDs]
  Mon_Summ[, SufficientData := N_Data > 0 & N_Years >= suff_years & ConsecutiveMonths]
  Mon_Summ[, ConsecutiveMonths := NULL]
  
  # Puts summary data in order based on MonitoringID
  Mon_Summ <- as.data.table(Mon_Summ[order(Mon_Summ$MonitoringID), ])
  
  # Creates column in data that determines how many years from the start for each
  # Monitoring location
  data[, YearFromStart := Year - min(Year), by = MonitoringID]
  # Adds SufficientData column to data table based on MonitoringID
  data[
    unique(Mon_Summ[, .(MonitoringID, SufficientData)]),
    on = .(MonitoringID),
    SufficientData := i.SufficientData
  ]
  
  # Creates Use_In_Analysis column for data that is determined if the row has
  # Include value of TRUE and SufficientData value of TRUE
  data[, Use_In_Analysis := Include & SufficientData]
  # Get list of and number of MonitoringID that are to be used in analysis
  Mon_IDs <- sort(unique(data$MonitoringID[data$Use_In_Analysis==TRUE]))
  n <- length(Mon_IDs)
  # Save Mon_IDs file for each parameter
  saveRDS(Mon_IDs, file = paste0(out_dir_tables,"/WC_Continuous_", param_abrev, "_Mon_IDs.rds"))
  
  ###################
  ### Coordinates ###
  ###################
  setDT(data)
  coordinates <- data[, .(n_data = .N,
                          year_min = min(Year),
                          year_max = max(Year),
                          years_of_data = max(Year) - min(Year),
                          lat = mean(OriginalLatitude),
                          lon = mean(OriginalLongitude)),
                      by = .(AreaID, ManagedAreaName, ParameterName, ProgramID, ProgramName, ProgramLocationID, Use_In_Analysis)]
  
  # Drop rows with NA values
  coordinates <- coordinates[complete.cases(coordinates)]
  coordinates$AreaID <- as.character(coordinates$AreaID)
  
  saveRDS(coordinates, file = paste0(out_dir_tables,"/WC_Continuous_", param_abrev, "_Station_Coordinates.rds"))
  coordinates_df <- bind_rows(coordinates_df, coordinates)
  
  ############################
  #### MANAGED AREA STATS ####
  ############################
  # Create summary statistics for each monitoring location based on Year and Month
  # intervals.
  Mon_YM_Stats <- data[Use_In_Analysis == TRUE, .(RelativeDepth = unique(RelativeDepth),
                                                  EarliestSampleDate = min(SampleDate),
                                                  LastSampleDate = max(SampleDate),
                                                  N_Data = .N,
                                                  Min = min(ResultValue),
                                                  Max = max(ResultValue),
                                                  Median = median(ResultValue),
                                                  Mean = mean(ResultValue),
                                                  StandardDeviation = sd(ResultValue)),
                       by = .(MonitoringID, AreaID, ManagedAreaName, ParameterName, 
                              ProgramID, ProgramName, ProgramLocationID, Year, Month)]
  # Puts the data in order based on ManagedAreaName, ProgramID, ProgramName,
  # ProgramLocationID, Year, then Month
  setorder(Mon_YM_Stats, ManagedAreaName, ProgramID, ProgramName, ProgramLocationID, Year, Month)
  # Get year from start for each monitoring location
  Mon_YM_Stats[, YearFromStart := Year - min(Year), by = .(MonitoringID)]
  # Create decimal value of year and month values
  Mon_YM_Stats[, YearMonthDec := Year + ((Month - 0.5) / 12)]
  
  # Saving RDS object to file
  cat("Saving Mon_YM_Stats", "\n")
  saveRDS(Mon_YM_Stats, file = paste0(out_dir_tables,"/WC_Continuous_", param_abrev, "_Mon_YM_Stats.rds"))
  
  # Create summary statistics for each monitoring location based on Year
  # intervals.
  Mon_Y_Stats <- data[Use_In_Analysis == TRUE, .(RelativeDepth = unique(RelativeDepth),
                                                 EarliestSampleDate = min(SampleDate),
                                                 LastSampleDate = max(SampleDate),
                                                 N_Data = .N,
                                                 Min = min(ResultValue),
                                                 Max = max(ResultValue),
                                                 Median = median(ResultValue),
                                                 Mean = mean(ResultValue),
                                                 StandardDeviation = sd(ResultValue)),
                      by = .(AreaID, ManagedAreaName, ParameterName,
                             ProgramID, ProgramName, ProgramLocationID, Year)]
  # Puts the data in order based on ManagedAreaName, ProgramID, ProgramName,
  # ProgramLocationID, then Year
  setorder(Mon_Y_Stats, ManagedAreaName, ProgramID, ProgramName, ProgramLocationID, Year)
  
  # Saving RDS object
  cat("Saving Mon_Y_Stats", "\n")
  saveRDS(Mon_Y_Stats, file = paste0(out_dir_tables,"/WC_Continuous_", param_abrev, "_Mon_Y_Stats.rds"))
  rm(Mon_Y_Stats)
  
  # Create summary statistics for each monitoring location based on Month
  # intervals.
  Mon_M_Stats <- data[Use_In_Analysis == TRUE, .(RelativeDepth = unique(RelativeDepth),
                                                 EarliestSampleDate = min(SampleDate),
                                                 LastSampleDate = max(SampleDate),
                                                 N_Data = .N,
                                                 Min = min(ResultValue),
                                                 Max = max(ResultValue),
                                                 Median = median(ResultValue),
                                                 Mean = mean(ResultValue),
                                                 StandardDeviation = sd(ResultValue)),
                      by = .(AreaID, ManagedAreaName, ParameterName, 
                             ProgramID, ProgramName, ProgramLocationID, Month)]
  # Puts the data in order based on ManagedAreaName, ProgramID, ProgramName,
  # ProgramLocationID, then Month
  setorder(Mon_M_Stats, ManagedAreaName, ProgramID, ProgramName, ProgramLocationID, Month)
  
  # Saving RDS object
  cat("Saving Mon_M_Stats", "\n")
  saveRDS(Mon_M_Stats, file = paste0(out_dir_tables,"/WC_Continuous_", param_abrev, "_Mon_M_Stats.rds"))
  rm(Mon_M_Stats)
  
  # Write full dataset to temporary file to help with memory overload in data aggregation step below
  temp_file <- tempfile(fileext = ".parquet")
  arrow::write_parquet(data, sink = temp_file)
  
  # Force clear memory before execution
  rm(data)
  gc()
  
  # Reduces size of data by getting a monthly average (this is the efficiency bottleneck)
  # New method uses polars package to more efficiently group & summarise
  tic()
  data <- pl$scan_parquet(temp_file)$
    group_by(
      "MonitoringID",
      "AreaID",
      "ManagedAreaName",
      "ProgramID",
      "ProgramName",
      "ProgramLocationID",
      "SampleDate"
    )$
    agg(
      pl$col("Year")$first()$alias("Year"),
      pl$col("Month")$first()$alias("Month"),
      pl$col("RelativeDepth")$first()$alias("RelativeDepth"),
      pl$col("ResultValue")$mean()$alias("ResultValue"),
      pl$col("Include")$first()$alias("Include"),
      pl$col("Use_In_Analysis")$first()$alias("Use_In_Analysis")
    )$
    collect(engine = "streaming") |>
    as.data.table()
  toc()
  
  # Remove temporary file
  unlink(temp_file)
  
  # Sets column formats to appropriate types
  data[, `:=` (
    SampleDate = as.Date(SampleDate),
    YearMonth = paste0(sprintf("%02d", Month), "-", Year),
    YearMonhtDec = Year + ((Month - 0.5) / 12),
    DecDate = decimal_date(as.Date(SampleDate))
  )]
  #######################################
  #### SEASONAL KENDALL TAU ANALYSIS ####
  #######################################
  
  # Dataframe for SKT results
  skt_stats_df <- data.table()
  # Determines if there are any monitoring locations to analyze
  if(n==0){
    print("There are no monitoring locations that qualify.")
    # # Create empty data table 
    # col_names <- c("MonitoringID","Independent","tau","p","SenSlope","SenIntercept","ChiSquared","pChiSquared","ub","lb")
    # skt_stats_df <- data.table(matrix(NA_real_, nrow = nrow(Mon_Summ), ncol = length(col_names)))
    # setnames(skt_stats_df, col_names)
  } else {
    # Starts cycling through Monitoring locations to determine Seasonal Kendall Tau
    for(i in 1:n){
      mon_id <- Mon_IDs[i]
      # Gets the number of rows of data for the monitoring location
      data_SKT <- Mon_YM_Stats[MonitoringID==mon_id, ]
      # Gets station name
      station_name <- Mon_YM_Stats[MonitoringID==mon_id, unique(ProgramLocationID)]
      # Perform analysis if there are more than 1 row
      if(nrow(data_SKT)>0){
        # Store the monitoring location summary statistics to be used in trend analysis
        SKT.med <- Mon_Summ[MonitoringID==mon_id, Median]
        SKT.minYr <- Mon_Summ[MonitoringID==mon_id, EarliestYear]
        SKT.maxYr <- Mon_Summ[MonitoringID==mon_id, LatestYear]
        SKT.ind <- TRUE
        SKT <- kendallSeasonalTrendTest(y=data_SKT$Mean,
                                        season=data_SKT$Month,
                                        year=data_SKT$YearFromStart,
                                        independent.obs=SKT.ind)
        
        
        if(is.na(SKT$estimate[1])){
          SKT.ind <- FALSE
          SKT <- kendallSeasonalTrendTest(y=data_SKT$Mean,
                                          season=data_SKT$Month,
                                          year=data_SKT$YearFromStart,
                                          independent.obs=SKT.ind)
        }
        # Save SKT output
        raw_cont_models[[param_abrev]][[station_name]] <- SKT
        
        # Create dataframe of results
        skt_stats <- data.table(
          "MonitoringID" = mon_id, 
          "Independent" = SKT.ind, 
          "tau" = SKT$estimate[1], 
          "p" = format(as.numeric(SKT$p.value[2]), scientific = FALSE),
          "SenSlope" = as.numeric(SKT$estimate[2]), 
          "SenIntercept" = as.numeric(SKT$estimate[3]), 
          "ChiSquared" = round(as.numeric(SKT$statistic[1]), digits = 4), 
          "pChiSquared" = round(as.numeric(SKT$p.value[1]), digits = 4), 
          "ub" = SKT$interval$limits["UCL"], 
          "lb" = SKT$interval$limits["LCL"]
        )
        
        ## Logic to determine trend value (0, 1, 2, -1, -2)
        # If the p value is significant, there is a trend
        significant <- skt_stats$p < 0.05
        # If the p value is significant and the slope is greater than 10% of the
        # median value, the trend is large (2).
        large_trend <- abs(skt_stats$SenSlope) > (abs(SKT.med)*0.1)
        # Large trend = 2, regular trend = 1, or no trend = 0
        trend_val <- ifelse(significant & large_trend, 2, ifelse(significant & !large_trend, 1, 0))
        # Sets the sign of the trend based on Sen Slope direction
        if(skt_stats$SenSlope <= 0){
          trend_val <- -trend_val
        }
        # Place trend value into skt_stats table
        skt_stats$Trend <- as.integer(trend_val)
        # Bind results for each monitoringID for a given datafile
        skt_stats_df <- bind_rows(skt_stats_df, skt_stats)
      }
    }
  }
  
  # Clears unused variables
  rm(SKT, data_SKT, SKT.med, SKT.minYr, SKT.maxYr, SKT.ind, skt_stats)
  
  # Combines the skt_stats_df with Mon_Summ
  if(n==0){
    skt_stats <- Mon_Summ %>% 
      mutate(Independent = NA, tau = NA, p = NA,
             SenSlope = NA, SenIntercept = NA,
             ChiSquared = NA, pChiSquared = NA, ub = NA, lb = NA, Trend = NA)
  } else {
    skt_stats <- merge.data.frame(Mon_Summ, skt_stats_df, by=c("MonitoringID"), all=TRUE)
  }
  
  skt_stats <- as.data.table(skt_stats[order(skt_stats$MonitoringID), ])
  
  cat("Saving SKT_stats", "\n")
  saveRDS(skt_stats, file = paste0(out_dir_tables,"/WC_Continuous_", param_abrev, "_skt_stats.rds"))
  saveRDS(select(skt_stats, -c(EarliestSampleDate)), file = paste0(out_dir_tables,"/WC_Continuous_", param_abrev, "_KendallTau_Stats.rds"))
  
  # Removes data rows with no ResultValue (created by merging with MA_All)
  data <- data[!is.na(ResultValue)]
  
  # Saving overall data object
  saveRDS(data, file = paste0(out_dir_tables,"/WC_Continuous_", param_abrev, "_data.rds"))
  
  # Grab list of unique stations by ManagedAreaName
  stations <- data %>% group_by(ManagedAreaName) %>% reframe(Stations = unique(ProgramLocationID), ParameterName = p)
  # append stations to list
  cont_stations <- bind_rows(cont_stations, stations)
}

# Save Coordinates data frame
saveRDS(coordinates_df, file = paste0(out_dir_tables,"/WC_Continuous_coordinates.rds"))

# write file_lists to file
fwrite(cont_files_short, "output/tables/cont/cont_file_list.txt", sep='|')

# all stations write to file
fwrite(cont_stations, "output/tables/cont/cont_station_list.txt", sep='|')

# Save model results
saveRDS(raw_cont_models, "output/models/All_Continuous_Models.rds")

toc()
End_time <- Sys.time()

print(paste0("Start time: ", Start_time))
print(paste0("End time: ", End_time))
