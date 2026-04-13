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
for(path in folder_paths) {if(!dir.exists(path)){dir.create(path)}}

cont_files_short <- data.frame()
cont_stations <- data.frame()
coordinates_df <- data.frame()
raw_cont_models <- list() # store raw model outputs for model aggregation

# Use the below line for most recent exports
file_list <- list.files(seacar_data_location, full.names = T)
cont_file_list <- str_subset(file_list, "_cont_")

# Creates function to check monitoring location for at least 2 years of
# continuous consecutive data
ContinuousConsecutiveCheck <- function(con_data){
  # Remove consecutive variable if it exists (start fresh)
  if(exists("consecutive")) rm(consecutive)
  # Gets MonitoringIDs
  IDs <- unique(con_data$MonitoringID[con_data$Include==TRUE &
                                        !is.na(con_data$Include)])
  # Loops through each MonitoringID
  for(i in 1:length(IDs)) {
    # Gets list of Years for MonitoringID
    Years <- unique(con_data$Year[con_data$MonitoringID==IDs[i] &
                                    con_data$Include==TRUE &
                                    !is.na(con_data$Include)])
    # Puts Years in order
    Years <- Years[order(Years)]
    # If there are fewer than 2 years, skip to next MonitoringID
    if(length(Years)<2) {
      next
    }
    # Starts loop to make sure there are at least 2 consecutive years with
    # consecutive months of data
    for(j in 2:length(Years)) {
      # If adjacent year entries are not 1 year apart, skip to the next set
      # of year entries
      if(Years[j]-Years[j-1]!=1) {
        next
      }
      # Gets the list of months from the first year
      Months1 <- unique(con_data$Month[con_data$MonitoringID==IDs[i] &
                                         con_data$Year==Years[j-1] &
                                         con_data$Include==TRUE &
                                         !is.na(con_data$Include)])
      # Gets list of months for the second year
      Months2 <- unique(con_data$Month[con_data$MonitoringID==IDs[i] &
                                         con_data$Year==Years[j] &
                                         con_data$Include==TRUE &
                                         !is.na(con_data$Include)])
      # If there are more than 2 months shared between the two years, the
      # MonitoringID passes the check and is stored
      if(length(intersect(Months1, Months2))>=2) {
        # Creates variable for stored MonitoringID if it doesn't exist
        if(exists("consecutive")==FALSE){
          consecutive <- IDs[i]
          break
        } else{
          # Adds to variable for storing MonitoringID if does exist
          consecutive <- append(consecutive, IDs[i])
          break
        }
      }
    }
  }
  # After going through all MonitoringID, return variable with list of all
  # that pass
  return(consecutive)
}

# Loop through all available continuous files
for(file in cont_file_list){
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
  
  
  
  ##### TEMPORARY FIX
  # Awaiting implementation of SpCond thresholds for Continunous
  if(p=="Specific Conductivity"){
    data <- data[ResultValue < 200 & ResultValue > 0, ]
  }
  ##############
  
  
  # Separate reference file to prevent having to de-concatenate ManagedAreaName on entire continuous dataset
  # This is a crosswalk of individual MonitoringID and ProgramLocationID associations by ManagedAreaName
  ma_ref <- data %>% group_by(AreaID, ManagedAreaName, ProgramID, ProgramName, ProgramLocationID) %>%
    reframe(MonitoringID = cur_group_id())
  # De-concatenate to make ManagedAreaName associations
  ma_ref <- SEACAR::clean_managed_areas(ma_ref, "ma")
  # Merge MonitoringID values back into dataset
  data <- data %>% merge(ma_ref %>% group_by(ProgramLocationID, MonitoringID) %>% reframe())
  
  # Remove all non-MA data, analyze only MA values
  data <- data[!is.na(AreaID)]
  # Removes any data rows that do not have Include==1
  data <- data[data$Include==1,]
  # Removes rows that have missing ResultValues
  data <- data[!is.na(data$ResultValue),]
  # Removes rows that have missing RelativeDepth
  data <- data[!is.na(data$RelativeDepth),]
  # Rremoves rows that have an ActivityType with Blank
  data <- data[!grep("Blank", data$ActivityType),]
  
  # Stores the MonitoringID that pass the consecutive year check
  consMonthIDs <- ContinuousConsecutiveCheck(data)
  # Creates data frame with summary for each monitoring location.
  median_na_rm <- function(x) ifelse(length(x) > 0, median(x, na.rm = TRUE), NA_real_)
  Mon_Summ <- data[, .(RelativeDepth = unique(RelativeDepth),
                       N_Data = sum(Include == TRUE & !is.na(ResultValue)),
                       N_Years = uniqueN(Year[Include == TRUE & !is.na(Year)]),
                       EarliestYear = min(Year[Include == TRUE]),
                       LatestYear = max(Year[Include == TRUE]),
                       EarliestSampleDate = min(SampleDate[Include == TRUE]),
                       LastSampleDate = max(SampleDate[Include == TRUE]),
                       Median = median_na_rm(ResultValue)), 
                   by = .(MonitoringID, AreaID, ManagedAreaName, ProgramID, ProgramName, ProgramLocationID, ParameterName)]
  
  Mon_Summ[, ConsecutiveMonths := ifelse(MonitoringID %in% consMonthIDs==TRUE, TRUE, FALSE)]
  Mon_Summ[, SufficientData := ifelse(N_Data>0 & N_Years>=suff_years & ConsecutiveMonths==TRUE, TRUE, FALSE)]
  Mon_Summ$ConsecutiveMonths <- NULL
  
  # Puts summary data in order based on MonitoringID
  Mon_Summ <- as.data.table(Mon_Summ[order(Mon_Summ$MonitoringID), ])
  
  # Creates column in data that determines how many years from the start for each
  # Monitoring location
  data[, YearFromStart := Year - min(Year), by = MonitoringID]
  # Adds SufficientData column to data table based on MonitoringID
  data <- merge.data.frame(data, Mon_Summ[,c("MonitoringID", "SufficientData")],
                           by="MonitoringID")
  # Creates Use_In_Analysis column for data that is determined if the row has
  # Include value of TRUE and SufficientData value of TRUE
  data$Use_In_Analysis <- ifelse(data$Include==TRUE & data$SufficientData==TRUE,
                                 TRUE, FALSE)
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
  Mon_YM_Stats <- as.data.table(Mon_YM_Stats[order(Mon_YM_Stats$ManagedAreaName,
                                                   Mon_YM_Stats$ProgramID,
                                                   Mon_YM_Stats$ProgramName,
                                                   Mon_YM_Stats$ProgramLocationID,
                                                   Mon_YM_Stats$Year,
                                                   Mon_YM_Stats$Month), ])
  
  # Get year from start for each monitoring location
  Mon_YM_Stats[, YearFromStart := Year - min(Year), by = MonitoringID]
  # Create decimal value of year and month values
  Mon_YM_Stats$YearMonthDec <- Mon_YM_Stats$Year + ((Mon_YM_Stats$Month-0.5) / 12)
  
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
  Mon_Y_Stats <- as.data.table(Mon_Y_Stats[order(Mon_Y_Stats$ManagedAreaName,
                                                 Mon_Y_Stats$ProgramID,
                                                 Mon_Y_Stats$ProgramName,
                                                 Mon_Y_Stats$ProgramLocationID,
                                                 Mon_Y_Stats$Year), ])
  
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
  Mon_M_Stats <- as.data.table(Mon_M_Stats[order(Mon_M_Stats$ManagedAreaName,
                                                 Mon_M_Stats$ProgramID,
                                                 Mon_M_Stats$ProgramName,
                                                 Mon_M_Stats$ProgramLocationID,
                                                 Mon_M_Stats$Month), ])
  
  # Saving RDS object
  cat("Saving Mon_M_Stats", "\n")
  saveRDS(Mon_M_Stats, file = paste0(out_dir_tables,"/WC_Continuous_", param_abrev, "_Mon_M_Stats.rds"))
  rm(Mon_M_Stats)
  
  # Reduces size of data by getting a monthly average (this is the efficiency bottleneck)
  # New method uses polars package to more efficiently group & summarise
  tic()
  data <- as_polars_df(data)$
    lazy()$
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
    collect() |>
    as.data.frame()
  toc()
  
  # Sets column formats to appropriate types
  data$SampleDate <- as.Date(data$SampleDate)
  data$YearMonth <- format(data$SampleDate, format = "%m-%Y")
  data$YearMonthDec <- data$Year + ((data$Month-0.5) / 12)
  data$DecDate <- decimal_date(data$SampleDate)
  
  #######################################
  #### SEASONAL KENDALL TAU ANALYSIS ####
  #######################################
  
  # Dataframe for SKT results
  skt_stats_df <- data.table()
  # Determines if there are any monitoring locations to analyze
  if(n==0){
    print("There are no monitoring locations that qualify.")
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
  skt_stats <- merge.data.frame(Mon_Summ, skt_stats_df, by=c("MonitoringID"), all=TRUE)
  skt_stats <- as.data.table(skt_stats[order(skt_stats$MonitoringID), ])
  
  cat("Saving SKT_stats", "\n")
  saveRDS(skt_stats, file = paste0(out_dir_tables,"/WC_Continuous_", param_abrev, "_skt_stats.rds"))
  saveRDS(select(skt_stats, -c(EarliestSampleDate)), file = paste0(out_dir_tables,"/WC_Continuous_", param_abrev, "_KendallTau_Stats.rds"))
  
  # Removes data rows with no ResultValue (created by merging with MA_All)
  data <- data[!is.na(data$ResultValue),]
  
  # Saving overall data object
  saveRDS(data, file = paste0(out_dir_tables,"/WC_Continuous_", param_abrev, "_data.rds"))
  
  # Grab list of unique stations by ManagedAreaName
  stations <- data %>% group_by(ManagedAreaName) %>% reframe(Stations = unique(ProgramLocationID), ParameterName = p)
  # append stations to list
  cont_stations <- bind_rows(cont_stations, stations)
  # Remove existing data objects
  rm(coordinates, n, Mon_IDs, stations, data)
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
