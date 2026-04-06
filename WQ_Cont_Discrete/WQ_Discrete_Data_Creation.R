# The purpose of this script is to generate modular .Rds files for each relevant combination of
# parameter, relative depth, and activity type for discrete WC data.
# Originally created by J.E. Panzik (jepanzik@usf.edu) for SEACAR
# Modified by T.G. Hill (Tyler.Hill@FloridaDEP.gov) in August, 2023

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
library(stringr)

source("../SEACAR_data_location.R")

tic()

#Set output directory
out_dir <- "output"

# output directory (discrete)
out_dir_tables <- paste0(out_dir, "/tables/disc")

#Set number of unique years a location must have to be considered for analysis
suff_years <- 10

# Import website parameters to determine which analyses to run
websiteParams <- SEACAR::WebsiteParameters[Website==1 & SamplingFrequency=="Discrete", ]

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- SEACAR::ManagedAreas

# Creates folders for outputs
folder_paths <- c("output/tables/", "output/tables/disc")
for(path in folder_paths){if(!dir.exists(path)){dir.create(path)}}

# Use the below line for most recent exports
file_list <- list.files(seacar_data_location, full.names = T)
# Use the below lines to run data objects for a previous export
# Export date to use (as string, matching folder within /archive/)

#####
# exportDate <- "2024-Mar-27"
# file_list <- list.files(paste0(seacar_data_location, "/archive/",exportDate), full.names = T)
#####

# Clean managed areas function (polars - faster)
library(polars)
clean_managed_areas_polars <- function(df, type) {
  if (type %in% c("ma", "buff")) {
    ma_col   <- if (type == "ma") "ManagedAreaName" else "ManagedAreaName_Buff"
    a_id_col <- if (type == "ma") "AreaID" else "AreaID_Buff"
  } else {
    stop("Input `type` must be either 'ma' or 'buff'.")
  }
  
  ldf <- as_polars_lf(df)$with_row_index(".row_id")
  
  area_long <- ldf$
    select(
      ".row_id",
      pl$col(a_id_col)$alias("a_id")
    )$
    with_columns(
      pl$col("a_id")$str$split("/")$alias("a_id")
    )$
    explode("a_id")$
    with_columns(
      pl$col("a_id")$str$strip_chars()$alias("a_id")
    )
  
  name_long <- ldf$
    select(
      ".row_id",
      pl$col(ma_col)$alias("ma")
    )$
    with_columns(
      pl$col("ma")$str$split("/")$alias("ma")
    )$
    explode("ma")$
    with_columns(
      pl$col("ma")$str$strip_chars()$alias("ma"),
      pl$col("ma")$str$extract("^(\\d+)", 1)$alias("ID_extracted"),
      pl$col("ma")$str$replace("^\\d+\\s*-\\s*", "")$alias("Name_clean")
    )
  
  df_other <- ldf$drop(c(a_id_col, ma_col))
  
  result <- area_long$
    join(
      name_long,
      left_on = c(".row_id", "a_id"),
      right_on = c(".row_id", "ID_extracted"),
      how = "inner"
    )$
    with_columns(
      pl$col("Name_clean")$alias(ma_col),
      pl$col("a_id")$alias(a_id_col)
    )$
    drop(c("ma", "Name_clean", "a_id"))$
    join(
      df_other,
      on = ".row_id",
      how = "left"
    )$
    drop(".row_id")$
    with_columns(
      pl$col(a_id_col)$cast(pl$Float64)
    )$
    collect() |>
    as.data.table()
  
  result
}

# Creates function to checks managed area for at least 2 years of
# continuous consecutive data
DiscreteConsecutiveCheck <- function(con_data){
  # Remove consecutive variable if it exists (start fresh)
  if(exists("consecutive")) rm(consecutive)
  # Gets AreaIDs
  IDs <- unique(con_data$AreaID[con_data$Include==TRUE &
                                  !is.na(con_data$Include)])
  # Loops through each AreaID
  for(i in 1:length(IDs)) {
    # Gets list of Years for AreaID
    Years <- unique(con_data$Year[con_data$AreaID==IDs[i] &
                                    con_data$Include==TRUE &
                                    !is.na(con_data$Include)])
    # Puts Years in order
    Years <- Years[order(Years)]
    # If there are fewer than 2 years, skip to next AreaID
    if(length(Years)<2) {
      next
    }
    # Starts loop to make sure there are at least 2 consecutive years
    # with consecutive months of data
    for(j in 2:length(Years)) {
      # If adjacent year entries are not 1 year apart, skip to the
      # next set of year entries
      if(Years[j]-Years[j-1]!=1) {
        next
      }
      # Gets the list of months from the first year
      Months1 <- unique(con_data$Month[
        con_data$AreaID==IDs[i] &
          con_data$Year==Years[j-1] &
          con_data$Include==TRUE &
          !is.na(con_data$Include)])
      # Gets list of months for the second year
      Months2 <- unique(con_data$Month[
        con_data$AreaID==IDs[i] &
          con_data$Year==Years[j] &
          con_data$Include==TRUE &
          !is.na(con_data$Include)])
      # If there are more than 2 months shared between the two
      # years, the AreaID passes the check and is stored
      if(length(intersect(Months1, Months2))>=2) {
        # Creates variable for stored AreaID if it
        # doesn't exist
        if(exists("consecutive")==FALSE){
          consecutive <- IDs[i]
          break
          # Adds to variable for storing AreaID if does exist
        } else{
          consecutive <- append(consecutive, IDs[i])
          break
        }
      }
    }
  }
  # After going through all AreaID, return variable with list of all
  # that pass
  return(consecutive)
}

# Store the filenames used in the analysis
disc_files_short <- data.frame()
# Store all relevant data for later use
data_directory <- list()

#Starts for loop that cycles through each parameter
for(p in unique(websiteParams$ParameterName)){
  cat(paste("Starting parameter:", p, "\n"))
  param_abrev <- websiteParams[ParameterName==p, ParameterShort]
  units <- websiteParams[ParameterName==p, ParameterUnits]
  #Gets the file with the filename containing the desired parameter
  disc_param_pattern <- paste0("NUT_", gsub(",", "", gsub(" ", "_", p)), "-")
  # Discrepancy in capitalization of parameters
  disc_param_pattern <- gsub("NUT_Chlorophyll_a_Corrected_for_Pheophytin-", "Chlorophyll_a_corrected_for_pheophytin-", disc_param_pattern)
  disc_param_pattern <- gsub("NUT_Chlorophyll_a_Uncorrected_for_Pheophytin-", "Chlorophyll_a_uncorrected_for_pheophytin-", disc_param_pattern)
  disc_param_pattern <- gsub("NUT_Total_Suspended_Solids-", "Total_Suspended_Solids_TSS-", disc_param_pattern)
  disc_param_pattern <- gsub("NUT_Colored_Dissolved_Organic_Matter-", "Colored_dissolved_organic_matter_CDOM-", disc_param_pattern)
  
  file_in <- str_subset(file_list, disc_param_pattern)
  
  # shortened filenames for display in report
  file_short <- tail(str_split(file_in, "/")[[1]],1)
  data_directory[[p]][["file_short"]] <- file_short
  # append filenames to disc_file_list
  disc_files_short <- bind_rows(disc_files_short, data.frame("ParameterName" = p,
                                                             "file_short" = file_short))
  
  # Set relevant depth and activity type for each parameter
  depth <- websiteParams[ParameterName==p, RelativeDepth]
  activity <-websiteParams[ParameterName==p, ActivityType]
  
  ###################
  ### FILE IMPORT ###
  ###################
  
  cat(paste0("The data file(s) used: \n", file_short, "\n"))
  data <- fread(file_in, sep = "|", na.strings = "NULL")
  # De-concatenate MA names
  data <- clean_managed_areas_polars(data, type = "ma")
  # Remove all non-MA data, analyze only MA values
  data <- data[!is.na(AreaID)]
  
  #################
  ### FILTERING ###
  #################
  
  # Removes data rows with missing ResultValue (not likely)
  data <- data[!is.na(ResultValue), ]
  # Invert values for Secchi Depth ## SEACAR Team meeting - 02/18/2025
  if(p=="Secchi Depth"){
    data$ResultValue <- -data$ResultValue
  }
  # Changes "Sample" to "Lab" for ActivityType
  data$ActivityType <- gsub("Sample", "Lab", data$ActivityType)
  
  # Gets data for the specific activity type if it is not All
  if(activity!="All"){
    data <- data[grep(activity, data$ActivityType),]
  }
  
  # Changes RelativeDepth to Bottom for the QAQC flag 12Q that indicates
  # measurements are both surface and bottom if the relative depth is bottom
  if(depth=="Bottom"){
    data$RelativeDepth[grep("12Q", data$SEACAR_QAQCFlagCode[
      data$RelativeDepth=="Surface"])] <- "Bottom"
  }
  # Removes missing RelativeDepth data and data for RelativeDepth not of interest
  # from all parameters except Secchi_Depth
  if(p!="Secchi Depth" & depth!="All"){
    data <- data[!is.na(data$RelativeDepth),]
    data <- data[data$RelativeDepth==depth,]
  }
  # Removes data rows that have "Blank" as an ActivityType
  if(length(grep("Blank", data$ActivityType))>0){
    data <- data[-grep("Blank", data$ActivityType),]
  }
  # Changes Include to be either TRUE or FALSE
  data$Include <- as.logical(data$Include)
  # Changes Include to be TRUE for ProgramID 476 if it had the H value qualifier
  data$Include[grep("H", data$ValueQualifier[data$ProgramID==476])] <- TRUE
  # Change Include to be FALSE for Secchi_Depth with U value qualifier
  if(p=="Secchi Depth"){
    data$Include[grep("U", data$ValueQualifier)] <- FALSE
  }
  # Stores the AreaID that pass the consecutive year check
  consMonthIDs <- DiscreteConsecutiveCheck(data)
  
  # Creates data frame with summary for each managed area
  MA_Summ <- data %>%
    group_by(AreaID, ManagedAreaName, ParameterName) %>%
    summarize(RelativeDepth=depth,
              ActivityType=activity,
              N_Data=length(ResultValue[Include==TRUE & !is.na(ResultValue)]),
              N_Years=length(unique(Year[Include==TRUE & !is.na(Year)])),
              EarliestYear=min(Year[Include==TRUE & N_Data!=0]),
              LatestYear=max(Year[Include==TRUE & N_Data!=0]),
              EarliestSampleDate=min(SampleDate[Include==TRUE]),
              LastSampleDate=max(SampleDate[Include==TRUE]),
              ConsecutiveMonths=ifelse(unique(AreaID) %in%
                                         consMonthIDs==TRUE, TRUE, FALSE),
              # Determines if monitoring location is sufficient for analysis
              # based on having more than 0 data entries, more than the
              # sufficient number of year, and the consecutive month criteria
              SufficientData=ifelse(N_Data>0 & N_Years>=suff_years &
                                      ConsecutiveMonths==TRUE, TRUE, FALSE),
              Median=median(ResultValue[Include==TRUE & N_Data!=0], na.rm=TRUE),
              .groups = "keep")
  MA_Summ$ConsecutiveMonths <- NULL
  # Creates column in data that determines how many years from the start for each managed area
  data <- data %>%
    group_by(AreaID, ManagedAreaName) %>%
    mutate(YearFromStart=Year-min(Year))
  # Adds SufficientData column to data table based on managed area
  data <- merge.data.frame(data, MA_Summ[,c("ManagedAreaName", "SufficientData")], by="ManagedAreaName")
  # Creates Use_In_Analysis column for data that is determined if the row has
  # Include value of TRUE and SufficientData value of TRUE
  data$Use_In_Analysis <- ifelse(data$Include==TRUE & data$SufficientData==TRUE,
                                 TRUE, FALSE)
  # Rearranges the summary data frame columns to be AreaID, ManagedAreaName,
  # ParameterName, RelativeDepth, ActivityType, SufficientData, everything else
  MA_Summ <- MA_Summ %>%
    select(AreaID, ManagedAreaName, ParameterName, RelativeDepth, ActivityType,
           SufficientData, everything()) %>% arrange(ManagedAreaName) %>% as.data.table()
  # Put SampleDate as date object
  data$SampleDate <- as.Date(data$SampleDate)
  # Creates character object for Month and Year
  data$YearMonth <- paste0(data$Month, "-", data$Year)
  # Creates variable that puts year and month into a decimal year format
  data$YearMonthDec <- data$Year + ((data$Month-0.5) / 12)
  # Converts ampleDate to a decimal date
  data$DecDate <- decimal_date(data$SampleDate)
  
  # Get list of and number of managed areas that are to be used in analysis
  MA_Include <- MA_Summ$ManagedAreaName[MA_Summ$SufficientData==TRUE]
  
  data_directory[[p]][["MA_Include"]] <- MA_Include
  saveRDS(MA_Include, file = paste0(out_dir_tables,"/WC_Discrete_", param_abrev, "_", activity, "_", depth, "_MA_Include.rds"))
  
  n <- length(MA_Include)
  # Get list of and number of managed areas that are excluded from analysis
  MA_Exclude <- MA_Summ[MA_Summ$N_Years<10 & MA_Summ$N_Years>0,]
  MA_Exclude <- MA_Exclude[,c("ManagedAreaName", "N_Years")]
  z <- nrow(MA_Exclude)
  
  ################################
  ### DETERMING ValueQualifers ###
  ################################
  
  # Find out how much total data exists and how much passed the initial filters
  total <- length(data$Include)
  pass_filter <- length(data$Include[data$Include==TRUE])
  # Get the number and percentage of data entries impacted by value qualifier H
  count_H <- length(grep("H", data$ValueQualifier[data$ProgramID==476]))
  perc_H <- 100*count_H/length(data$ValueQualifier)
  # Get the number and percentage of data entries impacted by value qualifier I
  count_I <- length(grep("I", data$ValueQualifier))
  perc_I <- 100*count_I/length(data$ValueQualifier)
  # Get the number and percentage of data entries impacted by value qualifier Q
  count_Q <- length(grep("Q", data$ValueQualifier))
  perc_Q <- 100*count_Q/length(data$ValueQualifier)
  # Get the number and percentage of data entries impacted by value qualifier S
  count_S <- length(grep("S", data$ValueQualifier))
  perc_S <- 100*count_S/length(data$ValueQualifier)
  # Get the number and percentage of data entries impacted by value qualifier U
  count_U <- length(grep("U", data$ValueQualifier))
  perc_U <- 100*count_U/length(data$ValueQualifier)
  # Copy ValueQualifier to a new VQ_Plot to create codes for plots
  data$VQ_Plot <- data$ValueQualifier
  # Determine if data with value qualifier H should be included for plots based
  # on the parameter being observed
  inc_H <- ifelse(p %in% c("pH", "Dissolved Oxygen", "Dissolved Oxygen Saturation"), TRUE, FALSE)
  # Loops through conditions to determine what indicators to include in plots.
  # If H should be included
  if(inc_H==TRUE){
    # Remove any Value qualifiers that aren't H or U
    data$VQ_Plot <- gsub("[^HU]+", "", data$VQ_Plot)
    # Standardize order of qualifiers. Puts UH as HU
    data$VQ_Plot <- gsub("UH", "HU", data$VQ_Plot)
    # Remove anything from ValueQualifier that isn't U from programs and that
    # aren't ProgramID 476
    data$VQ_Plot[na.omit(data$ProgramID!=476)] <-
      gsub("[^U]+", "", data$VQ_Plot[na.omit(data$ProgramID!=476)])
    # Changes blank character strings to NA
    data$VQ_Plot[data$VQ_Plot==""] <- NA
    # Prints the number and percentage of H, I, Q, U value qualifiers
    cat(paste0("Number of Measurements: ", total,
               ", Number Passed Filter: ", pass_filter, "\n",
               "Program 476 H Codes: ", count_H, " (", round(perc_H, 6), "%)\n",
               "I Codes: ", count_I, " (", round(perc_I, 6), "%)\n",
               "Q Codes: ", count_Q, " (", round(perc_Q, 6), "%)\n",
               "U Codes: ", count_U, " (", round(perc_U, 6), "%)"))
    # If Parameter is Secchi_Depth
  } else if(p=="Secchi Depth"){
    # Count the number of S ValueQualifier
    count_S <- length(grep("S", data$ValueQualifier))
    # Get percentage of S ValueQualifier
    perc_S <- 100*count_S/length(data$ValueQualifier)
    # Remove anything from ValueQualifier that isn't S or U
    data$VQ_Plot <- gsub("[^SU]+", "", data$VQ_Plot)
    # Change all ValueQualifier that are US to be US, standardizes codes
    data$VQ_Plot <- gsub("US", "SU", data$VQ_Plot)
    # Sets any blank character ValueQualifier to be NA
    data$VQ_Plot[data$VQ_Plot==""] <- NA
    # Prints the number and percentage of I, Q, S, U
    cat(paste0("Number of Measurements: ", total,
               ", Number Passed Filter: ", pass_filter, "\n",
               "I Codes: ", count_I, " (", round(perc_I, 6), "%)\n",
               "Q Codes: ", count_Q, " (", round(perc_Q, 6), "%)\n",
               "S Codes: ", count_S, " (", round(perc_S, 6), "%)\n",
               "U Codes: ", count_U, " (", round(perc_U, 6), "%)\n"))
    # For all other scenarios
  } else {
    # Remove all ValueQualifier except U
    data$VQ_Plot <- gsub("[^U]+", "", data$VQ_Plot)
    # Sets any blank character ValueQualifier to be NA
    data$VQ_Plot[data$VQ_Plot==""] <- NA
    # Prints the number and percentage of I, Q, U
    cat(paste0("Number of Measurements: ", total,
               ", Number Passed Filter: ", pass_filter, "\n",
               "I Codes: ", count_I, " (", round(perc_I, 6), "%)\n",
               "Q Codes: ", count_Q, " (", round(perc_Q, 6), "%)\n",
               "U Codes: ", count_U, " (", round(perc_U, 6), "%)\n"))
  }
  
  # Creates a data table that summarizes the number and percentage of
  # ValueQualifier H, I, Q, S, and U for each managed area each year
  data_summ <- data %>%
    group_by(AreaID, ManagedAreaName, ParameterName, Year) %>%
    summarize(RelativeDepth=depth,
              ActivityType=activity,
              N_Total=length(ResultValue),
              N_AnalysisUse=length(ResultValue[Use_In_Analysis==TRUE]),
              N_H=length(grep("H", ValueQualifier[ProgramID==476])),
              perc_H=100*N_H/length(ValueQualifier),
              N_I=length(grep("I", ValueQualifier)),
              perc_I=100*N_I/length(ValueQualifier),
              N_Q=length(grep("Q", ValueQualifier)),
              perc_Q=100*N_Q/length(ValueQualifier),
              N_S=length(grep("S", ValueQualifier)),
              perc_S=100*N_S/length(ValueQualifier),
              N_U=length(grep("U", ValueQualifier)),
              perc_U=100*N_U/length(ValueQualifier), .groups = "keep") %>%
    arrange(ManagedAreaName, Year)
  # Writes the ValueQualifier summary to a RDS file
  cat("Saving data_summ.rds \n")
  data_directory[[p]][["VQSummary"]] <- data_summ
  saveRDS(data_summ, file = paste0(out_dir_tables,"/WC_Discrete_", param_abrev, "_", activity, "_", depth, "_VQSummary.rds"))
  rm(data_summ)
  
  ###############################
  ### MANAGED AREA STATISTICS ###
  ###############################
  
  # Set up a dataframe to loop through statistic aggregation scenarios (Year-Month, Month, Year)
  stats_scenarios <- data.table(
    "scenario" = c("MA_YM_Stats", "MA_Y_Stats", "MA_M_Stats"),
    "columns" = c("ManagedAreaName|Year|Month", "ManagedAreaName|Year", "ManagedAreaName|Month"),
    "filepath" = c("MA_MMYY_Stats", "MA_Yr_Stats", "MA_Mo_Stats")
  )
  
  for(i in 1:nrow(stats_scenarios)){
    scenario <- stats_scenarios[i]$scenario
    cols <- str_split_1(stats_scenarios[i]$columns, "\\|")
    all_cols <- c("AreaID", "ParameterName", cols)
    stats <- data %>% 
      filter(Use_In_Analysis) %>%
      group_by(across(all_of(all_cols))) %>%
      summarize(RelativeDepth=depth,
                ActivityType=activity,
                N_Data=length(ResultValue),
                Min=min(ResultValue),
                Max=max(ResultValue),
                Median=median(ResultValue),
                Mean=mean(ResultValue),
                StandardDeviation=sd(ResultValue),
                Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                               collapse=', '),
                ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                                 collapse=', '),
                .groups = "keep") %>%
      arrange(across(all_of(cols)))
    
    if(scenario=="MA_YM_Stats"){
      stats <- stats %>%
        group_by(AreaID, ManagedAreaName) %>%
        mutate(YearFromStart = Year - min(Year),
               YearMonthDec = Year + ((Month - 0.5) / 12))
      # Create local variable for use later (SKT analysis)
      MA_YM_Stats <- setDT(copy(stats))
    }
    
    # Writes summary statistics to file
    cat(paste0("Saving ", scenario, ".rds \n"))
    data_directory[[p]][[scenario]] <- stats
    saveRDS(stats, file = paste0(out_dir_tables,"/WC_Discrete_", param_abrev, "_", activity, "_", depth, "_", stats_scenarios[i]$filepath, ".rds"))
    rm(stats, i)
  }
  
  # Gets summary statistics for monitoring locations, which are defined as unique
  # combinations of ManagedAreaName, ProgramID, And ProgramLocationID
  Mon_Stats <- data[data$Use_In_Analysis==TRUE, ] %>%
    group_by(AreaID, ManagedAreaName, ParameterName, ProgramID, ProgramName,
             ProgramLocationID) %>%
    summarize(RelativeDepth=depth,
              ActivityType=activity,
              EarliestSampleDate=min(SampleDate),
              LastSampleDate=max(SampleDate),
              N_Data=length(ResultValue),
              Min=min(ResultValue),
              Max=max(ResultValue),
              Median=median(ResultValue),
              Mean=mean(ResultValue),
              StandardDeviation=sd(ResultValue), .groups = "keep") %>%
    arrange(ManagedAreaName, ProgramName, ProgramID, ProgramLocationID)
  # Write summary statistics to file
  cat("Saving Mon_Stats.rds \n")
  saveRDS(Mon_Stats, file = paste0(out_dir_tables,"/WC_Discrete_", param_abrev, "_", activity, "_", depth, "_MonLoc_Stats.rds"))
  rm(Mon_Stats)
  
  #######################################
  #### SEASONAL KENDALL TAU ANALYSIS ####
  #######################################
  
  # Dataframe for SKT results
  skt_stats_df <- data.table()
  # Determines if there are any managed areas to analyze
  if(n==0){
    cat("There are no managed areas that qualify. \n")
  } else {
    # Starts cycling through managed areas to determine seasonal Kendall Tau
    for(i in 1:n){
      ma <- MA_Include[i]
      # Gets the number of rows of data for the managed area
      data_SKT <- MA_YM_Stats[ManagedAreaName==ma, ]
      # Perform analysis if there is more than 1 row of data
      if(nrow(data_SKT)>0){
        # Store the managed area summary statistics to be used in trend analysis
        SKT.med <- MA_Summ[ManagedAreaName==ma, Median]
        SKT.minYr <- MA_Summ[ManagedAreaName==ma, EarliestYear]
        SKT.maxYr <- MA_Summ[ManagedAreaName==ma, LatestYear]
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
        data_directory[[p]][["SKT"]] <- SKT
        
        # Create dataframe of results
        skt_stats <- data.table(
          "AreaID" = MA_Summ[ManagedAreaName==ma, AreaID],
          "ManagedAreaName" = ma,
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
        rm(skt_stats)
      }
    }
  }
  # Clears unused variables
  rm(SKT, data_SKT, SKT.med, SKT.minYr, SKT.maxYr, SKT.ind)
  # Combines the skt_stats with MA_Summ
  skt_stats <-  merge.data.frame(MA_Summ, skt_stats_df,
                                 by=c("AreaID","ManagedAreaName"), all=TRUE)
  
  # Writes combined statistics to file
  cat("Saving SKT_stats.rds \n")
  
  data_directory[[p]][["skt_stats"]] <- skt_stats
  data_directory[[p]][["KendallTau_Stats"]] <- select(skt_stats, -c(EarliestSampleDate))
  
  saveRDS(skt_stats, file = paste0(out_dir_tables,"/WC_Discrete_", param_abrev, "_", activity, "_", depth, "_skt_stats.rds"))
  saveRDS(select(skt_stats, -c(EarliestSampleDate)), file = paste0(out_dir_tables,"/WC_Discrete_", param_abrev, "_", activity, "_", depth, "_KendallTau_Stats.rds"))
  
  # Removes data rows with no ResultValue (created by merging with MA_All)
  data <- data[!is.na(data$ResultValue),]
  
  data_directory[[p]][["data"]] <- data
  saveRDS(data, file = paste0(out_dir_tables,"/WC_Discrete_", param_abrev, "_data.rds"))
}

# write file_lists to file
fwrite(disc_files_short, paste0(out_dir_tables, "/disc_file_list.txt"), sep='|')

# Save entire data_directory for all parameters
saveRDS(data_directory, file = paste0(out_dir_tables, "/data_directory.rds"))

toc()
End_time <- Sys.time()

print(Start_time)
print(End_time)
