# The purpose of this script is to automate the production of Rmd documents for each relevant combination of
# parameter and region for continuous WC data.
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
library(collapse)
library(stringr)

source("../SEACAR_data_location.R")

tic()
#Sets whether to run documents with plots or not (APP_Plots==TRUE to include plots)
APP_Plots <- TRUE

#Set output directory
out_dir <- "output"

#Set number of unique years a location must have to be considered for analysis
suff_years <- 5

#Sets the list of parameter names to cycle through. This can be edited to limit the number of parameters.
#Easiest way to edit is to comment out undesired parameters.
#If only one parameter is desired, comment out all other parameters and delete comma after remaining parameter
all_params <- c(
  "Dissolved_Oxygen",
  "Dissolved_Oxygen_Saturation",
  "pH",
  "Salinity",
  "Turbidity",
  "Water_Temperature"
)

#Sets the list of parameter abbreviation names to cycle through. This can be edited to limit the number of parameters.
#Easiest way to edit is to comment out undesired parameters.
#If only one parameter is desired, comment out all other parameters and delete comma after remaining parameter
all_params_short <- c(
  "DO",
  "DOS",
  "pH",
  "Sal",
  "Turb",
  "TempW"
)

#Sets the list of regions to cycle through. This can be edited to limit the number of regions.
#If only one region is desired, comment out the other regions and delete comma after remaining region
all_regions <- c(
  "NE",
  "NW",
  "SE",
  "SW"
)

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
                na.strings = "")

# Creates folders for outputs
folder_paths <- c("output/tables","output/tables/cont")
for (path in folder_paths) {if(!dir.exists(path)){dir.create(path)}}

# Defines standard plot theme: black and white, no major or minor grid lines,
# Arial font. Title is centered, size 12, and blue (hex coded). Subtitle is
# centered, size 10, and blue (hex coded). Legend title is size 10 and the
# legend is left-justified. X-axis title is size 10 and the margins are padded
# at the top and bottom to give more space for angled axis labels. Y-axis title
# is size 10 and margins are padded on the right side to give more space for
# axis labels. Axis labels are size 10 and the x-axis labels are rotated -45
# degrees with a horizontal justification that aligns them with the tick mark
plot_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(family="Arial"),
        plot.title=element_text(hjust=0.5, size=12, color="#314963"),
        plot.subtitle=element_text(hjust=0.5, size=10, color="#314963"),
        legend.title=element_text(size=10),
        legend.text = element_text(hjust=0),
        axis.title.x = element_text(size=10, margin = margin(t = 5, r = 0,
                                                             b = 10, l = 0)),
        axis.title.y = element_text(size=10, margin = margin(t = 0, r = 10,
                                                             b = 0, l = 0)),
        axis.text=element_text(size=10),
        axis.text.x=element_text(angle = 60, hjust = 1))

cont_file_list <- list()
cont_station_list <- list()
coordinates_df <- data.frame()

# Use the below line for most recent exports
file_list <- list.files(seacar_data_location, full.names = T)
# Use the below lines to run data objects for a previous export
# Export date to use (as string, matching folder within /archive/)

#####
# exportDate <- "2024-Mar-27"
# file_list <- list.files(paste0(seacar_data_location, "/archive/",exportDate), full.names = T)
#####

#Starts for loop that cycles through each parameter
# for (j in 1:length(all_params)){
for (j in 1:length(all_params)){
  param_name <- all_params[j]
  param_abrev <- all_params_short[j]
  print(paste0("Starting parameter: ", param_name))
  
  for (i in 1:length(all_regions)){
    param_file_name <- paste0("cont_",param_name)
    #Gets the files with the file names containing the desired parameter
    file_list_subset <- str_subset(file_list, param_file_name)
    
    #Since Dissolved_Oxygen will return both Dissolved_Oxygen and Dissolved_Oxygen_Saturation,
    #the if statement removes the entries for Dissolved_Oxygen_Saturation when trying to get Dissolved_Oxygen
    if(param_name=="Dissolved_Oxygen" & length(grep("Saturation", file_list_subset))>0){
      file_list_subset <- file_list_subset[-grep("Saturation", file_list_subset)]
    }
    
    # setting output path for cont. files
    out_dir_tables <- paste0(out_dir, "/tables/cont")
    
    #Filters list of file names for the desired region
    region <- all_regions[i]
    # Uses _[Region Abbreviation]- so that is does not return any coincidental combinations
    file_in <- file_list_subset[grep(paste0("_", region, "-"), file_list_subset)]
    
    #create new variable to help store file_names
    par_region <- paste0(param_abrev, "_", region)
    
    # shortened filenames for display in report
    file_short <- sub("data/cont/", "", file_in)
    # append filenames to cont_file_list
    cont_file_list[[par_region]] <- file_short
    
    print(paste0("Starting region: ", region))
    print(file_short)
    
    ###########################
    ### BEGIN DATA CREATION ###
    ###########################
    
    ###################
    ### FILE IMPORT ###
    ###################
    
    data <- fread(file_in, sep="|", header=TRUE, stringsAsFactors=FALSE,
                  select=c("ManagedAreaName", "ProgramID", "ProgramName",
                           "ProgramLocationID", "SampleDate", "Year", "Month",
                           "RelativeDepth", "ActivityType", "ParameterName",
                           "ResultValue", "ParameterUnits", "ValueQualifier",
                           "SEACAR_QAQCFlagCode", "Include", "OriginalLatitude", "OriginalLongitude"),
                  na.strings=c("NULL","","NA"))
    
    parameter <- unique(data$ParameterName)
    unit <- unique(data$ParameterUnits)
    
    #################
    ### FILTERING ###
    #################
    
    # Converts Include to be a logical either TRUE or FALSE
    data$Include <- as.logical(data$Include)
    # Removes any data rows that do not have Include set to TRUE
    data <- data[data$Include==TRUE,]
    # Removes rows that have missing ResultValues
    data <- data[!is.na(data$ResultValue),]
    # Removes rows that have missing RelativeDepth
    data <- data[!is.na(data$RelativeDepth),]
    # Rremoves rows that have an ActivityType with Blank
    data <- data[!grep("Blank", data$ActivityType),]
    
    # Gets list of managed areas for the specific region being looked at
    MA_All_Region <- MA_All[MA_All$Region==region,]
    
    # Gets AreaID for data by merging data with the managed area list for the region
    # data <- merge.data.frame(MA_All_Region[,c("AreaID", "ManagedAreaName")],
    #                          data, by="ManagedAreaName", all=TRUE)
    data <- merge(MA_All_Region[,c("AreaID", "ManagedAreaName")],
                  data, by="ManagedAreaName", all=TRUE)
    
    # Creates MonitoringID to more easily cycle through monitoring locations
    setDT(data)
    data[, MonitoringID := .GRP, by = .(AreaID, ManagedAreaName, ProgramID, ProgramName, ProgramLocationID)]
    
    # Creates function to checks monitoring location for at least 2 years of
    # continuous consecutive data
    ContinuousConsecutiveCheck <- function(con_data){
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
    
    # Stores the MonitoringID that pass the consecutive year check
    consMonthIDs <- ContinuousConsecutiveCheck(data)
    
    # Creates data frame with summary for each monitoring location.
    median_na_rm <- function(x) ifelse(length(x) > 0, median(x, na.rm = TRUE), NA_real_)
    Mon_Summ <- data[, .(ParameterName = parameter,
                         RelativeDepth = unique(RelativeDepth),
                         N_Data = sum(Include == TRUE & !is.na(ResultValue)),
                         N_Years = uniqueN(Year[Include == TRUE & !is.na(Year)]),
                         EarliestYear = min(Year[Include == TRUE]),
                         LatestYear = max(Year[Include == TRUE]),
                         EarliestSampleDate = min(SampleDate[Include == TRUE]),
                         LastSampleDate = max(SampleDate[Include == TRUE]),
                         Median = median_na_rm(ResultValue)), 
                     by = .(MonitoringID, AreaID, ManagedAreaName, ProgramID, ProgramName, ProgramLocationID)]
    
    Mon_Summ[, ConsecutiveMonths := ifelse(unique(MonitoringID) %in% consMonthIDs==TRUE, TRUE, FALSE)]
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
    Mon_IDs <- unique(data$MonitoringID[data$Use_In_Analysis==TRUE])
    Mon_IDs <- Mon_IDs[order(Mon_IDs)]
    n <- length(Mon_IDs)
    
    saveRDS(Mon_IDs, file = paste0(out_dir_tables,"/WC_Continuous_", param_abrev, "_", region, "_Mon_IDs.rds"))
    
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
                        by = .(ManagedAreaName, ProgramID, ProgramName, ProgramLocationID, Use_In_Analysis)]
    
    # Drop rows with NA values
    coordinates <- coordinates[complete.cases(coordinates)]
    
    coordinates$Parameter <- param_abrev
    coordinates$Region <- region
    
    saveRDS(coordinates, file = paste0(out_dir_tables,"/WC_Continuous_", param_abrev, "_", region, "_Station_Coordinates.rds"))
    
    ############################
    #### MANAGED AREA STATS ####
    ############################
    
    # Create summary statistics for each monitoring location based on Year and Month
    # intervals.
    # Filtered data to be used for all summary statistics
    filtered_data <- data[Use_In_Analysis == TRUE]
    
    Mon_YM_Stats <- filtered_data[, .(ParameterName = parameter,
                                      RelativeDepth = unique(RelativeDepth),
                                      EarliestSampleDate = min(SampleDate),
                                      LastSampleDate = max(SampleDate),
                                      N_Data = .N,
                                      Min = min(ResultValue),
                                      Max = max(ResultValue),
                                      Median = median(ResultValue),
                                      Mean = mean(ResultValue),
                                      StandardDeviation = sd(ResultValue)),
                                  by = .(MonitoringID, AreaID, ManagedAreaName, ProgramID, ProgramName, ProgramLocationID, Year, Month)]
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
    print("Saving Mon_YM_Stats.rds")
    saveRDS(Mon_YM_Stats, file = paste0(out_dir_tables,"/WC_Continuous_", param_abrev, "_", region, "_Mon_YM_Stats.rds"))
    
    # Create summary statistics for each monitoring location based on Year
    # intervals.
    Mon_Y_Stats <- filtered_data[, .(ParameterName = parameter,
                                     RelativeDepth = unique(RelativeDepth),
                                     EarliestSampleDate = min(SampleDate),
                                     LastSampleDate = max(SampleDate),
                                     N_Data = .N,
                                     Min = min(ResultValue),
                                     Max = max(ResultValue),
                                     Median = median(ResultValue),
                                     Mean = mean(ResultValue),
                                     StandardDeviation = sd(ResultValue)),
                                 by = .(AreaID, ManagedAreaName, ProgramID, ProgramName, ProgramLocationID, Year)]
    # Puts the data in order based on ManagedAreaName, ProgramID, ProgramName,
    # ProgramLocationID, then Year
    Mon_Y_Stats <- as.data.table(Mon_Y_Stats[order(Mon_Y_Stats$ManagedAreaName,
                                                   Mon_Y_Stats$ProgramID,
                                                   Mon_Y_Stats$ProgramName,
                                                   Mon_Y_Stats$ProgramLocationID,
                                                   Mon_Y_Stats$Year), ])
    
    # Saving RDS object
    print("Saving Mon_Y_Stats.rds")
    saveRDS(Mon_Y_Stats, file = paste0(out_dir_tables,"/WC_Continuous_", param_abrev, "_", region, "_Mon_Y_Stats.rds"))
    
    # Create summary statistics for each monitoring location based on Month
    # intervals.
    Mon_M_Stats <- filtered_data[, .(ParameterName = parameter,
                                     RelativeDepth = unique(RelativeDepth),
                                     EarliestSampleDate = min(SampleDate),
                                     LastSampleDate = max(SampleDate),
                                     N_Data = .N,
                                     Min = min(ResultValue),
                                     Max = max(ResultValue),
                                     Median = median(ResultValue),
                                     Mean = mean(ResultValue),
                                     StandardDeviation = sd(ResultValue)),
                                 by = .(AreaID, ManagedAreaName, ProgramID, ProgramName, ProgramLocationID, Month)]
    # Puts the data in order based on ManagedAreaName, ProgramID, ProgramName,
    # ProgramLocationID, then Month
    Mon_M_Stats <- as.data.table(Mon_M_Stats[order(Mon_M_Stats$ManagedAreaName,
                                                   Mon_M_Stats$ProgramID,
                                                   Mon_M_Stats$ProgramName,
                                                   Mon_M_Stats$ProgramLocationID,
                                                   Mon_M_Stats$Month), ])
    
    # Saving RDS object
    print("Saving Mon_M_Stats.rds")
    saveRDS(Mon_M_Stats, file = paste0(out_dir_tables,"/WC_Continuous_", param_abrev, "_", region, "_Mon_M_Stats.rds"))
    
    # Reduces size of data by getting a monthly average (this is the efficiency bottleneck)
    # New method uses "Collapse" package to more efficiently group & summarise
    data <- data %>%
      fgroup_by(MonitoringID, AreaID, ManagedAreaName, ProgramID, ProgramName,
                ProgramLocationID, SampleDate) %>%
      fsummarise(Year = first(Year), 
                 Month = first(Month),
                 RelativeDepth = first(RelativeDepth),
                 ResultValue = fmean(ResultValue), 
                 Include = first(Include),
                 Use_In_Analysis = first(Use_In_Analysis))
    
    # Sets column formats to appropriate types
    data$SampleDate <- as.Date(data$SampleDate)
    data$YearMonth <- format(data$SampleDate, format = "%m-%Y")
    data$YearMonthDec <- data$Year + ((data$Month-0.5) / 12)
    data$DecDate <- decimal_date(data$SampleDate)
    
    #######################################
    #### SEASONAL KENDALL TAU ANALYSIS ####
    #######################################
    
    # List for column names
    c_names <- c("MonitoringID", "Independent", "tau", "p",
                 "SennSlope", "SennIntercept", "ChiSquared", "pChiSquared", "ub", "lb", "Trend")
    
    skt_stats <- data.frame(matrix(ncol = length(c_names), nrow = n))
    
    colnames(skt_stats) <- c_names
    # Determines if there are any monitoring locations to analyze
    if(n==0){
      print("There are no monitoring locations that qualify.")
    } else{
      # Starts cycling through Monitoring locations to determine seasonal
      # Kendall Tau
      for (i in 1:n) {
        # Gets the number of rows of data for the monitoring location
        data_SKT <- Mon_YM_Stats[Mon_YM_Stats$MonitoringID==Mon_IDs[i], ]
        # Gets station name from monitoring ID
        station_name <- Mon_YM_Stats[Mon_YM_Stats$MonitoringID==Mon_IDs[i], unique(ProgramLocationID)]
        x <- nrow(data_SKT)
        # Perform analysis if there is more than 1 row
        if (x>0) {
          # Store the monitoring location summary statistics to be used in
          # trend analysis
          SKT.med <- Mon_Summ$Median[Mon_Summ$MonitoringID==Mon_IDs[i]]
          SKT.minYr <- Mon_Summ$EarliestYear[Mon_Summ$MonitoringID==Mon_IDs[i]]
          SKT.maxYr <- Mon_Summ$LatestYear[Mon_Summ$MonitoringID==Mon_IDs[i]]
          SKT.ind <- TRUE
          SKT <- kendallSeasonalTrendTest(y=data_SKT$Mean,
                                          season=data_SKT$Month,
                                          year=data_SKT$YearFromStart,
                                          independent.obs=SKT.ind)
          saveRDS(SKT, file=paste0("output/models/",param_abrev,"_",region,"_",station_name,".rds"))
          if(is.na(SKT$estimate[1])==TRUE){
            SKT.ind <- FALSE
            SKT <- kendallSeasonalTrendTest(y=data_SKT$Mean,
                                            season=data_SKT$Month,
                                            year=data_SKT$YearFromStart,
                                            independent.obs=SKT.ind)
          }
          skt_stats$MonitoringID[i] <- Mon_IDs[i]
          skt_stats$Independent[i] <- SKT.ind
          skt_stats$tau[i] <- SKT$estimate[1]
          skt_stats$p[i] <- SKT$p.value[2]
          skt_stats$SennSlope[i] <- SKT$estimate[2]
          skt_stats$SennIntercept[i] <- SKT$estimate[3]
          skt_stats$ChiSquared[i] <- SKT$statistic[1]
          skt_stats$pChiSquared[i] <- SKT$p.value[1]
          skt_stats$ub[i] <- SKT$interval$limits["UCL"]
          skt_stats$lb[i] <- SKT$interval$limits["LCL"]
          # If the p value is less than 5% and the slope is greater than 10% of the
          # median value, the trend is large (2).
          if (skt_stats$p[i] < .05 & abs(skt_stats$SennSlope[i]) >
              abs(SKT.med) / 10.) {
            skt_stats$Trend[i] <- 2
            
            # If the p value is less than 5% and the slope is less than 10% of the
            # median value, there is a trend (1).
          }else if (skt_stats$p[i] < .05 & abs(skt_stats$SennSlope[i]) <
                    abs(SKT.med) / 10.) {
            skt_stats$Trend[i] <- 1
            
            # Otherwise, there is no trend (0)
          }else {
            skt_stats$Trend[i] <- 0
          }
          # Sets the sign of the trend based on Senn Slope direction
          if (skt_stats$SennSlope[i] <= 0) {
            skt_stats$Trend[i] <- -skt_stats$Trend[i]
          }
        }
      }
      
      # Stores as data frame
      skt_stats <- as.data.frame(skt_stats)
      
    }
    # Clears unused variables
    rm(SKT, data_SKT, x, SKT.med, SKT.minYr, SKT.maxYr, SKT.ind)
    # Combines the skt_stats with Mon_Summ
    skt_stats <-  merge.data.frame(Mon_Summ, skt_stats,
                                   by=c("MonitoringID"), all=TRUE)
    
    skt_stats <- as.data.table(skt_stats[order(skt_stats$MonitoringID), ])
    
    # Sets variables to proper format and rounds values if necessary
    skt_stats$tau <- as.numeric(skt_stats$tau)
    skt_stats$p <- format(as.numeric(skt_stats$p),
                          scientific=FALSE)
    skt_stats$SennSlope <- as.numeric(skt_stats$SennSlope)
    skt_stats$SennIntercept <- as.numeric(skt_stats$SennIntercept)
    skt_stats$ChiSquared <- round(as.numeric(skt_stats$ChiSquared), digits=4)
    skt_stats$pChiSquared <- round(as.numeric(skt_stats$pChiSquared), digits=4)
    skt_stats$Trend <- as.integer(skt_stats$Trend)
    
    print("Saving SKT_stats.rds")
    
    saveRDS(skt_stats, file = paste0(out_dir_tables,"/WC_Continuous_", param_abrev, "_", region, "_skt_stats.rds"))
    saveRDS(select(skt_stats, -c(EarliestSampleDate)), file = paste0(out_dir_tables,"/WC_Continuous_", param_abrev, "_", region, "_KendallTau_Stats.rds"))
    
    # Removes data rows with no ResultValue (created by merging with MA_All)
    data <- data[!is.na(data$ResultValue),]
    
    # Saving overall data object for each region
    saveRDS(data, file = paste0(out_dir_tables,"/WC_Continuous_", param_abrev, "_", region, "_data.rds"))
    
    # Save monitoring station info to show number of cont. stations in report
    stations <- data %>%
      group_by(ManagedAreaName) %>%
      summarise(Stations = unique(ProgramLocationID)) %>%
      mutate(Region = region)
    
    # append stations to list for each region
    cont_station_list[[region]] <- stations
  }
  
  cont_file_list_df <- bind_rows(cont_file_list)
}

# bind coordinates together
coordinates_df <- bind_rows(coordinates, coordinates_df)

# Save Coordinates data frame
saveRDS(coordinates_df, file = paste0(out_dir_tables,"/WC_Continuous_coordinates.rds"))

# write file_lists to file
fwrite(cont_file_list_df, "output/tables/cont/cont_file_list.txt", sep='|')

# combine all station lists for each region & write to file
cont_station_df <- bind_rows(cont_station_list)
fwrite(cont_station_df, "output/tables/cont/cont_station_list.txt", sep='|')

toc()
End_time <- Sys.time()

print(paste0("Start time: ", Start_time))
print(paste0("End time: ", End_time))