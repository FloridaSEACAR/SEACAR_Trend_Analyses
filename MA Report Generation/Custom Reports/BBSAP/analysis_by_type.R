# Stand-alone script to perform data manipulations and run analyses

library(tictoc)
library(lubridate)
library(EnvStats)

## WQ Discrete ----

# Number of years of data required for SKT analysis
suff_years <- 10

# Depths to analyse
all_depths <- c("Surface", "Bottom", "All")
# Activity types to analyse
all_activity <- c("Field", "Lab", "All")
# List all parameters featured
all_params <- unique(wq_data$ParameterName)

# Define SEACAR-approved theme
plot_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(family="Arial"),
        plot.title=element_text(hjust=0.5, size=12, color="#314963"),
        plot.subtitle=element_text(hjust=0.5, size=10, color="#314963"),
        legend.title=element_text(size=10),
        legend.text.align = 0,
        axis.title.x = element_text(size=10, margin = margin(t = 5, r = 0,
                                                             b = 10, l = 0)),
        axis.title.y = element_text(size=10, margin = margin(t = 0, r = 10,
                                                             b = 0, l = 0)),
        axis.text=element_text(size=10),
        axis.text.x=element_text(angle = 60, hjust = 1))

# Defines lists of params and their respective activity types
# Determines which parameters should be analyzed for each activity type
field_params <- c("Dissolved Oxygen",
                  "Dissolved Oxygen Saturation",
                  "pH",
                  "Secchi Depth",
                  "Turbidity",
                  "Water Temperature")
lab_params <- c("Chlorophyll a, Corrected for Pheophytin",
                "Chlorophyll a, Uncorrected for Pheophytin",
                "Colored Dissolved Organic Matter",
                "Total Nitrogen",
                "Total Phosphorus",
                "Turbidity")
combined_params <- c("Salinity",
                     "Total Suspended Solids")

# Parameter looping ----
tic()
for(i in 1:length(all_params)){
  param <- all_params[i]
  print(paste0("Starting parameter: ", param))
  
  for(depth in all_depths){
    # Because secchi depth does not have bottom measurement
    # skip secchi for bottom
    if(param=="Secchi Depth" & (depth=="Bottom" | depth=="All")){next}
    
    # Begin looping through activity types
    # Skip for parameters that aren't analyzed for a given activity type
    for(activity in all_activity){
      if(activity=="Field" & !param %in% field_params){
        next
      } else if(activity=="Lab" & !param %in% lab_params){
        next
      } else if(activity=="All" & !param %in% combined_params){
        next
      }
      
      ### Begin Data Creation ###
      # Subset data for each parameter
      data <- wq_data[ParameterName==param, ]
      data <- data[Include==1 & MADup==1, ]
      
      # grab unit of measurement
      unit <- unique(data$ParameterUnits)
      
      ### Filtering ### ----
      # Changes "Sample" to "Lab" for activity type
      data$ActivityType <- gsub("Sample", "Lab", data$ActivityType)
      
      # Gets data for a specific activity type if it is not "All"
      if(activity!="All"){
        data <- data[grep(activity, data$ActivityType), ]
      }
      
      # Changes RelativeDepth to Bottom for QAQC flag 12Q that indicates
      # measurements are both surface and bottom if relative depth is bottom
      if(depth=="Bottom"){
        data$RelativeDepth[grep("12Q", data$SEACAR_QAQCFlagCode[
          data$RelativeDepth=="Surface"])] <- "Bottom"
      }
      
      # Removes missing RelativeDepth data and data for RelativeDepth
      # not of interest from all parameters except Secchi Depth
      if(param!="Secchi Depth" & depth!="All"){
        data <- data[!is.na(RelativeDepth), ]
        data <- data[RelativeDepth==depth, ]
      }
      
      # Changes Include to be logical (T/F)
      data$Include <- as.logical(data$Include)
      
      # Change include to false for secchi depth with U ValueQualifier
      if(param=="Secchi Depth"){
        data$Include[grep("U", data$ValueQualifier)] <- FALSE
      }
      
      # Function to check managed area for at least 2 yrs of 
      # continuous/consecutive data
      DiscreteConsecutiveCheck <- function(con_data){
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
      # Stores the AreaID that pass the consecutive year check
      consMonthIDs <- DiscreteConsecutiveCheck(data)
      
      # Creates data frame with summary for each managed area
      MA_Summ <- data %>%
        group_by(System, Type) %>%
        summarize(ParameterName=param,
                  RelativeDepth=depth,
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
                  Median=median(ResultValue[Include==TRUE & N_Data!=0], na.rm=TRUE))
      
      MA_Summ$ConsecutiveMonths <- NULL
      
      # Creates column in data that determines how many years from the start for each
      # managed area
      data <- data %>%
        group_by(System, Type) %>%
        mutate(YearFromStart=Year-min(Year))
      # Adds SufficientData column to data table based on managed area
      data <- merge.data.frame(data, MA_Summ[,c("System", "Type", "SufficientData")],
                               by=c("System","Type"))
      # Creates Use_In_Analysis column for data that is determined if the row has
      # Include value of TRUE and SufficientData value of TRUE
      data$Use_In_Analysis <- ifelse(data$Include==TRUE & data$SufficientData==TRUE,
                                     TRUE, FALSE)
      # Rearranges the summary data frame columns to be System,
      # ParameterName RelativeDepth, ActivityType, SufficientData, everything else
      MA_Summ <- MA_Summ %>%
        select(System, Type, ParameterName, RelativeDepth, ActivityType,
               SufficientData, everything())
      # Puts summary data in order based on managed area
      MA_Summ <- as.data.frame(MA_Summ[order(MA_Summ$System), ])
      # Put SampleDate as date object
      data$SampleDate <- as.Date(data$SampleDate)
      # Creates character object for Month and Year
      data$YearMonth <- paste0(data$Month, "-", data$Year)
      # Creates variable that puts year and month into a decimal year format
      data$YearMonthDec <- data$Year + ((data$Month-0.5) / 12)
      # Converts SampleDate to a decimal date
      data$DecDate <- decimal_date(data$SampleDate)
      
      # Get list of systems to be used in analysis
      sys_include <- MA_Summ$System[MA_Summ$SufficientData==TRUE]
      # number of systems included
      n <- length(sys_include)
      
      ### Determining ValueQualifers ###
      
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
      inc_H <- ifelse(param=="pH" | param=="Dissolved Oxygen" |
                        param=="Dissolved Oxygen Saturation", TRUE, FALSE)
      # Loops through conditions to determine what indicators to include in plots.
      # If H should be included
      if (inc_H==TRUE){
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
      } else if (param=="Secchi Depth") {
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
                   "U Codes: ", count_U, " (", round(perc_U, 6), "%)"))
        # For all other scenarios
      } else{
        # Remove all ValueQualifier except U
        data$VQ_Plot <- gsub("[^U]+", "", data$VQ_Plot)
        # Sets any blank character ValueQualifier to be NA
        data$VQ_Plot[data$VQ_Plot==""] <- NA
        # Prints the number and percentage of I, Q, U
        cat(paste0("Number of Measurements: ", total,
                   ", Number Passed Filter: ", pass_filter, "\n",
                   "I Codes: ", count_I, " (", round(perc_I, 6), "%)\n",
                   "Q Codes: ", count_Q, " (", round(perc_Q, 6), "%)\n",
                   "U Codes: ", count_U, " (", round(perc_U, 6), "%)"))
      }
      
      # Creates a data table that summarizes the number and percentage of
      # ValueQualifier H, I, Q, S, and U for each managed area each year
      data_summ <- data %>%
        group_by(System, Type, Year) %>%
        summarize(ParameterName=param,
                  RelativeDepth=depth,
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
                  perc_U=100*N_U/length(ValueQualifier))
      # Orders the data table rows based on managed area name
      data_summ <- as.data.table(data_summ[order(data_summ$System,
                                                 data_summ$Year), ])
      
      # Store results in directory
      data_directory[["data_summ"]][[param]] <- data_summ
      
      if(nrow(data[data$Use_In_Analysis==TRUE, ])){
        # Create Summary statistics for each System based on Y and M intervals
        YM_Stats <- data[data$Use_In_Analysis==TRUE, ] %>%
          group_by(System, Type, Year, Month) %>%
          summarize(ParameterName=param,
                    RelativeDepth=depth,
                    ActivityType=activity,
                    N_Data=length(ResultValue),
                    Min=min(ResultValue),
                    Max=max(ResultValue),
                    Median=median(ResultValue),
                    Mean=mean(ResultValue),
                    StandardDeviation=sd(ResultValue),
                    Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                                   collapse=", "),
                    ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                                     collapse=", "))
        
        # Set order
        YM_Stats <- as.data.table(YM_Stats[order(YM_Stats$System,
                                                 YM_Stats$Year,
                                                 YM_Stats$Month), ])
        # Get year from start for each system used in SKT analysis
        YM_Stats <- YM_Stats %>%
          group_by(System, Type) %>%
          mutate(YearFromStart=Year-min(Year))
        
        # Create decimal value of year and month values
        YM_Stats$YearMonthDec <- YM_Stats$Year + ((YM_Stats$Month-0.5)/12)
        
        # Store results
        data_directory[["YM_Stats"]][[param]] <- YM_Stats
        
        ### SKT Analysis ----
        
        skt_stats <- data.frame()
        
        setDT(YM_Stats)
        sys_types <- YM_Stats %>% group_by(System, Type) %>% distinct(System, Type)
        # Determines if there are any systems to analyze
        if(n==0){
          print("There are no Systems that qualify.")
        } else{
          # Starts cycling through systems to determine seasonal Kendall Tau
          for (i in 1:nrow(sys_types)) {
            sys <- sys_types$System[i]
            type <- sys_types$Type[i]
            # Gets the number of rows of data for the managed area
            data_SKT <- YM_Stats[YM_Stats$System==sys & YM_Stats$Type==type, ]
            x <- nrow(data_SKT)
            # Perform analysis if there is more than 1 row
            if (x>0) {
              # Store the managed area summary statistics to be used in
              # trend analysis
              SKT.med <- MA_Summ$Median[MA_Summ$System==sys & MA_Summ$Type==type]
              SKT.minYr <- MA_Summ$EarliestYear[MA_Summ$System==sys & MA_Summ$Type==type]
              SKT.maxYr <- MA_Summ$LatestYear[MA_Summ$System==sys & MA_Summ$Type==type]
              SKT.ind <- TRUE
              SKT <- kendallSeasonalTrendTest(y=data_SKT$Mean,
                                              season=data_SKT$Month,
                                              year=data_SKT$YearFromStart,
                                              independent.obs=SKT.ind)
              if(is.na(SKT$estimate[1])==TRUE){
                SKT.ind <- FALSE
                SKT <- kendallSeasonalTrendTest(y=data_SKT$Mean,
                                                season=data_SKT$Month,
                                                year=data_SKT$YearFromStart,
                                                independent.obs=SKT.ind)
              }
              
              # Store results in dataframe
              skt_stats_df <- data.frame(
                "System" = sys,
                "Type" = type,
                "Independent" = SKT.ind,
                "tau" = SKT$estimate[1],
                "p" = SKT$p.value[2],
                "SennSlope" = SKT$estimate[2],
                "SennIntercept" = SKT$estimate[3],
                "ChiSquared" = SKT$statistic[1],
                "pChiSquared" = SKT$p.value[1]
              )
              
              # If the p value is less than 5% and the slope is greater than 10% of the
              # median value, the trend is large (2).
              if (skt_stats_df$p < .05 & abs(skt_stats_df$SennSlope) >
                  abs(SKT.med) / 10.) {
                skt_stats_df$Trend <- 2
                
                # If the p value is less than 5% and the slope is less than 10% of the
                # median value, there is a trend (1).
              }else if (skt_stats_df$p < .05 & abs(skt_stats_df$SennSlope) <
                        abs(SKT.med) / 10.) {
                skt_stats_df$Trend <- 1
                
                # Otherwise, there is no trend (0)
              }else {
                skt_stats_df$Trend <- 0
              }
              # Sets the sign of the trend based on Senn Slope direction
              if (skt_stats_df$SennSlope <= 0) {
                skt_stats_df$Trend <- -skt_stats_df$Trend
              }
              skt_stats <- bind_rows(skt_stats, skt_stats_df)
            }
          }
          
          # Stores as data frame
          skt_stats <- as.data.frame(skt_stats)
          # Clears unused variables
          rm(SKT, data_SKT, x, SKT.med, SKT.minYr, SKT.maxYr, SKT.ind)
        }
        
        # Combines the skt_stats with MA_Summ
        skt_stats <-  merge.data.frame(MA_Summ, skt_stats,
                                       by=c("System", "Type"), all=TRUE)
        
        skt_stats <- as.data.table(skt_stats[order(skt_stats$System), ])
        
        # Sets variables to proper format and rounds values if necessary
        skt_stats$tau <- round(as.numeric(skt_stats$tau), digits=4)
        skt_stats$p <- format(round(as.numeric(skt_stats$p), digits=4),
                              scientific=FALSE)
        skt_stats$SennSlope <- as.numeric(skt_stats$SennSlope)
        skt_stats$SennIntercept <- as.numeric(skt_stats$SennIntercept)
        skt_stats$ChiSquared <- round(as.numeric(skt_stats$ChiSquared), digits=4)
        skt_stats$pChiSquared <- round(as.numeric(skt_stats$pChiSquared), digits=4)
        skt_stats$Trend <- as.integer(skt_stats$Trend)
        
        # Save results to directory
        data_directory[["skt_stats"]][[param]] <- skt_stats
        
        # Save overall data file
        data_directory[["data_analysis"]][[param]] <- data        
        
      }
      
      # COMPUTE YM STATS FOR ALL DATA
      # Create Summary statistics for each System based on Y and M intervals
      YM_Stats2 <- data %>%
        group_by(System, Type, Year, Month) %>%
        summarize(ParameterName=param,
                  RelativeDepth=depth,
                  ActivityType=activity,
                  N_Data=length(ResultValue),
                  Min=min(ResultValue),
                  Max=max(ResultValue),
                  Median=median(ResultValue),
                  Mean=mean(ResultValue),
                  StandardDeviation=sd(ResultValue),
                  Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                                 collapse=", "),
                  ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                                   collapse=", "))
      
      # Set order
      YM_Stats2 <- as.data.table(YM_Stats2[order(YM_Stats2$System,
                                                 YM_Stats2$Year,
                                                 YM_Stats2$Month), ])
      # Get year from start for each system used in SKT analysis
      YM_Stats2 <- YM_Stats2 %>%
        group_by(System, Type) %>%
        mutate(YearFromStart=Year-min(Year))
      
      # Create decimal value of year and month values
      YM_Stats2$YearMonthDec <- YM_Stats2$Year + ((YM_Stats2$Month-0.5)/12)
      
      # Store results
      data_directory[["YM_Stats2"]][[param]] <- YM_Stats2
      
    }
  }
}
toc()