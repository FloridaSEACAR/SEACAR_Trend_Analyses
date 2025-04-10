library(stringr)
library(dplyr)
library(data.table)
library(ggplot2)
library(tictoc)
library(rstudioapi)
library(lubridate)
library(scales)
library(EnvStats)
library(tidyr)
library(glue)
library(grid)
library(leaflet)
library(mapview)
library(sf)

# Gets directory of this script and sets it as the working directory
wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

##########
# Source in Continuous and Discrete data creation scripts
# These scripts process the WQ data and generate the statistical files necessary for plots and report generation.
# Outputs multiple data objects, including SeasonalKendallTau (necessary to generate SKT summary files using WQ_KendallTau_Stats_Combine.R)
# These scripts should be run when there are new combined tables available
# Comment out if they have been run for this export, Cont. processing takes ~1.5hr

# source("WQ_Discrete_Data_Creation.R", echo=T)
# source("WQ_Continuous_Data_Creation.R", echo=T)

# WQ_KendallTau_Stats_Combine produces the following files needed for Atlas updates
# WQ_Discrete_All_KendallTau_Stats & WQ_Continuous_All_KendallTau_Status

# source("WQ_KendallTau_Stats_Combine.R", echo=T)

##########

# save_plots variable == TRUE will save plots
save_plots <- TRUE
# render_reports variable == TRUE renders reports
render_reports <- TRUE
# save_maps variable == TRUE will render maps (for MA Report Gen)
save_maps <- TRUE
# Import necessary shapefiles if save_maps is TRUE
if(save_maps){
  source("load_shape_files.R", echo=T)
}

# Set height and width for plot outputs (.png outputs only)
h <- 891
w <- 1600

# Set color palette for sig and non-sig trendlines
sig_color <- "#000099"
nonsig_color <- "#900667"

# Change folder date to select which objects to load (if plots need to match Atlas)
# "most recent" to use the latest MA Report Generation outputs
disc_folder_date <- "most recent"
cont_folder_date <- "most recent"

# Point to location where Disc objects are located
data_obj_loc <- "output/tables/"

disc_loc <- ifelse(disc_folder_date=="most recent", 
                   paste0(data_obj_loc,"disc/"), 
                   paste0(data_obj_loc,"disc/",disc_folder_date,"/"))
cont_loc <- ifelse(cont_folder_date=="most recent",
                   paste0(data_obj_loc,"cont/"),
                   paste0(data_obj_loc,"cont/",cont_folder_date,"/"))

# Lists of disc and cont .rds objects to read
disc_files <- list.files(disc_loc,pattern = "\\.rds$", full.names = T)
cont_files <- list.files(cont_loc,pattern = "\\.rds$", full.names = T)

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, 
                stringsAsFactors = FALSE, na.strings = "")
# Load websiteParams to determine which plots to show
websiteParams <- fread("data/WebsiteParameters.csv", sep=",", header = TRUE, 
                       stringsAsFactors = FALSE, na.strings = "")
# Correct order of websiteParams to match format from the Atlas
websiteParams <- websiteParams %>% 
  arrange(factor(IndicatorName, levels = c("Nutrients","Water Quality","Water Clarity")),
          factor(ParameterName, levels = c("Total Nitrogen","Total Phosphorus",
                                           "Dissolved Oxygen", "Dissolved Oxygen Saturation", "Salinity", "Water Temperature", "pH",
                                           "Turbidity", "Total Suspended Solids", "Chlorophyll a, Uncorrected for Pheophytin",
                                           "Chlorophyll a, Corrected for Pheophytin", "Secchi Depth", "Colored Dissolved Organic Matter"))) %>%
  filter(Website==1)
setDT(websiteParams)

# Load in Database_Thresholds output from IndicatorQuantiles
# Quantiles are applied only to the viewing windows when final plots are displayed.
# Does not influence / remove values from analyses
db_thresholds <- setDT(openxlsx::read.xlsx("../../IndicatorQuantiles/output/ScriptResults/Database_Thresholds.xlsx", startRow = 6))
db_thresholds <- db_thresholds[Habitat=="Water Column" & IndicatorName!="Nekton" & 
                                 ParameterName!=	"Fluorescent dissolved organic matter, FDOM" &
                                 ThresholdID!=31, 
                               c("CombinedTable", "ParameterName", "LowQuantile", "HighQuantile")]
# Create function to retrieve high and low quantile values
set_view_window <- function(plot_data, type = "Discrete WQ"){
  param <- unique(plot_data$ParameterName)
  # Invert sign for Secchi Depth calculation
  if(param=="Secchi Depth"){plot_data$Mean <- -plot_data$Mean}
  # Grab min and max Y values from data
  miny <- min(plot_data$Mean)
  maxy <- max(plot_data$Mean)
  # Grab quantile limits for a given parameter
  low_q <- db_thresholds[ParameterName==param & CombinedTable==type, LowQuantile]
  high_q <- db_thresholds[ParameterName==param & CombinedTable==type, HighQuantile]
  # Is the lowest value below quantile limit?
  if(miny<low_q){
    # If so, use the next lowest value not below limit
    miny <- plot_data %>% filter(!Mean < low_q) %>% pull(Mean) %>% min()
  }
  # Check to see if the lowest value conflicts with trendline values (if trendline exists)
  # This ensures that the entire trendline is shown, even when it starts below the values
  if(TRUE %in% unique(plot_data$SufficientData)){
    if(miny>min(plot_data$start_y, na.rm = T) | miny>min(plot_data$end_y, na.rm = T)){
      miny <- min(min(plot_data$start_y, na.rm = T), min(plot_data$end_y, na.rm = T))
    }  
  }
  # Is the highest value above quantile limit?
  if(maxy>high_q){
    # If so, use the next highest value not above limit
    maxy <- plot_data %>% filter(!Mean > high_q) %>% pull(Mean) %>% max()
  }
  # Check to see if the highest value conflicts with trendline values (if trendline exists)
  # This ensures that the entire trendline is shown, even when it starts above the values
  if(TRUE %in% unique(plot_data$SufficientData)){
    if(maxy<max(plot_data$start_y, na.rm = T) | maxy<max(plot_data$end_y, na.rm = T)){
      maxy <- max(max(plot_data$start_y, na.rm = T), max(plot_data$end_y, na.rm = T))
    }  
  }
  # Uninvert sign for Secchi Depth (flip min and max, ensure 0 at top)
  if(param=="Secchi Depth"){
    miny <- -maxy
    maxy <- 0
  }
  return(c(miny, maxy))
}

# Short parameter names for selecting .rds objects
all_params_short <- unique(websiteParams$ParameterShort)
cont_params_short <- websiteParams[SamplingFrequency=="Continuous", unique(ParameterShort)]

# function of parameter, activity type, depth, with specified filetype
# retrieves RDS filepath to be loaded
get_files <- function(p, a, d, filetype) {
  # "data" contains overall data for each param, regardless of depth/activity
  if (filetype == "data") {
    pattern <- paste0(p,"_",filetype)
  } else {
    pattern <- paste0(p,"_",a,"_",d,"_",filetype)
  }
  # subset directory files for given pattern
  file_return <- str_subset(disc_files, pattern)
  return(file_return)
}

#function to check the number of managed areas for each p,a,d combination
n_managedareas <- function(p, a, d) {
  # Declaring n value as count of managed areas
  # return 0 if unable to load file (activity/depth combo not available for that param)
  n <- tryCatch(
    {
      ma_file <- get_files(p, a, d, "MA_Include")
      ma_inclusion <- readRDS(ma_file)
      n <- length(ma_inclusion)
      rm(ma_inclusion)
      n
    },
    error = function(e) {
      0
    },
    warning = function(w) {
      0
    }
  )
  return(n)
}

#function to make a list of managed area names
get_managed_area_names <- function(p, a, d) {
  ma_list <- with(
    readRDS(paste0(get_files(p, a, d, "MA_MMYY"))),
    {
      unique(ManagedAreaName)
    }
  )
  return(list(ma_list))
}

#results list to record managed areas for each combination
results_list <- list()
# Record which combinations of Disc files to select/load
disc_file_list <- c()

for (param in all_params_short) {
  if (param == "Secchi"){
    depth <- "Surface"
  } else {
    depth <- "All"
  }
  
  # Choosing which analyses to plot, when to combine 
  if(param == "Secchi"){depth <- "Surface"} else {depth <- "All"}
  if(param %in% c("ChlaC", "Chla", "CDOM", "TN", "TP")){activity = "Lab"} 
  else if(param %in% c("DO","DOS","pH","Secchi","TempW")){activity = "Field"} 
  else if (param %in% c("Sal","TSS","Turb")){activity = "All"}
  
  n <- n_managedareas(param, activity, depth)
  
  if (n > 0) {
    print(n)
    managed_area_names <- get_managed_area_names(param, activity, depth)
    
    # Concatenate the managed area names into a single character vector
    concatenated_names <- unlist(managed_area_names)
    
    # Create a data frame for the current combination
    result_df <- data.frame(Parameter = param,
                            Depth = depth,
                            Activity = activity,
                            ManagedAreaName = paste(concatenated_names))
    
    # Append the result data frame to the list
    results_list <- c(results_list, list(result_df))
    rm(result_df, concatenated_names, managed_area_names, n)
    
  } else {
    print(0)
  }
  
  file_pattern <- paste0("_",param,"_",activity,"_",depth)
  file <- str_subset(disc_files, file_pattern)
  disc_file_list <- c(disc_file_list, file)
}

# Bind the list of data frames using bind_rows()
managed_area_df <- setDT(bind_rows(results_list))

# Discrete & Continuous Combine necessary tables ----
# Combine continuous/disc trend result tables for faster plot generation
for(type in c("disc", "cont")){
  # Specify table names and file lists for disc and cont
  if(type=="disc"){
    tables <- c("MA_MMYY_Stats", "skt_stats")
    files <- disc_files
  } else {
    tables <- c("Mon_YM_Stats", "skt_stats")
    files <- cont_files
  }
  
  for(table in tables){
    # Subset for desired RDS files
    table_file <- str_subset(files, table)
    # importing RDS files
    df <- lapply(table_file, readRDS)
    # Combine all regions into 1 single output dataframe
    output <- do.call(rbind, df)
    # Create variable of same name
    eval(call("<-", as.name(paste0(table,"_",type)),output))
  }
}

# Apply preliminary trend-text logic
skt_stats_disc <- skt_stats_disc %>% 
  mutate("Period of Record" = paste0(EarliestYear, " - ", LatestYear),
         "Statistical Trend" = ifelse(p <= 0.05 & SennSlope > 0, "Significantly increasing trend",
                                      ifelse(p <= 0.05 & SennSlope < 0, "Significantly decreasing trend", 
                                             ifelse(SufficientData==FALSE, "Insufficient data to calculate trend",
                                                    ifelse(SufficientData==TRUE & is.na(SennSlope), "Model did not fit the available data", 
                                                           ifelse(is.na(Trend), "Insufficient data to calculate trend","No significant trend"))))))
skt_stats_disc[is.na(Trend), `:=` ("Statistical Trend" = "Insufficient data to calculate trend")]
skt_stats_disc[str_detect("NA", p), `:=` ("p" = NA)]
saveRDS(skt_stats_disc, file = "output/tables/disc/skt_stats_disc.rds")

skt_stats_cont <- skt_stats_cont %>% 
  mutate("Period of Record" = paste0(EarliestYear, " - ", LatestYear),
         "Statistical Trend" = ifelse(p <= 0.05 & SennSlope > 0, "Significantly increasing trend",
                                      ifelse(p <= 0.05 & SennSlope < 0, "Significantly decreasing trend", 
                                             ifelse(SufficientData==FALSE, "Insufficient data to calculate trend",
                                                    ifelse(SufficientData==TRUE & is.na(SennSlope), "Model did not fit the available data", 
                                                           ifelse(is.na(Trend), "Insufficient data to calculate trend","No significant trend"))))))
skt_stats_cont[is.na(Trend), `:=` ("Statistical Trend" = "Insufficient data to calculate trend")]

# Combine all discrete data into a single output file
data_output_disc <- setDT(do.call(rbind, lapply(str_subset(disc_files, "data"), readRDS)))
saveRDS(data_output_disc, file = "output/tables/disc/data_output_disc.rds")

# # Create subset of all available "overall data" files so data can be included 
# # from ALL stations not just the stations with successful Trends
cont_data_files <- str_subset(cont_files, "_data.rds")

# # Append to directory-style archive for more efficient access within reports
data_output_cont <- list()
for(param in cont_params_short){
  # Combines data files for all regions for a given parameter
  param_files <- str_subset(cont_data_files, paste0("_",param,"_"))
  # Full ParameterName
  parameter <- websiteParams[ParameterShort==param, unique(ParameterName)]
  # Read in data files for each region and combine
  data <- setDT(do.call(rbind, lapply(param_files, readRDS)))
  # Included data only. Group and determine summary stats
  data <- data[Include==1, ]
  data <- data[, .(ParameterName = parameter,
                   RelativeDepth = unique(RelativeDepth),
                   EarliestSampleDate = min(SampleDate),
                   LastSampleDate = max(SampleDate),
                   N_Data = .N,
                   Min = min(ResultValue),
                   Max = max(ResultValue),
                   Median = median(ResultValue),
                   Mean = mean(ResultValue),
                   StandardDeviation = sd(ResultValue)),
               by = .(MonitoringID, AreaID, ManagedAreaName, ProgramID,
                      ProgramName, ProgramLocationID, Year, Month)]
  # Add decimal YearMonth
  data$YearMonthDec <- data$Year + ((data$Month-0.5) / 12)
  # Save into data_output_cont directory
  data_output_cont[[param]] <- data
  print(paste0(param," processing complete"))
}
# Combine all continuous results together
cont_data_combined <- bind_rows(data_output_cont)
# Grab skt results (start and end of x and y to plot trendlines)
KT.Plot <- skt_stats_cont %>%
  group_by(ProgramID, ProgramLocationID, ManagedAreaName, ParameterName) %>%
  reframe(start_x=decimal_date(EarliestSampleDate),
          start_y=(start_x-EarliestYear)*SennSlope+SennIntercept,
          end_x=decimal_date(LastSampleDate),
          end_y=(end_x-EarliestYear)*SennSlope+SennIntercept)
KT.Plot <- as.data.table(KT.Plot[order(KT.Plot$ProgramLocationID), ])
KT.Plot <- KT.Plot[!is.na(KT.Plot$end_y),]
KT.Plot <- KT.Plot[!is.na(KT.Plot$start_y),]
setDT(KT.Plot)

# Combine into single file for more efficient plotting
cont_plot_data <- merge(
  cont_data_combined, 
  KT.Plot,
  by=c("ProgramID", "ProgramLocationID", "ParameterName", "ManagedAreaName"),
  all = TRUE)

# Merge in p-val to plot significance
cont_plot_data <- merge(
  cont_plot_data, 
  skt_stats_cont[, c("p","ProgramID","ProgramLocationID","ParameterName",
                     "ManagedAreaName", "SufficientData")],
  by=c("ProgramID","ProgramLocationID","ParameterName","ManagedAreaName"))
# Create significant column
setDT(cont_plot_data)
cont_plot_data[SufficientData==TRUE, `:=` (
  sig = ifelse(p<=0.05, "Significant trend", "Non-significant trend")
)]
cont_plot_data[ , `:=` (label = paste0(ProgramLocationID, " - ", RelativeDepth))]

## Setting plot theme for plots
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
        axis.text.x=element_text(angle = -45, hjust = 0))

# Full parameter names and units
# Create dataframe containing that info so that 
# full data file doesn't have to load each time
cont_params_long <- c("Dissolved Oxygen","Dissolved Oxygen Saturation","pH",
                      "Salinity","Turbidity","Water Temperature")
cont_params_short <- c("DO","DOS","pH","Sal","Turb","TempW")
cont_param_units <- c("mg/L","%","pH","ppt","NTU","Degrees C")
cont_regions <- c("NE","NW","SE","SW")
cont_param_df <- data.table(param_short = cont_params_short,
                            parameter = cont_params_long,
                            unit = cont_param_units)
# function to determine x-axis breaks and labels
breaks <- function(plot_data, type="Discrete", ret="break"){
  if(type=="Discrete"){
    #Determine max and min time (Year) for plot x-axis
    t_min <- min(plot_data$Year)
    t_max <- max(plot_data$YearMonthDec)
    t_max_brk <- as.integer(round(t_max, 0))
    t <- t_max-t_min
    
    # Sets break intervals based on the number of years spanned by data
    if(t>=30){
      brk <- -10
    }else if(t<30 & t>=10){
      brk <- -4
    }else if(t<10){
      brk <- -1
    }
  }
  
  if(type=="Continuous"){
    #Determine max and min time (Year) for plot x-axis
    t_min <- min(plot_data$Year)
    t_max <- max(plot_data$YearMonthDec)
    t_max_brk <- as.integer(round(t_max, 0))
    t <- t_max-t_min
    min_RV <- min(plot_data$Mean)
    
    # Creates break intervals for plots based on number of years of data
    if(t>=30){
      # Set breaks to every 10 years if more than 30 years of data
      brk <- -10
    }else if(t<30 & t>=10){
      # Set breaks to every 4 years if between 30 and 10 years of data
      brk <- -4
    }else if(t<10 & t>=4){
      # Set breaks to every 2 years if between 10 and 4 years of data
      brk <- -2
    }else if(t<4 & t>=2){
      # Set breaks to every year if between 4 and 2 years of data
      brk <- -1
    }else if(t<2){
      # Set breaks to every year if less than 2 years of data
      brk <- -1
      # Sets t_max to be 1 year greater and t_min to be 1 year lower
      # Forces graph to have at least 3 tick marks
      t_max <- t_max+1
      t_min <- t_min-1
    }
  }
  
  if(ret=="break"){
    return(seq(t_max_brk, t_min, brk))
  }
  
  if(ret=="lims"){
    return(c(t_min-0.25, t_max+0.25))
  }
}
# function to plot discrete trendlines
plot_trendlines <- function(p, a, d, activity_label, depth_label, y_labels, parameter, skt_data, discrete_data){
  # SKT data
  skt_stats <- skt_data[ParameterName==parameter & RelativeDepth==d & ActivityType==a, ]
  # plot data
  data <- discrete_data[ParameterName==parameter & Include, ]
  if(!a=="All"){
    data <- data[grep(a, data$ActivityType),]
    data$ActivityType <- a
  } else {
    data <- data[grep("Lab|Field", data$ActivityType),]
    data[grep("Lab", data$ActivityType), `:=` (ActivityType = "Lab")]
    data[grep("Field", data$ActivityType), `:=` (ActivityType = "Field")]
  }
  # Generate mean Result Values by Year / Month (monthly means creation)
  data[, Mean := mean(ResultValue, na.rm = TRUE), by = .(Year, Month, ActivityType)]
  
  ### SKT STATS ###
  # Gets x and y values for starting point for trendline
  KT.Plot <- skt_stats %>%
    group_by(ManagedAreaName) %>%
    summarize(start_x=decimal_date(EarliestSampleDate),
              start_y=(start_x-EarliestYear)*SennSlope+SennIntercept,
              end_x=decimal_date(LastSampleDate),
              end_y=(end_x-EarliestYear)*SennSlope+SennIntercept,
              p = unique(p))
  KT.Plot <- as.data.table(KT.Plot[order(KT.Plot$ManagedAreaName), ])
  KT.Plot <- KT.Plot[!is.na(KT.Plot$end_y),]
  KT.Plot <- KT.Plot[!is.na(KT.Plot$start_y),] 
  setDT(KT.Plot)
  
  if (nrow(data) == 0) {invisible()} else {
    cat(glue("## {parameter} - {type} \n"))
    
    # Gets data to be used in plot for managed area
    plot_data <- merge(data, KT.Plot, by=c("ManagedAreaName"), all=TRUE)
    plot_data[, `:=` (sig = ifelse(p<=0.05, "Significant trend", "Non-significant trend"))]
    
    # Create plot object with data and trendline
    p1 <- ggplot(data=plot_data,
                 aes(x=YearMonthDec, y=Mean)) +
      # geom_line(size=0.75, color="#333333", alpha=0.6) +
      geom_point(aes(shape=plot_data$ActivityType), size=3, color="#333333", fill="#cccccc",
                 alpha=0.75) +
      geom_segment(aes(x = start_x, y = start_y, xend = end_x, yend = end_y, 
                       color = sig),
                   linewidth = 1.2, alpha = 0.7, show.legend = TRUE) +
      labs(title=paste0(parameter," - Discrete, ", depth_label),
           subtitle=ma,
           x="Year", y=y_labels) +
      scale_x_continuous(limits=breaks(plot_data, type="Discrete", ret="lims"),
                         breaks=breaks(plot_data, type="Discrete", ret="break")) +
      scale_shape_manual(values = c("Field"=21,"Lab"=24), 
                         name = "Activity type") +
      scale_color_manual(name = "Trend type",
                         values = c("Significant trend" = sig_color,
                                    "Non-significant trend" = nonsig_color),
                         limits = c("Significant trend", "Non-significant trend"),
                         na.translate = FALSE,
                         drop = TRUE) +
      plot_theme +
      guides(col = guide_legend(order = 1), 
             shape = guide_legend(order = 2, override.aes = list(linetype = NA))) + 
      coord_cartesian(ylim = set_view_window(plot_data, "Discrete WQ"))
      # {if(parameter=="Secchi Depth"){ylim(c(min(plot_data$Mean), 0))}} # Adjust Y-axis for (inverted) secchi depth to always display 0 at top 
    
    # Save png
    areaID <- MA_All[Abbreviation==ma_short, AreaID]
    pvID <- websiteParams[SamplingFrequency=="Discrete" & ParameterName==parameter, ParameterVisId]
    filePath <- "output/WQ_Discrete/"
    fileName <- paste0(filePath, "ma-", areaID, "-pv-", pvID, ".png")
    ggsave(filename = fileName,
           plot = p1, width = w, height = h, units = "px", dpi = 300,
           scale = 2)
    saveRDS(plot_data, file=paste0("output/figuredata/discrete_",ma_short, "_", p, ".rds"))
    rm(plot_data)
    rm(MA_YM_Stats)
    rm(skt_stats)
  }
}

# Number of stations above which plots are separated into individual programs
n_cutoff <- 10

# empty dataframe to store whether a continuous MA / PID gets multiple plots
continuous_overview <- data.table()

# function to plot continuous trendlines onto combined plot
plot_trendlines_cont_combined <- function(ma, cont_plot_data, param, y_labels, parameter){
  # Continuous data, including skt results (pre-processed above)
  data <- cont_plot_data[ManagedAreaName==ma & ParameterName==parameter, ]
  # Only perform operations when there are stations to plot
  if(length(unique(data$ProgramLocationID))>0){
    cat(glue("## {parameter} - {type}\n"))
    
    # Set variables for fileName convention
    areaID <- MA_All[ManagedAreaName==ma, AreaID]
    pvID <- websiteParams[SamplingFrequency=="Continuous" & 
                            ParameterName==parameter, ParameterVisId]
    
    filePath <- "output/WQ_Continuous/"
    
    # Account for managed areas with large number of continuous sites
    # Too many to plot together, plot combined by Program
    if(length(unique(data$ProgramLocationID))>n_cutoff){
      for(prog_n in 1:length(unique(data$ProgramID))){
        pid <- unique(data$ProgramID)[prog_n]
        
        # record that this MA / PID contains multiple plots
        temp_overview <- data.table(
          "ManagedAreaName" = ma,
          "ProgramID" = pid,
          "areaID" = areaID,
          "pvID" = pvID,
          "ProgramNumber" = prog_n,
          "fileName" = paste0("ma-", areaID, "-pv-", pvID, ".", prog_n),
          "multiple" = TRUE
        )
        continuous_overview <- bind_rows(continuous_overview, temp_overview)
        rm(temp_overview)
        
        # all plots together for a given ProgramID
        plot_data <- setDT(data[ProgramID==pid, ])
        
        # Program Name
        p_name <- unique(plot_data$ProgramName)
        # number of stations for shape-palette
        n <- length(unique(plot_data$ProgramLocationID))
        # Array of shape values, account for missing ggplot shape values 26-32
        shapes <- c(21,22,23,24,25,seq(1:(n-5)))
        # Create plot
        p1 <- ggplot(data = plot_data, aes(x = YearMonthDec, y = Mean, group = factor(ProgramLocationID))) +
          geom_point(aes(shape = label), color = "#444444", fill = "#cccccc", size = 3, alpha = 0.9, show.legend = TRUE) +
          geom_segment(aes(x = start_x, y = start_y, xend = end_x, yend = end_y, 
                           linetype = label, color = label),
                       linewidth = 1.2, alpha = 0.7, show.legend = TRUE) +
          labs(title = paste0(ma, "\n", p_name, "\nProgramID: ", pid),
               subtitle = paste0(parameter, " - Continuous"),
               x = "Year", y = y_labels) +
          scale_x_continuous(limits = breaks(plot_data, type = "Continuous", ret = "lims"),
                             breaks = breaks(plot_data, type = "Continuous", ret = "break")) +
          scale_shape_manual(values = shapes) +
          scale_linetype_manual(values = 1:n) +
          scale_color_manual(values = 1:n) + 
          labs(shape = "Program location", linetype = "Program location", color = "Program location") + 
          plot_theme +
          theme(legend.text = element_text(size = 7)) +
          coord_cartesian(ylim = set_view_window(plot_data, "Continuous WQ"))
        
        # save fig
        fileName <- paste0(filePath, "ma-", areaID, "-pv-", pvID, ".", prog_n, ".png")
        ggsave(filename = fileName,
               plot = p1, width = w, height = h, units = "px", dpi = 300,
               scale = 2)
      }
    } else {
      # all plots together
      plot_data <- setDT(data)
      
      # record that this MA / PID contains multiple plots
      temp_overview <- data.table(
        "ManagedAreaName" = ma,
        "ProgramID" = NA,
        "areaID" = areaID,
        "pvID" = pvID,
        "ProgramNumber" = NA,
        "fileName" = paste0("ma-", areaID, "-pv-", pvID),
        "multiple" = FALSE
      )
      continuous_overview <- bind_rows(continuous_overview, temp_overview)
      rm(temp_overview)
      
      # number of stations for shape-palette
      n <- length(unique(plot_data$ProgramLocationID))
      shapes <- c(21,22,23,24,25,seq(1:(n-5)))
      # Create plot
      p1 <- ggplot(data=plot_data, aes(x=YearMonthDec, y=Mean, group=factor(ProgramLocationID))) +
        geom_point(aes(shape=label), color="#444444" ,fill="#cccccc", size=3,alpha=0.9, show.legend = TRUE) +
        geom_segment(aes(x = start_x, y = start_y, xend = end_x, yend = end_y, 
                         color = sig, linetype=label),
                     linewidth = 1.2, alpha = 0.7, show.legend = TRUE) +
        labs(title=paste0(ma, "\nAll Stations"),
             subtitle=paste0(parameter, " - Continuous"),
             x="Year", y=y_labels) +
        scale_x_continuous(limits=breaks(plot_data, type="Continuous", ret="lims"),
                           breaks=breaks(plot_data, type="Continuous", ret="break")) +
        plot_theme + 
        scale_shape_manual(values=shapes, name = "Program location") +
        scale_color_manual(name = "Trend type",
                           values = c("Significant trend" = sig_color,
                                      "Non-significant trend" = nonsig_color),
                           limits = c("Significant trend", "Non-significant trend"),
                           na.translate = TRUE,
                           drop = FALSE) +
        labs(shape  = "Program location", linetype = "Program location", colour = "Trend type") +
        guides(
          col = guide_legend("Trend type", order = 2,
                             override.aes = list(shape = NA, colour = c(sig_color, nonsig_color))),
          shape = guide_legend("Program location", order = 1),
          linetype = guide_legend("Program location", order = 1)
        ) +
        coord_cartesian(ylim = set_view_window(plot_data, "Continuous WQ"))
      
      # save fig
      fileName <- paste0(filePath, "ma-", areaID, "-pv-", pvID, ".png")
      ggsave(filename = fileName,
             plot = p1, width = w, height = h, units = "px", dpi = 300,
             scale = 2)
    }
  }
}

# Get list of managed areas to create plots for
all_managed_areas <- unique(managed_area_df$ManagedAreaName)
# Get list of managed areas with continuous data
cont_managed_areas <- skt_stats_cont[!is.na(ProgramID), unique(ManagedAreaName)]
# Save plots as .pngs
if(save_plots){
  # Loop through list of managed areas
  for(ma in all_managed_areas){
    print(ma)
    # determine which analyses to run for each MA
    # variables will be input into RMD file
    ma_df <- managed_area_df %>% filter(ManagedAreaName == ma)
    p_inc <- unique(ma_df$Parameter)
    d_inc <- unique(ma_df$Depth)
    a_inc <- unique(ma_df$Activity)
    
    discrete_data <- data_output_disc[ManagedAreaName==ma, ]
    skt_data <- skt_stats_disc[ManagedAreaName==ma, ]
    
    # Shortened names for managed areas
    ma_short <- MA_All[ManagedAreaName==ma, Abbreviation]
    # record region name
    region <- MA_All[ManagedAreaName==ma, Region]
    # create plots
    for(indicator in unique(websiteParams$IndicatorName)){
      cat(glue("# {indicator} \n"))
      
      # Filter once for the current indicator
      indicator_subset <- websiteParams[IndicatorName == indicator, ]
      # loop through all indicator/parameter combinations, producing plots
      for(i in 1:nrow(indicator_subset)){
        filteredSubset <- indicator_subset[i]
        parameter <- filteredSubset$ParameterName
        param_short <- filteredSubset$ParameterShort
        unit <- filteredSubset$ParameterUnits
        type <- filteredSubset$SamplingFrequency
        activity <- filteredSubset$ActivityType
        depth <- filteredSubset$RelativeDepth
        
        # Parameter names to lowercase, except for "pH" and "Secchi depth"
        if(parameter %in% c("pH", "Secchi Depth")){
          parameter_lower <- ifelse(parameter=="pH", parameter, "Secchi depth")
        } else {
          parameter_lower <- str_to_lower(parameter)
        }
        
        # Define y-label, activity-label, depth-label for plot labels
        y_labels <- ifelse(parameter == "pH", paste0("Monthly average ", parameter_lower), paste0("Monthly average ", parameter_lower, " (" , unit, ")"))
        activity_label <- ifelse(activity=="All", "Lab and Field Combined", activity)
        depth_label <- ifelse(depth=="All", "All Depths", "Surface")
        
        if(type=="Continuous"){
          plot_trendlines_cont_combined(ma = ma, cont_plot_data = cont_plot_data,
                                        param = param_short, y_labels = y_labels,
                                        parameter = parameter)
        }
        
        if(type=="Discrete"){
          plot_trendlines(param_short, activity, depth, activity_label,
                          depth_label, y_labels, parameter, skt_data,
                          discrete_data)
        }
      }
    }
  }  
}

# Get list of available plot files created by WC_Plot_Render.R
cont_plots <- list.files("output/WQ_Continuous/", full.names = T)
disc_plots <- list.files("output/WQ_Discrete/", full.names = T)

# Render reports if `render_reports` is TRUE
if(render_reports){
  # Loop through list of managed areas
  for(ma in all_managed_areas){
    print(ma)
    # determine which analyses to run for each MA
    # variables will be input into RMD file
    ma_df <- managed_area_df %>% filter(ManagedAreaName == ma)
    p_inc <- unique(ma_df$Parameter)
    d_inc <- unique(ma_df$Depth)
    a_inc <- unique(ma_df$Activity)
    
    discrete_data <- data_output_disc[ManagedAreaName==ma, ]
    skt_data <- skt_stats_disc[ManagedAreaName==ma, ]
    
    # Shortened names for managed areas
    ma_short <- MA_All[ManagedAreaName==ma, Abbreviation]
    # if(!ma_short %in% c("EBAP")) next
    # record region name
    region <- MA_All[ManagedAreaName==ma, Region]
    # output path for managed area reports
    output_path <- "output/Reports/"
    file_out <- paste0(ma_short,"_WC_Report")
    ### RENDERING ###
    rmarkdown::render(input = "WC_ReportTemplate.Rmd",
                      output_format = "pdf_document",
                      output_file = paste0(file_out, ".pdf"),
                      output_dir = output_path,
                      clean=TRUE)
    unlink(paste0(output_path, file_out, ".md"))
    unlink(paste0(output_path, file_out, ".tex"))
    unlink(paste0(output_path, file_out, "_files"), recursive=TRUE)
    unlink(paste0(file_out, ".log"))
  }
}

# Zip all files into Discrete and Continuous .zips in "output" folder
for(type in c("Discrete", "Continuous")){
  # Get list of all available plots
  fig_list <- list.files(paste0("output/WQ_",type), pattern = ".png", full=TRUE)
  zip(zipfile = paste0("output/WQ_",type), 
      files = fig_list)
}

# Create maps if needed
if(save_maps){
  source("WQ_Create_Maps.R")
}
