library(stringr)
library(dplyr)
library(data.table)
library(ggplot2)
library(tictoc)
library(rstudioapi)

# Change folder date to select which objects to load (if plots need to match Atlas)
# "most recent" to use the latest MA Report Generation outputs
disc_folder_date <- "most recent"
cont_folder_date <- "most recent"

# Point to location where Disc objects are located
data_obj_loc <- "C:/Users/Hill_T/Desktop/SEACAR GitHub/SEACAR_Trend_Analyses/MA Report Generation/output/tables/"

disc_loc <- ifelse(disc_folder_date=="most recent", 
                   paste0(data_obj_loc,"disc/"), 
                   paste0(data_obj_loc,"disc/",disc_folder_date,"/"))
cont_loc <- ifelse(cont_folder_date=="most recent",
                   paste0(data_obj_loc,"cont/"),
                   paste0(data_obj_loc,"cont/",cont_folder_date,"/"))

# Lists of disc and cont .rds objects to read
disc_files <- list.files(disc_loc,pattern = "\\.rds$", full.names = T)
cont_files <- list.files(cont_loc,pattern = "\\.rds$", full.names = T)

# Gets directory of this script and sets it as the working directory
wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

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

skt_stats_disc <- skt_stats_disc %>% 
  mutate("Period of Record" = paste0(EarliestYear, " - ", LatestYear),
         "Statistical Trend" = ifelse(p <= 0.05 & SennSlope > 0, "Significantly increasing trend",
                                      ifelse(p <= 0.05 & SennSlope < 0, "Significantly decreasing trend", 
                                             ifelse(SufficientData==FALSE, "Insufficient data to calculate trend",
                                                    ifelse(SufficientData==TRUE & is.na(SennSlope), "Model did not fit the available data", 
                                                           ifelse(is.na(Trend), "Insufficient data to calculate trend","No significant trend"))))))
skt_stats_disc[is.na(Trend), `:=` ("Statistical Trend" = "Insufficient data to calculate trend")]
skt_stats_disc[str_detect("NA", p), `:=` ("p" = NA)]

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
  # Save into data_output_cont directory
  data_output_cont[[param]] <- data
  print(paste0(param," processing complete"))
}

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

# Get list of managed areas to create reports for
all_managed_areas <- unique(managed_area_df$ManagedAreaName)
# all_managed_areas <- "Estero Bay Aquatic Preserve"
# all_managed_areas <- "Pellicer Creek Aquatic Preserve"
# all_managed_areas <- "Big Bend Seagrasses Aquatic Preserve"

all_managed_areas <- all_managed_areas[
  !all_managed_areas %in% c("Florida Keys National Marine Sanctuary",
                            "Nassau River-St. Johns River Marshes Aquatic Preserve",
                            "Rookery Bay National Estuarine Research Reserve")]

cont_managed_areas <- skt_stats_cont[!is.na(ProgramID), unique(ManagedAreaName)]

tic()
# Loop through list of managed areas
for (ma in all_managed_areas) {
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
  unlink(paste0(output_path, file_out, "_files"), recursive=TRUE)
}
toc()