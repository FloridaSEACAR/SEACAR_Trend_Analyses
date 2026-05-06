# The purpose of this script is to combine the individual SKT results
# into a combined .txt file for use in SEACAR Atlas website pages
# Processes both discrete and continuous
options(scipen = 999)
library(data.table)
library(dplyr)
library(stringr)

# Function to apply TrendIcon and TrendText for display on the Atlas
apply_trend_icon <- function(suffData, trend){
  trendPresent <- trend %in% c(-1, 1, 2, -2)
  if(suffData){
    if(trendPresent){
      TrendIcon <- ifelse(trend>0, 1, -1)
      TrendText <- ifelse(trend>0, "Increasing trend", "Decreasing trend")
    } else {
      TrendIcon <- 0
      TrendText <- "No detectable trend"
    }
  } else {
    TrendIcon <- 2
    TrendText <- "Insufficient data"
  }
  return(list("TrendIcon" = TrendIcon, 
              "TrendText" = TrendText))
}

# Find discrete KendallTau outputs
discrete_files <- list.files("output/tables/disc", pattern = "\\KendallTau_Stats.rds$", full.names = TRUE)
# Find continuous KendallTau outputs
continuous_files <- list.files("output/tables/cont", pattern = "\\KendallTau_Stats.rds$", full.names = TRUE)

for(file_type in c("Discrete", "Continuous")){
  if(file_type=="Discrete"){
    # Load each RDS file
    df <- lapply(discrete_files, readRDS)
    # Columns to merge by
    columns <- c("ParameterName","RelativeDepth","ActivityType")
  } else if(file_type=="Continuous"){
    # Load each RDS file
    df <- lapply(continuous_files, readRDS)
    # Columns to merge by
    columns <- c("ParameterName")
  }
  
  # Bind them together
  data <- do.call(rbind, df)
  
  # Set median, latestyear, earliestyear as NA where necessary
  data$Median[data$EarliestYear=="Inf"] <- NA
  data$LatestYear[data$EarliestYear=="Inf"] <- NA
  data$EarliestYear[data$EarliestYear=="Inf"] <- NA
  
  # Load in WebsiteParameters.csv
  website <- SEACAR::WebsiteParameters
  
  # Select discrete/cont parameters only
  website <- website[SamplingFrequency==file_type, ]
  
  if(file_type=="Continuous"){
    website <- website[ , c("ParameterName","Website","SamplingFrequency")]
  }
  
  data <- merge.data.frame(data, website, by=columns, all=TRUE)
  data <- data %>% filter(!is.na(AreaID))
  data$Website[is.na(data$Website)] <- 0
  
  if(file_type=="Discrete"){
    data <- data %>% 
      select(-c("SamplingFrequency", "IndicatorName", "ParameterShort", "ParameterUnits", "IndicatorShort", "ParameterVisId",
                "HabitatName", "HabitatShort")) %>%
      select(AreaID, ManagedAreaName, everything()) %>%
      arrange(ManagedAreaName, ParameterName, RelativeDepth, ActivityType) %>%
      as.data.table()
  } else if(file_type=="Continuous"){
    data <- data %>% 
      select(-c("SamplingFrequency", "MonitoringID")) %>%
      select(AreaID, ManagedAreaName, ProgramID, ProgramName, ProgramLocationID,
             everything()) %>%
      as.data.table()
    data$RelativeDepth <- stringr::str_to_title(data$RelativeDepth)
  }
  
  # Remove leading spaces from NA P-values
  data <- data[str_detect(data$p, "NA"), `:=` (p=NA)]
  data$p <- as.numeric(data$p)
  data <- data %>%
    mutate_if(is.numeric, round, 5)
  
  output_path <- paste0("output/WQ_", file_type, "_All_KendallTau_Stats")
  
  # De-concatenate continuous results
  if(file_type=="Continuous"){
    data <- setDT(SEACAR::clean_managed_areas(data, "ma")) %>% 
      arrange(ManagedAreaName, ProgramID, ProgramName, ProgramLocationID, ParameterName)
    data$ProgramLocationID <- as.character(data$ProgramLocationID)
  }
  
  #### Add icon-based trends + trend text columns
  data <- data %>% rowwise() %>%
    mutate(TrendIcon = apply_trend_icon(SufficientData, Trend)$TrendIcon,
           TrendText = apply_trend_icon(SufficientData, Trend)$TrendText) %>%
    as.data.table()
  
  fwrite(data[!N_Data==0, ], paste0(output_path, ".txt"), sep="|")
  fwrite(data[!N_Data==0, ], paste0(output_path, ".csv"), sep=",")
  
}

## This script exports the necessary files for continuous_wq dashboard
## Grabs the continuous outputs (SKT results, YM data [condensed])
## Combines them into individual files to allow for quicker loading with .RDS
cont_rds_loc <- "output/tables/cont/"

# For loading continuous data
# Load Data Table Function
load_cont_data_table <- function(param, region, table) {
  
  # Declaring RDS file list of respective tables
  files <- list.files(cont_rds_loc,pattern = "\\.rds$")
  file_path <- paste0("_",param,"_", region,"_", table) 
  
  # subset file list to select desired table RDS file
  table_file <- paste0(cont_rds_loc,str_subset(files, file_path))
  
  # importing RDS files
  df <- readRDS(table_file)
  
  return(df)
}

files <- list.files(cont_rds_loc,pattern = "\\.rds$", full=T)
skt_files <- str_subset(files, "skt_stats")
ym_files <- str_subset(files, "Mon_YM_Stats")

read_combine <- function(x){
  df <- readRDS(x)
  df$AreaID <- as.character(df$AreaID)
  SEACAR::clean_managed_areas(df, "ma")
}

YM_combined <- lapply(ym_files, read_combine) %>% bind_rows()
skt_combined <- lapply(skt_files, read_combine) %>% bind_rows()

# output path should be location of wq_continuous dashboard /data/ folder
out_path <- "../../SEACAR-Dashboards/Continuous WQ/data/"
saveRDS(YM_combined, file = paste0(out_path, "YM_combined.rds"))
saveRDS(skt_combined, file = paste0(out_path, "skt_combined.rds"))
