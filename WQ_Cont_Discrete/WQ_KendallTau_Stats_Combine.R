# The purpose of this script is to combine the individual SKT results
# into a combined .txt file for use in SEACAR Atlas website pages
# Processes both discrete and continuous
options(scipen = 999)
library(data.table)
library(dplyr)

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
  website <- fread("data/WebsiteParameters.csv",  sep=",", header=TRUE,
                   stringsAsFactors=FALSE, na.strings=c("NULL","","NA"))
  
  # Select discrete/cont parameters only
  website <- website[SamplingFrequency==file_type, ]
  
  if(file_type=="Continuous"){
    website <- website[ , c("ParameterName","Website","SamplingFrequency")]
  }
  
  data <- merge.data.frame(data, website, by=columns, all=TRUE)
  data$Website[is.na(data$Website)] <- 0
  
  if(file_type=="Discrete"){
    data <- data %>% 
      select(-c("SamplingFrequency", "IndicatorName", "ParameterShort", "ParameterUnits", "IndicatorShort", "ParameterVisId")) %>%
      select(AreaID, ManagedAreaName, everything())
    
    data <- as.data.table(data[order(data$ManagedAreaName, data$ParameterName,
                                     data$RelativeDepth, data$ActivityType), ])
  } else if(file_type=="Continuous"){
    data <- data %>% 
      select(-c("SamplingFrequency")) %>%
      select(AreaID, ManagedAreaName, ProgramID, ProgramName, ProgramLocationID,
             everything())
    data <- as.data.table(data[order(data$ManagedAreaName, data$ProgramID,
                                     data$ProgramName, data$ProgramLocationID,
                                     data$ParameterName), ])
  }
  
  # Remove leading spaces from NA P-values
  data <- data[str_detect(data$p, "NA"), `:=` (p=NA)]
  data$p <- as.numeric(data$p)
  data <- data %>%
    mutate_if(is.numeric, round, 5)
  
  output_path <- paste0("output/WQ_", file_type, "_All_KendallTau_Stats")
  
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

params <- c("DO","DOS","pH","Sal","TempW","Turb")
regions <- c("NE","NW","SE","SW")

YM_combined <- data.table()
skt_combined <- data.table()

for(p in params){
  for(r in regions){
    Mon_YM_Stats <- as.data.frame(load_cont_data_table(p, r, "Mon_YM_Stats"))
    skt_stats <- as.data.frame(load_cont_data_table(p, r, "skt_stats"))
    
    YM_combined <- bind_rows(YM_combined, Mon_YM_Stats)
    skt_combined <- bind_rows(skt_combined, skt_stats)
  }
}
# output path should be location of wq_continuous dashboard /data/ folder
out_path <- "../../SEACAR-Dashboards/Continuous WQ/data/"
saveRDS(YM_combined, file = paste0(out_path, "YM_combined.rds"))
saveRDS(skt_combined, file = paste0(out_path, "skt_combined.rds"))
