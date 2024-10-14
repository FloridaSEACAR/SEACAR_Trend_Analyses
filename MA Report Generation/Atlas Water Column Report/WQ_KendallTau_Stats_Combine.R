# The purpose of this script is to combine the individual SKT results
# into a combined .txt file for use in SEACAR Atlas website pages
# Processes both discrete and continuous

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
  website <- website[type==file_type, ]
  
  if(file_type=="Continuous"){
    website <- website[ , c("ParameterName","Website","type")]
  }
  
  data <- merge.data.frame(data, website, by=columns, all=TRUE)
  data$Website[is.na(data$Website)] <- 0
  
  if(file_type=="Discrete"){
    data <- data %>% 
      select(-c("type")) %>%
      select(AreaID, ManagedAreaName, everything())
    
    data <- as.data.table(data[order(data$ManagedAreaName, data$ParameterName,
                                     data$RelativeDepth, data$ActivityType), ])
  } else if(file_type=="Continuous"){
    data <- data %>% 
      select(-c("type")) %>%
      select(AreaID, ManagedAreaName, ProgramID, ProgramName, ProgramLocationID,
             everything())
    data <- as.data.table(data[order(data$ManagedAreaName, data$ProgramID,
                                     data$ProgramName, data$ProgramLocationID,
                                     data$ParameterName), ])
  }
  
  # Remove leading spaces from NA P-values
  data <- data[p %in% c("    NA","NA"), `:=` (p="NA")]
  
  output_path <- paste0("output/Data/WQ_", file_type, "_All_KendallTau_Stats")
  
  fwrite(data[!N_Data==0, ], paste0(output_path, ".txt"), sep="|")
  fwrite(data[!N_Data==0, ], paste0(output_path, ".csv"), sep=",")
  
}