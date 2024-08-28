# The purpose of this script is to generate a customized report for
# Big Bend Seagrasses Aquatic Preserve and separate analyses by system
# Developed by Tyler G Hill (Florida DEP)

library(rstudioapi)
library(data.table)
library(stringr)
library(glue)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(sf)
library(leaflet)
library(knitr)
library(rmarkdown)
library(lubridate)
library(mapview)
library(htmltools)
library(htmlwidgets)
library(webshot)
library(cowplot)
library(kableExtra)

# Set working directory
wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

# Creates folders for outputs
folder_paths <- c("output", "output/tables","output/maps", 
                  "output/Figures")
for(path in folder_paths){if(!dir.exists(path)){dir.create(path)}}

# Data Prep ----
source("seacar_data_location.R")

# Read in files, separate into WQ and SAV
files <- list.files(seacar_data_location, full.names = TRUE)
wq_files <- str_subset(files, "Combined_WQ_WC_NUT_")
wq_files <- str_subset(wq_files, "_cont_", negate=TRUE)
sav_file <- str_subset(files, "All_SAV")

# Read in data frame
sav <- fread(sav_file, sep='|', na.strings = "NULL")

# Create a directory to store data specific to BBSAP
data_directory <- list()

ma <- "Big Bend Seagrasses Aquatic Preserve"

for(file in wq_files){
  # Read in data file, subset for Include, MADup and BBSAP
  file_short <- tail(str_split(file, "/")[[1]],1)
  print(glue("Reading in {file_short}"))
  data <- fread(file, sep='|', na.strings = "NULL")
  data <- data[Include==1 & MADup==1 & ManagedAreaName==ma, ] %>% select(-ExportVersion)
  
  # If there is data for BBSAP, append to directory
  if(nrow(data)>0){
    # Grab parameter name from data file
    param <- unique(data$ParameterName)
    
    # Record unique ProgramID, ProgramLocationID combinations
    ids <- data %>%
      group_by(ProgramID, ProgramLocationID) %>%
      summarise(n = n(), .groups = "keep")
    ids$parameter <- param
    
    # Store results into directory
    data_directory[["id_overview"]][[param]] <- ids
    data_directory[["data"]][[param]] <- data
    rm(data, ids)
  }
}

# Combine the WQ results into one dataframe, grab unique pID and pLID
wq_ids <- distinct(bind_rows(data_directory[["id_overview"]]))
wq_ploc <- unique(wq_ids$ProgramLocationID)
wq_pid <- unique(wq_ids$ProgramID)

# Combine unique ProgramIDs from WQ and SAV
bb_pid <- c(wq_pid, unique(sav[ManagedAreaName==ma, ]$ProgramID))
# Combine unique ProgramLocationIDs from WQ and SAV
bb_ploc <- c(wq_ploc, unique(sav[ManagedAreaName==ma, ]$ProgramLocationID))

# BBSAP_system_shapefile.R contains the code for modifying the initial
# sample locations shapefile to produce the modified shapefile which contains
# groupings by system ("bbsap_systems.shp")
# uses "bb_shape_location" variable loaded by seacar_data_location.R

# "bb_shape_location" has St. Marks-Aucilla-Econfina grouped together
# bb_points <- st_read(bb_shape_location)

# "bb_shape_location_rivers" separates St. Marks-Aucilla-Econfina, 
# Also adds River vs. Estuary factor ## USE THIS
bb_points <- st_read(bb_shape_location_rivers)

# Combine all WQ parameters into a single data frame
wq_data_combined <- bind_rows(data_directory[["data"]])

# Merge WQ data with sample location info, selecting appropriate columns
# Allows subsetting by system and provides coordinates
wq_data <- merge(x = wq_data_combined,
                 y = bb_points[ , c("LocationID", "ProgramID", "ProgramLoc", 
                                    "Latitude_D", "Longitude_", "System", "Type", 
                                    "geometry")],
                 by.x = c("ProgramID", "ProgramLocationID"), 
                 by.y = c("ProgramID", "ProgramLoc"))

# Merge SAV data with sample location info
sav_data <- merge(x = sav[ManagedAreaName==ma, ],
                  y = bb_points[ , c("LocationID","ProgramID","ProgramLoc",
                                     "Latitude_D","Longitude_","System",
                                     "geometry")],
                  by.x = c("LocationID", "ProgramID", "ProgramLocationID"),
                  by.y = c("LocationID", "ProgramID", "ProgramLoc"))
# Keep "St. Marks" designation for SAV instead of splitting into Aucilla & St. Marks
sav_data[System=="Aucilla", `:=` (System = "St. Marks")]

# Analysis ----
# Import separate discrete script
# Adds to additional analyses to data_directory
# source("analysis.R") # Analyze each system
source("analysis_by_type.R") # Each sys AND type combination (Estuary,River)
# SAV analysis
source("sav_analysis.R")
# SAV GAM plots
source("system_gam.R")

# Combine all SKT results into a single data frame
skt_data_combined <- bind_rows(data_directory[["skt_stats"]])
# Allowing p-values to be perceived as true NA where applicable
skt_data_combined$p[skt_data_combined$p %in% c("    NA","NA")] <- NA
skt_data_combined$p[skt_data_combined$p==" 0"] <- 0

fwrite(skt_data_combined, "output/tables/Discrete_WQ_SKT_Stats.txt", sep="|")

skt_data_combined$SennSlope <- round(skt_data_combined$SennSlope, 3)
skt_data_combined$SennIntercept <- round(skt_data_combined$SennIntercept, 2)
skt_data_combined$ChiSquared <- round(skt_data_combined$ChiSquared, 2)

# KT Plot info
skt_data_combined <- skt_data_combined %>%
  mutate(start_x = decimal_date(EarliestSampleDate),
         end_x = decimal_date(LastSampleDate),
         start_y = (start_x - EarliestYear) * SennSlope + SennIntercept,
         end_y = (end_x - EarliestYear) * SennSlope + SennIntercept)

# Combine skt_stats and YM stats
YM_Stats_combined <- bind_rows(data_directory[["YM_Stats2"]])
data_combined <- merge(YM_Stats_combined, skt_data_combined %>% select(-c(N_Data, Median)), 
                       by = c("System", "Type", "ParameterName", "RelativeDepth", "ActivityType"))
setDT(data_combined)

# Allows text coloring within report
colorize <- function(x, color) {sprintf("\\textcolor{%s}{%s}", color, x)}

# Include program information for each system
prog_data <- wq_data %>%
  group_by(ParameterName, ProgramID, Type, System) %>%
  summarise(n_data = n()) %>%
  pivot_wider(names_from = Type, values_from = n_data, names_prefix = "n-data-")
setDT(prog_data)

# Render report ----
file_name <- paste0("BBSAP_report_by_system_", gsub("-","",Sys.Date()))
rmarkdown::render(input="ReportTemplate.Rmd",
                  output_format = "pdf_document",
                  output_file = paste0(file_name,".pdf"),
                  output_dir = "output",
                  clean = TRUE)
# Remove unwanted files
# unlink(paste0("output/",file_name,".md"))
# unlink(paste0("output/",file_name,".tex"))
# unlink(paste0("output/",file_name,"_files"))

## DASHBOARD IMPLEMENTATION ----
map_df <- wq_data %>% 
  select(ProgramID, ProgramLocationID, ProgramName, System, Type, ResultValue,
         ParameterName, SampleDate, OriginalLatitude, OriginalLongitude)

#SAVE MAP_DF RDS
params <- names(data_directory[["YM_Stats"]])

sysPal <- colorFactor(seacar_palette, unique(map_df$System))
paramPal <- colorFactor(seacar_palette, params)

groupNames <- c()
for(sys in unique(map_df$System)){
  
  # Blank map for each system to fill with parameter information
  map <- leaflet() %>% addTiles()
  
  for(param in params){
    
    filtered_data <- map_df[System==sys & ParameterName==param, ] %>% 
      distinct(OriginalLatitude, OriginalLongitude)
    
    groupNames <- c(groupNames, param)
    
    map <- map %>%
      addCircleMarkers(data = filtered_data,
                       lat = ~OriginalLatitude, lng = ~OriginalLongitude,
                       weight = 0.5, fillOpacity = 0.4, opacity = 0.4, color="black",
                       fillColor = ~paramPal(ParameterName), group = param) %>%
      addLayersControl(overlayGroups = groupNames,
                       options = layersControlOptions(collapsed=TRUE))
  }
  
  # Save map
  saveRDS(map, file = paste0("output/rds/maps/",sys,"_map.rds"))
}


map <- leaflet(map_df) %>% 
  addProviderTiles(providers$CartoDB.PositronNoLabels,
                   group = "Positron by CartoDB") %>%
  addCircleMarkers(lat = ~OriginalLatitude, lng = ~OriginalLongitude,
                   weight = 0.5, fillOpacity = 0.4, opacity = 0.4, color = "black",
                   fillColor = ~sysPal(System))

map <- map %>%
  addLayersControl(baseGroups = c("Positron by CartoDB"),
                   overlayGroups = groupNames,
                   options = layersControlOptions(collapsed=TRUE))

# Export .rds objects for Dashboard use
saveRDS(wq_data, file = "output/rds/wq_data.rds")
saveRDS(sav_data, file = "output/rds/sav_data.rds")
saveRDS(bind_rows(data_directory[["YM_Stats"]]), file="output/rds/YM_Stats.rds")
saveRDS(bind_rows(data_directory[["skt_stats"]]), file="output/rds/skt_stats.rds")