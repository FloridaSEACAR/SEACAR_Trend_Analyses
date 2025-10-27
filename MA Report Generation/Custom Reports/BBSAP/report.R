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

# Render report?
# FALSE will generate objects necessary for dashboard, TRUE creates .PDF output as well 
render_report <- TRUE

# Set working directory
wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

# Creates folders for outputs
folder_paths <- c("output", "output/tables","output/maps", 
                  "output/Figures", "output/rds", "output/rds/plots", "output/rds/maps/")
for(path in folder_paths){if(!dir.exists(path)){dir.create(path)}}

# Data Prep ----
source("../../../SEACAR_data_location.R")

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

# Combine all WQ parameters into a single data frame
wq_data_combined <- bind_rows(data_directory[["data"]])

# Load in BBSAP polygon with System and Type associations
bbsap_polygon <- sf::st_read("shapefiles/bbsap_systems.shp", crs = 4326)

### Convert wq_data_combined into sf spatial data frame with geometries
### Then cross-reference with bbsap_polygon to classify by System & Type
wq_data_combined_sf <- wq_data_combined %>% 
  st_as_sf(coords = c("OriginalLongitude","OriginalLatitude"), crs = 4326) %>%
  st_join(bbsap_polygon) %>% 
  filter(!is.na(System))
# Make a copy for later use
wq_data <- copy(wq_data_combined_sf)

# Merge SAV data with System and Type designations
sav_data <- sav[ManagedAreaName==ma, ] %>% 
  st_as_sf(coords = c("OriginalLongitude","OriginalLatitude"), crs = 4326) %>%
  st_join(bbsap_polygon) %>% 
  filter(!is.na(System))

# Keep "St. Marks" designation for SAV instead of splitting into Aucilla & St. Marks
sav_data$System[sav_data$System=="Aucilla"] <- "St. Marks"

##### Create mapping-related dataframes, remove sf functionality for leaflet mapping
# Map for wq data
map_df <- wq_data %>% 
  select(ProgramID, ProgramLocationID, ProgramName, System, Type, ResultValue,
         ParameterName, SampleDate)
# convert from sf to regular data table, create popup text for maps
map_df <- map_df %>%
  bind_cols(as_tibble(st_coordinates(map_df)) %>%
              setNames(c("OriginalLongitude", "OriginalLatitude"))) %>% 
  st_drop_geometry() %>% 
  mutate(popup = paste("ProgramID: ", ProgramID,
                       "<br> ProgramName: ", ProgramName,
                       "<br> ProgLocID: ", ProgramLocationID))
# Create label for maps
map_df <- map_df %>% group_by(ProgramLocationID, ParameterName, System, Type, 
                              OriginalLatitude, OriginalLongitude) %>%
  reframe(popup = unique(popup),
          Mean = round(mean(ResultValue),2)) %>% 
  mutate(label = paste0("ProgLocID: ", ProgramLocationID))
setDT(map_df)

# Create map for SAV data
# Convert from sf to regular data table
sav_map_df <- sav_data %>%
  bind_cols(as_tibble(st_coordinates(sav_data)) %>%
              setNames(c("OriginalLongitude", "OriginalLatitude"))) %>%
  st_drop_geometry() %>%
  mutate(params = paste(unique(ParameterName), collapse=", ")) %>%
  select(ProgramID, ProgramLocationID, ProgramName, System, ResultValue, 
         params, SampleDate, OriginalLatitude, OriginalLongitude) %>%
  mutate(popup = paste("ProgramID: ", ProgramID,
                       "<br> ProgramName: ", ProgramName,
                       "<br> ProgLocID: ", ProgramLocationID,
                       "<br> Parameters: ", params))
sav_map_df <- sav_map_df %>% group_by(ProgramID, ProgramLocationID, System) %>%
  reframe(OriginalLatitude = unique(OriginalLatitude),
          OriginalLongitude = unique(OriginalLongitude),
          popup = unique(popup)) %>% 
  mutate(label = paste0("ProgLocID: ", ProgramLocationID))
setDT(sav_map_df)

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
skt_data_combined$EarliestSampleDate <- as.POSIXct(skt_data_combined$EarliestSampleDate)
skt_data_combined$LastSampleDate <- as.POSIXct(skt_data_combined$LastSampleDate)

fwrite(skt_data_combined, "output/tables/Discrete_WQ_SKT_Stats.txt", sep="|")

skt_data_combined$SennSlope <- round(skt_data_combined$SennSlope, 3)
skt_data_combined$SennIntercept <- round(skt_data_combined$SennIntercept, 2)
skt_data_combined$ChiSquared <- round(skt_data_combined$ChiSquared, 2)

# KT Plot info
skt_data_combined <- skt_data_combined %>%
  mutate(start_x = decimal_date(EarliestSampleDate),
         end_x = decimal_date(LastSampleDate),
         start_y = (start_x - EarliestYear) * SennSlope + SennIntercept,
         end_y = (end_x - EarliestYear) * SennSlope + SennIntercept) %>%
  select(-ActivityType) # Remove activity type to enable proper merging in next step

# Combine skt_stats and YM stats
YM_Stats_combined <- bind_rows(data_directory[["YM_Stats2"]])
data_combined <- merge(YM_Stats_combined %>% select(-Median), skt_data_combined %>% select(-c(N_Data)), 
                       by = c("System", "Type", "ParameterName", "RelativeDepth"))
setDT(data_combined)
data_combined[, `:=` (sig = ifelse(p<=0.05, "Significant Trend", "Non-significant Trend"))]

# Allows text coloring within report
colorize <- function(x, color) {sprintf("\\textcolor{%s}{%s}", color, x)}

# Include program information for each system
prog_data <- wq_data %>% st_drop_geometry() %>% as.data.frame() %>%
  group_by(ParameterName, ProgramID, ProgramName, Type, System) %>%
  reframe(n_data = n()) %>%
  pivot_wider(names_from = Type, values_from = n_data, names_prefix = "n-data-")
setDT(prog_data)

# Determine which programs to include citations for
# Combine unique SAV programs + WQ programs
all_prog_ids <- unique(c(unique(sav_data$ProgramID), unique(prog_data$ProgramID)))
# nocite_refs string will be imputed into YAML header
# This ensures only programs in this report will be displayed in references
# "SEACAR DDI citations.bib" is BibTeX-format export from Zotero SEACAR library
nocite_refs <- paste0("@SEACARID", all_prog_ids, collapse = ", ")

# Render report ----
if(render_report){
  file_name <- paste0("BBSAP_report_", gsub("-","",Sys.Date()))
  rmarkdown::render(input="ReportTemplate.Rmd",
                    output_format = "pdf_document",
                    output_file = paste0(file_name,".pdf"),
                    output_dir = "output",
                    clean = TRUE)
  # Remove unwanted files
  unlink(paste0("output/",file_name,".md"))
  unlink(paste0("output/",file_name,".tex"))
  unlink(paste0("output/",file_name,"_files"))
  # Create copy without date for linking on GitHub pages website
  file.copy(from = paste0("output/",file_name, ".pdf"), to = "output/BBSAP_report.pdf", overwrite = T)  
}

## DASHBOARD IMPLEMENTATION ----
#SAVE MAP_DF RDS
params <- names(data_directory[["YM_Stats"]])

sysPal <- colorFactor(SEACAR::seacar_palette2, unique(map_df$System))
paramPal <- colorFactor(SEACAR::seacar_palette2, params)

groupNames <- c()
for(sys in unique(map_df$System)){
  # Blank map for each system to fill with parameter information
  map <- leaflet() %>% addTiles()
  for(param in params){
    # Filter data for a given parameter and system
    filtered_data <- map_df[System==sys & ParameterName==param, ] %>% 
      distinct(OriginalLatitude, OriginalLongitude, ParameterName, ProgramLocationID)
    # Record group names
    groupNames <- c(groupNames, param)
    # Add circle markers to map
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

# map <- leaflet(map_df) %>% 
#   addProviderTiles(providers$CartoDB.PositronNoLabels,
#                    group = "Positron by CartoDB") %>%
#   addCircleMarkers(lat = ~OriginalLatitude, lng = ~OriginalLongitude,
#                    weight = 0.5, fillOpacity = 0.4, opacity = 0.4, color = "black",
#                    fillColor = ~sysPal(System))
# 
# map <- map %>%
#   addLayersControl(baseGroups = c("Positron by CartoDB"),
#                    overlayGroups = groupNames,
#                    options = layersControlOptions(collapsed=TRUE))

##### Export .rds objects for Dashboard use
YM_Stats <- bind_rows(data_directory[["YM_Stats"]])
skt_stats <- bind_rows(data_directory[["skt_stats"]])
sav_stats <- fread("output/tables/SAV_BBpct_LMEresults_All.txt") # stats file from sav_analysis.R
publish_date <- Sys.Date()
# List of .rds objects to export
rds_to_save <- c("wq_data", "sav_data", "YM_Stats", "skt_stats", "data_combined",
                 "sys_include", "publish_date", "map_df", "sav_map_df", "skt_data_combined", 
                 "sav_stats", "groupNames")
#### RECREATE SAV_MAP_DF
for(file in rds_to_save){
  saveRDS(get(file), file = paste0("BBSAP_dashboard/rds/", file, ".rds"))
}
