#Load libraries
library(knitr)
library(readr)
library(tidyverse)
library(data.table)
library(purrr)
library(rstudioapi)
library(stringr)
library(utils)
library(geosphere)
library(leaflet)
library(leaflegend)
library(mapview)
library(magick)
library(mgcv)
library(cowplot)
library(webshot)
library(sf)
library(fontawesome)
library(gridExtra)
library(ggpubr)
library(glue)
library(kableExtra)
library(distill)
library(dplyr)
library(RColorBrewer)
library(tictoc)
library(SEACAR)

## Render Atlas Reports? - provide overview in similar format to Atlas - helpful for review
render_atlas_reports <- FALSE

# Gets directory of this script and sets it as the working directory
wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

# Create folder paths if they don't yet exist
folder_paths <- c("output", "output/Reports", "output/maps", "output/Reports/HTML", 
                  "output/Reports/PDF", "output/Reports/AtlasReports")
for(path in folder_paths){if(!dir.exists(path)){dir.create(path)}}

#Set output directory
out_dir <- "output"
report_out_dir <- "output/Reports"

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- SEACAR::ManagedAreas

#Load WebsiteParameters spreadsheet to grab ParameterVisualizationID
websiteParams <- SEACAR::WebsiteParameters
websiteParams <- websiteParams %>% 
  arrange(factor(IndicatorName, levels = c("Nutrients","Water Quality","Water Clarity")),
          factor(ParameterName, levels = c("Total Nitrogen","Total Phosphorus",
                                           "Dissolved Oxygen", "Dissolved Oxygen Saturation", "Salinity", "Water Temperature", "pH",
                                           "Turbidity", "Total Suspended Solids", "Chlorophyll a, Uncorrected for Pheophytin",
                                           "Chlorophyll a, Corrected for Pheophytin", "Secchi Depth", "Colored Dissolved Organic Matter"))) %>%
  filter(Website==1) %>% as.data.table()

#Gets the desired file locations
#Imports SEACAR data file path information as variable "seacar_data_location"
source("../SEACAR_data_location.R")

files <- list.files(seacar_data_location, full.names=TRUE)
hab_files <- str_subset(files, "All_")

cw_file_in <- str_subset(hab_files, "CW")
cw_file_short <- tail(str_split(cw_file_in, "/")[[1]],1)

coral_file_in <- str_subset(hab_files, "CORAL")
coral_file_short <- tail(str_split(coral_file_in, "/")[[1]],1)

nekton_file_in <- str_subset(hab_files, "NEKTON")
nekton_file_short <- tail(str_split(nekton_file_in, "/")[[1]],1)

sav_file_in <- str_subset(hab_files, "SAV")
sav_file_short <- tail(str_split(sav_file_in, "/")[[1]],1)

oyster_file_in <- str_subset(hab_files, "OYSTER")
oyster_file_short <- tail(str_split(oyster_file_in, "/")[[1]],1)

cont_files <- str_subset(files, "_NUT_cont")

############################
### call in source files ### -----
############################
# All habitats must have their analyses run individually within SEACAR_Trend_Analyses
# prior to running this script. These scripts provide the functions for plotting those results / plots
source("scripts/WQ_Continuous.R")
source("scripts/WQ_Discrete.R")
source("scripts/Nekton.R")
source("scripts/CoastalWetlands.R")
source("scripts/SAV-Functions.R")
source("scripts/Coral.R")
source("scripts/Oyster.R")
# Imports SAV4 created by SAV.R above
SAV4 <- readRDS("../SAV/output/SAV_DataUsed.rds")
############################

seacar_palette <- c("#005396", "#0088B1", "#00ADAE", "#65CCB3", "#AEE4C1", 
                    "#FDEBA8", "#F8CD6D", "#F5A800", "#F17B00")

################
## file names ##
# Pulls file names from discrete and cont. file list .txt rendered during .RDS object creation
wq_discrete_file <- fread("../WQ_Cont_Discrete/output/tables/disc/disc_file_list.txt", sep='|')
wq_discrete_files <- wq_discrete_file %>% 
  pivot_longer(cols=names(wq_discrete_file)) %>%
  pull(unique(value))

wq_cont_file <- fread("../WQ_Cont_Discrete/output/tables/cont/cont_file_list.txt", sep='|')
wq_cont_files <- wq_cont_file %>%
  pivot_longer(cols=names(wq_cont_file)) %>%
  pull(unique(value))
# Removes file path and isolates file names
wq_cont_files_short <- lapply(wq_cont_files, function(x){tail(str_split(x, "/")[[1]],1)})

#################
#################

# Point to discrete map locations (pre-made in WQ_Cont_Discrete script)
discrete_map_locs <- list.files("../WQ_Cont_Discrete/output/maps/discrete", full.names = T, pattern = ".png")
cont_map_locs <- list.files("../WQ_Cont_Discrete/output/maps/continuous/", full.names = T, pattern = ".png")
sav_map_locs <- list.files("../SAV/output/maps/", full.names = T, pattern = ".png")
coral_map_locs <- list.files("../Coral/output/maps/", full.names = T, pattern = ".png")
nekton_map_locs <- list.files("../Nekton/output/maps/", full.names = T, pattern = ".png")
cw_map_locs <- list.files("../Coastal_Wetlands/output/maps/", full.names = T, pattern = ".png")
oyster_map_locs <- list.files("../Oyster/output/maps/", full.names = T, pattern = ".png")
# Point to discrete plot locations
discrete_plot_locs <- list.files("../WQ_Cont_Discrete/output/WQ_Discrete", full.names = T)
# Point to continuous plot locations
cont_plot_locs <- list.files("../WQ_Cont_Discrete/output/WQ_Continuous", full.names = T)

# Function to locate WQ plots
get_plot <- function(ma_abrev, parameter, type, pid){
  areaID <- MA_All[Abbreviation==ma_abrev, AreaID]
  pvID <- websiteParams[SamplingFrequency==type & Website==1 & 
                          ParameterName==parameter, ParameterVisId]
  
  # Determine which file lsit to search through
  if(type=="Discrete"){
    pattern <- paste0("output/WQ_Discrete/ma-", areaID, "-pv-", pvID, ".png")
    file <- str_subset(discrete_plot_locs, pattern)
  }else{
    # When all stations are plotted without ProgramID subdivisions
    if(pid=="none"){
      pattern <- paste0("output/WQ_Continuous/ma-", areaID, "-pv-", pvID, ".png")
      # When parameter data is divided into ProgramID subdivisions
    } else {
      pattern <- paste0("output/WQ_Continuous/ma-", areaID, "-pv-", pvID, ".", pid, ".png")
    }
    file <- str_subset(cont_plot_locs, pattern)
  }
  return(file)
}

# Read each species-based habitat file in separately for more efficient parsing in species_available function
species_data <- list()
species_data[["SAV"]] <- fread(sav_file_in, sep='|', na.strings = "NULL")
species_data[["Coral"]] <- fread(coral_file_in, sep='|', na.strings = "NULL")
species_data[["CW"]] <- fread(cw_file_in, sep='|', na.strings = "NULL")
species_data[["Nekton"]] <- fread(nekton_file_in, sep='|', na.strings = "NULL")

# Function to return species lists at end of report
species_available <- function(ma){
  all_sp <- c() # Collect all available species for each ma
  used_in_analysis <- list() # Collect which species are included in analysis
  species_reject <- c("Vallisneria americana", "Najas guadalupensis",
                      "Hydrilla verticillata", "Potamogeton pusillus",
                      "Zannichellia palustris")
  if(in_sav){
    dataFile <- species_data[["SAV"]] %>% filter(ManagedAreaName==ma)
    all_sp <- c(all_sp, unique(dataFile$CommonIdentifier))
    # Species list of species used in analysis
    sp <- unique(dataFile %>% 
                   filter(SpeciesGroup1 %in% c("Seagrass", "Macroalgae", "Total SAV"),
                          !CommonIdentifier %in% species_reject) %>% 
                   pull(CommonIdentifier))
    used_in_analysis[["Submerged Aquatic Vegetation"]] <- sp
  }
  
  if(in_coral){
    dataFile <- species_data[["Coral"]] %>% filter(ManagedAreaName==ma)
    all_sp <- c(all_sp, unique(dataFile$CommonIdentifier))
    sp1 <- unique(dataFile %>% filter(SpeciesGroup1 %in% c("Grazers and reef dependent species", "Reef fish"),
                                      ParameterName=="Presence/Absence") %>% pull(CommonIdentifier))
    sp2 <- unique(dataFile %>% filter(SpeciesGroup1 %in% c("Octocorals","Milleporans","Scleractinians"),
                                      ParameterName=="Percent Cover") %>% pull(CommonIdentifier))
    used_in_analysis[["Coral Reef - Species Richness"]] <- sp1
    used_in_analysis[["Coral Reef - Percent Cover"]] <- sp2
  }
  
  if(in_cw){
    dataFile <- species_data[["CW"]] %>% filter(ManagedAreaName==ma)
    all_sp <- c(all_sp, unique(dataFile$CommonIdentifier))
    sp <- unique(dataFile %>% filter(SpeciesGroup1 %in% c("Marsh","Marsh succulents",
                                                          "Mangroves and associates")) %>% 
                   pull(CommonIdentifier))
    used_in_analysis[["Coastal Wetlands"]] <- sp
  }
  
  if(in_nekton){
    dataFile <- species_data[["Nekton"]] %>% filter(ManagedAreaName==ma)
    all_sp <- c(all_sp, unique(dataFile$CommonIdentifier))
    sp <- unique(dataFile %>% filter(!is.na(SpeciesGroup2)) %>% pull(CommonIdentifier))
    used_in_analysis[["Nekton"]] <- sp
  }
  
  all_sp <- sort(unique(all_sp))
  
  all_species <- all_sp
  legend_key <- c()
  for(i in 1:length(names(used_in_analysis))){
    hab <- names(used_in_analysis)[i]
    if(report_type=="PDF"){
      all_species <- ifelse(all_species %in% used_in_analysis[[i]], paste0(all_species, "\\textsuperscript{", i, "}"), all_species)
    } else {
      all_species <- ifelse(all_species %in% used_in_analysis[[i]], paste0(all_species, "^", i, "^"), all_species)
    }
    legend_key <- c(legend_key, paste0(i, " - ", hab))
  }
  
  all_species_m <- matrix(all_species, ncol = 3, byrow=FALSE)
  
  cat("  \n")
  print(
    kable(all_species_m, format = ifelse(report_type=="PDF", "latex", "simple"), 
          escape=F, longtable=T, booktabs=T, linesep="") %>% 
      kable_styling(latex_options=c("scale_down", "HOLD_position"))
  )
  cat("  \n")
  cat(paste(legend_key, collapse = ", "))
  cat("  \n")
}

##### THE LATEST `SEACAR_Metadata.xlsx` is needed as an input!!!! -----
# Download any file from the DDI to obtain latest `SEACAR_Metadata.xlsx`
# Import thresholds
thresholds <- openxlsx::read.xlsx("data/SEACAR_Metadata.xlsx", 
                                  startRow = 6, sheet = "Ref_QAThresholds")
# Import QAQC Flag descriptions
qaqc_table <- openxlsx::read.xlsx("data/SEACAR_Metadata.xlsx", 
                                  sheet = "Ref_QAQCFlag", 
                                  rows = c(6:21),
                                  sep.names = " ")
vq_desc_table <- openxlsx::read.xlsx("data/SEACAR_Metadata.xlsx", 
                                     sheet = "Ref_Include", 
                                     rows = c(35:54),
                                     sep.names = " ")

# Subset for MAs
MA_All <- MA_All[!MA_All$ManagedAreaName=="Biscayne Bay-Cape Florida to Monroe County Line Aquatic Preserve"]

# Load in Figure Captions
FigureCaptions <- SEACAR::FigureCaptions %>% 
  mutate(FigureCaptions = stringi::stri_replace_all_regex(
    FigureCaptions,
    pattern = c("<p>", "</p>"),
    replacement = c("", ""),
    vectorize = FALSE
  )) %>% as.data.table()

# Load in TableDescriptions
# Create separate columns for HTML and LaTeX formatted description statements
TableDescriptions <- SEACAR::TableDescriptions %>%
  mutate(DescriptionHTML = Description,
         DescriptionLatex = stringi::stri_replace_all_regex(
           Description,
           pattern = c("<i>", "</i>", "&#8805;"),
           replacement = c("*", "*", ">="),
           vectorize = FALSE
         )) %>%
  as.data.table()

# Choose which type of report to render, or render both
report_types <- c("PDF", "HTML")

# iterate through every possible MA
# apply checks for coral, sav, etc. within .Rmd doc
tic()
for(i in seq_len(nrow(MA_All))){
  
  ma <- MA_All[i, ]$ManagedAreaName
  ma_short <- MA_All[i, ]$ShortName
  
  # MA abbreviation
  ma_abrev <- MA_All[i, ]$Abbreviation
  # if(!ma_abrev=="BBCFMCAP") next
      
  # perform checks for habitats in each MA
  # Check which habitats to include in each MA
  in_sav <- ma %in% sav_managed_areas
  in_nekton <- ma %in% nekton_managed_areas
  in_coral <- ma %in% coral_managed_areas
  in_cw <- ma %in% cw_managed_areas
  in_discrete <- ma %in% disc_managed_areas
  in_continuous <- ma %in% cont_managed_areas
  in_oyster <- ma %in% oyster_managed_areas
  
  #####################
  ### RENDER REPORT ### ----
  #####################

  if(in_sav | in_nekton | in_coral | in_cw | in_discrete | in_continuous){
    
    # Render reports in output/Reports/ folder
    ma_report_out_dir <- paste0(report_out_dir)

    for(report_type in report_types){
      # Determine which description format to render
      descriptionColumn <- ifelse(report_type=="HTML", "DescriptionHTML", "DescriptionLatex")
      ma_report_out_dir <- paste0(report_out_dir,"/",report_type)

      file_out <-  paste0(ma_abrev, "_Report")
      format_string <- paste0(tolower(report_type),"_document")
      
      rmarkdown::render(input = "ReportTemplate.Rmd",
                        output_format = format_string,
                        output_file = paste0(file_out, ".", tolower(report_type)),
                        output_dir = ma_report_out_dir,
                        clean=TRUE)
      
      #Removes unwanted files created in the rendering process
      unlink(paste0(ma_report_out_dir, "/", file_out, ".md"))
      unlink(paste0(ma_report_out_dir, "/", file_out, ".tex"))
      unlink(paste0(ma_report_out_dir, "/", file_out, "_files"), recursive=TRUE)
      unlink(paste0(file_out, ".log"))
    }
  }
}
toc()

# Render index.html directory to list on GitHub pages
knitr::knit("index.Rhtml")

###### Render Atlas Reports -----
if(render_atlas_reports){
  # Renders an individual report for each MA
  for(i in seq_len(nrow(MA_All))){
    
    ma <- MA_All[i, ]$ManagedAreaName
    ma_short <- MA_All[i, ]$ShortName
    
    # MA abbreviation
    ma_abrev <- MA_All[i, ]$Abbreviation
    
    # perform checks for habitats in each MA
    # Check which habitats to include in each MA
    
    in_sav <- ma %in% sav_managed_areas
    in_nekton <- ma %in% nekton_managed_areas
    in_coral <- ma %in% coral_managed_areas
    in_cw <- ma %in% cw_managed_areas
    in_discrete <- ma %in% disc_managed_areas
    in_continuous <- ma %in% cont_managed_areas
    in_oyster <- ma %in% oyster_managed_areas
    
    #####################
    ### RENDER REPORT ### ----
    #####################
    
    if(in_sav | in_nekton | in_coral | in_cw | in_discrete | in_continuous){
      # Render reports in output/Reports/ folder
      ma_report_out_dir <- paste0(report_out_dir, "/AtlasReports")
      
      report_type <- "HTML"
      
      file_out <-  paste0(ma_abrev, "_AtlasReport")
      format_string <- paste0(tolower(report_type),"_document")
      
      rmarkdown::render(input = "AtlasReportTemplate.Rmd",
                        output_format = format_string,
                        output_file = paste0(file_out, ".", tolower(report_type)),
                        output_dir = ma_report_out_dir,
                        clean=TRUE)
      
      #Removes unwanted files created in the rendering process
      unlink(paste0(ma_report_out_dir, "/", file_out, ".md"))
      unlink(paste0(ma_report_out_dir, "/", file_out, ".tex"))
      unlink(paste0(ma_report_out_dir, "/", file_out, "_files"), recursive=TRUE)
      unlink(paste0(file_out, ".log"))
    }
  }
  # Render a final, combined Atlas report
  # Contains all potential habitats/indicators
  rmarkdown::render(input = "AtlasReportTemplate_Combined.Rmd",
                    output_format = "html_document",
                    output_file = "AtlasReport_combined.html",
                    output_dir = paste0(report_out_dir, "/AtlasReports"),
                    clean=TRUE)
  rmarkdown::render(input = "AtlasReportTemplate_Combined.Rmd",
                    output_format = "word_document",
                    output_file = "AtlasReport_combined.docx",
                    output_dir = paste0(report_out_dir, "/AtlasReports"),
                    clean=TRUE)
  unlink(paste0(report_out_dir, "/AtlasReports/AtlasReport_combined.md"))
}

# Create parallel-ready function to render reports
# library(parallel)
# # Render report function
# render_ma_report <- function(i, MA_All, report_out_dir, report_types,
#                              sav_managed_areas, nekton_managed_areas, coral_managed_areas,
#                              cw_managed_areas, disc_managed_areas, cont_managed_areas,
#                              managed_area_df, wq_discrete_files, nekton_file_in,
#                              cw_file_in, coral_file_in, sav_file_in, oyster_file_in,
#                              locs_pts_rcp, find_shape, get_shape_coordinates, rcp, station_coordinates,
#                              coordinates_df) {
#   library(knitr)
#   library(readr)
#   library(tidyverse)
#   library(data.table)
#   library(purrr)
#   library(rstudioapi)
#   library(stringr)
#   library(utils)
#   library(geosphere)
#   library(leaflet)
#   library(leaflegend)
#   library(mapview)
#   library(magick)
#   library(mgcv)
#   library(cowplot)
#   library(webshot)
#   library(sf)
#   library(fontawesome)
#   library(gridExtra)
#   library(ggpubr)
#   library(glue)
#   library(kableExtra)
#   library(distill)
#   library(dplyr)
#   library(RColorBrewer)
#   library(tictoc)
#   
#   source("scripts/WQ_Continuous.R")
#   source("scripts/WQ_Discrete.R")
#   source("scripts/Nekton.R")
#   source("scripts/CoastalWetlands.R")
#   source("scripts/SAV-Functions.R")
#   source("scripts/Coral.R")
#   source("scripts/Oyster.R")
#   
#   tryCatch({
# 
#     message(paste("Processing index:", i))
# 
#     # Check if the required columns exist in the data
#     if (!"ManagedAreaName" %in% colnames(MA_All)) {
#       stop("Column 'ManagedAreaName' not found in MA_All")
#     }
#     if (!"ManagedAreaName" %in% colnames(managed_area_df)) {
#       stop("Column 'ManagedAreaName' not found in managed_area_df")
#     }
# 
#     ma <- MA_All[i, "ManagedAreaName"]
#     ma_short <- MA_All[i, "ShortName"]
#     ma_abrev <- MA_All[i, "Abbreviation"]
# 
#     # Log the Managed Area being processed
#     message(paste("Managed Area:", ma, "| ShortName:", ma_short, "| Abbreviation:", ma_abrev))
# 
#     # Perform checks for habitats in each MA
#     in_sav <- ma %in% sav_managed_areas
#     in_nekton <- ma %in% nekton_managed_areas
#     in_coral <- ma %in% coral_managed_areas
#     in_cw <- ma %in% cw_managed_areas
#     in_discrete <- ma %in% disc_managed_areas
#     in_continuous <- ma %in% cont_managed_areas
# 
#     # Log which habitat checks passed
#     message(paste("Habitat checks - SAV:", in_sav, "| Nekton:", in_nekton,
#                   "| Coral:", in_coral, "| CW:", in_cw, "| Discrete:", in_discrete,
#                   "| Continuous:", in_continuous))
# 
#     # Render report if the area is in any habitat group
#     if (in_sav | in_nekton | in_coral | in_cw | in_discrete | in_continuous) {
#       for (report_type in report_types) {
#         ma_report_out_dir <- paste0(report_out_dir, "/", report_type)
#         file_out <- paste0(ma_abrev, "_Report")
#         format_string <- paste0(tolower(report_type), "_document")
# 
#         message(paste("Rendering report for:", ma_abrev, "as", format_string))
# 
#         rmarkdown::render(input = "ReportTemplate.Rmd",
#                           output_format = format_string,
#                           output_file = paste0(file_out, ".", tolower(report_type)),
#                           output_dir = ma_report_out_dir,
#                           params = list(
#                             ma = ma,
#                             managed_area_df = managed_area_df,
#                             ma_abrev = ma_abrev,
#                             in_sav = in_sav,
#                             in_nekton = in_nekton,
#                             in_coral = in_coral,
#                             in_cw = in_cw,
#                             in_discrete = in_discrete,
#                             in_continuous = in_continuous
#                           ),
#                           clean = TRUE)
# 
#         # Remove unwanted files created in the rendering process
#         unlink(paste0(ma_report_out_dir, "/", file_out, ".md"))
#         unlink(paste0(ma_report_out_dir, "/", file_out, ".tex"))
#         unlink(paste0(ma_report_out_dir, "/", file_out, "_files"), recursive = TRUE)
#       }
#     }
#   }, error = function(e) {
#     message(paste("Error in iteration", i, ":", e))
#     return(e)
#   })
# }
# 
# # Create a cluster
# cl <- makeCluster(detectCores() - 8)
# 
# # Export necessary variables and functions to the cluster
# clusterExport(cl, varlist = c("MA_All", "render_ma_report", "report_out_dir", "report_types",
#                               "sav_managed_areas", "nekton_managed_areas", "coral_managed_areas",
#                               "cw_managed_areas", "disc_managed_areas", "cont_managed_areas",
#                               "managed_area_df", "wq_discrete_files", "nekton_file_in",
#                               "cw_file_in", "coral_file_in", "sav_file_in", "oyster_file_in",
#                               "locs_pts_rcp", "find_shape", "get_shape_coordinates", "rcp",
#                               "station_coordinates", "coordinates_df", "thresholds", 
#                               "qaqc_table", "vq_desc_table", "SAV4"))
# 
# tic()
# # Use parLapply to parallelize the rendering
# parLapply(cl, seq_len(nrow(MA_All[1])), render_ma_report, MA_All = MA_All,
#           report_out_dir = report_out_dir, report_types = report_types,
#           sav_managed_areas = sav_managed_areas,
#           nekton_managed_areas = nekton_managed_areas,
#           coral_managed_areas = coral_managed_areas,
#           cw_managed_areas = cw_managed_areas,
#           disc_managed_areas = disc_managed_areas,
#           cont_managed_areas = cont_managed_areas,
#           managed_area_df = managed_area_df,
#           wq_discrete_files = wq_discrete_files,
#           nekton_file_in = nekton_file_in,
#           cw_file_in = cw_file_in,
#           coral_file_in = coral_file_in,
#           sav_file_in = sav_file_in,
#           oyster_file_in = oyster_file_in,
#           locs_pts_rcp = locs_pts_rcp,
#           find_shape = find_shape,
#           get_shape_coordinates = get_shape_coordinates,
#           rcp = rcp,
#           station_coordinates = station_coordinates,
#           coordinates_df = coordinates_df)
# toc()
# 
# # Stop the cluster
# stopCluster(cl)
