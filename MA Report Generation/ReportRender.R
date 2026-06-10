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

# Gets directory of this script and sets it as the working directory
wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

#### CREATE TABLE DESCRIPTION OUTPUT ####
# Combines all habitat-specific table descriptions into a final file for use in these reports and on SEACAR Atlas
# Requires the Atlas-Tools repository be in the same root folder as SEACAR_Trend_Analyses repository
# source("../../Atlas-Tools/tableDescriptions/run.R")
#### RUN MODEL AGGREGATION PROCEDURES ####
# source("../../Atlas-Tools/ModelAggregation/run.R")
# The output from model aggregation procedures is loaded within ReportTemplate.Rmd

# Load in the newly created table description output
td_files <- list.files("../../Atlas-Tools/tableDescriptions/output/", full=T, pattern = ".xlsx")
TableDescriptions <- setDT(openxlsx::read.xlsx(td_files[which.max(file.info(td_files)$mtime)]) %>%
                             mutate(DescriptionLatex = gsub("&#8805;", ">=", Description)))
cat(glue("Using file: {td_files[which.max(file.info(td_files)$mtime)]} \n"))
rm(td_files)
#########################################

## Render AtlasReports? - provide overview in similar format to Atlas - helpful for review
render_atlas_reports <- FALSE

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

# Determine list of programs managed by RCP to highlight them in reports
managingEntities <- openxlsx::read.xlsx("data/MonitoringPrograms_ManagingEntityUpdate_2025-12.xlsx") %>%
  mutate(ManagingEntity = ifelse(is.na(ActionNeeded), Managing.Entity, Managing.Entity_proposed)) %>% 
  select(Id, Name, ManagingEntity) %>% rename("ProgramID" = "Id", "ProgramName" = "Name") %>% 
  rowwise() %>%
  mutate(Entity = str_split_1(ManagingEntity, ";")[1])
rcp_progs <- managingEntities %>% 
  filter(stringr::str_detect(Entity, "Office of Resilience")) %>% pull(ProgramID)
# Colorize function to enable color throughout report (LaTeX format)
colorize <- function(x, color, reportType){
  if(reportType=="HTML"){
    sprintf("<span style='color: %s;'>%s</span>", color, x)
  } else if(reportType=="PDF"){
    sprintf("\\textcolor{%s}{%s}", color, x)
  }
}
# Colorize table function to color program ID values (and others) within tables
colorize_tables <- function(df, color){
  f <- ifelse(report_type=="HTML", "html", "latex")
  df %>% mutate(text_color = ifelse(ProgramID %in% rcp_progs, color, "black"),
                ProgramID = cell_spec(ProgramID, color = text_color, format = f)) %>% 
    select(-text_color)
}

#Gets the desired file locations
#Imports SEACAR data file path information as variable "seacar_data_location"
source("../../SEACAR_Trend_Analyses/SEACAR_data_location.R")

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

seacar_palette <- SEACAR::seacar_palette1

################
## file names ##
# Pulls file names from discrete and cont. file list .txt rendered during .RDS object creation
wq_discrete_file <- fread("../WQ_Cont_Discrete/output/tables/disc/disc_file_list.txt", sep='|')
wq_discrete_files <- unique(wq_discrete_file$file_short)

wq_cont_file <- fread("../WQ_Cont_Discrete/output/tables/cont/cont_file_list.txt", sep='|')
wq_cont_files <- unique(wq_cont_file$file_short)

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
    dataFile <- species_data[["SAV"]] %>% filter(str_detect(ManagedAreaName, ma))
    all_sp <- c(all_sp, unique(dataFile$CommonIdentifier))
    # Species list of species used in analysis
    sp <- unique(dataFile %>% 
                   filter(SpeciesGroup1 %in% c("Seagrass", "Macroalgae", "Total SAV"),
                          !CommonIdentifier %in% species_reject) %>% 
                   pull(CommonIdentifier))
    used_in_analysis[["Submerged Aquatic Vegetation"]] <- sp
  }
  
  if(in_coral){
    dataFile <- species_data[["Coral"]] %>% filter(str_detect(ManagedAreaName, ma))
    all_sp <- c(all_sp, unique(dataFile$CommonIdentifier))
    sp1 <- unique(dataFile %>% filter(SpeciesGroup1 %in% c("Grazers and reef dependent species", "Reef fish"),
                                      ParameterName=="Presence/Absence") %>% pull(CommonIdentifier))
    sp2 <- unique(dataFile %>% filter(SpeciesGroup1 %in% c("Octocorals","Milleporans","Scleractinians"),
                                      ParameterName=="Percent Cover") %>% pull(CommonIdentifier))
    used_in_analysis[["Coral Reef - Species Richness"]] <- sp1
    used_in_analysis[["Coral Reef - Percent Cover"]] <- sp2
  }
  
  if(in_cw){
    dataFile <- species_data[["CW"]] %>% filter(str_detect(ManagedAreaName, ma))
    all_sp <- c(all_sp, unique(dataFile$CommonIdentifier))
    sp <- unique(dataFile %>% filter(SpeciesGroup1 %in% c("Marsh","Marsh succulents",
                                                          "Mangroves and associates")) %>% 
                   pull(CommonIdentifier))
    used_in_analysis[["Coastal Wetlands"]] <- sp
  }
  
  if(in_nekton){
    dataFile <- species_data[["Nekton"]] %>% filter(str_detect(ManagedAreaName, ma))
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
ddi_metadata_file <- "https://data.florida-seacar.org/static/metadataexport/SEACAR_Metadata.xlsx"
thresholds <- openxlsx::read.xlsx(ddi_metadata_file, 
                                  startRow = 6, sheet = "Ref_QAThresholds")
# Import QAQC Flag descriptions
qaqc_table <- openxlsx::read.xlsx(ddi_metadata_file, 
                                  sheet = "Ref_QAQCFlag", 
                                  rows = c(6:21),
                                  sep.names = " ")
vq_desc_table <- openxlsx::read.xlsx(ddi_metadata_file, 
                                     sheet = "Ref_Include", 
                                     rows = c(35:54),
                                     sep.names = " ")

# Subset for MAs
# MA_All <- MA_All[!MA_All$ManagedAreaName=="Biscayne Bay-Cape Florida to Monroe County Line Aquatic Preserve"]

# Load in Figure Captions
FigureCaptions <- SEACAR::FigureCaptions %>% 
  mutate(FigureCaptions = stringi::stri_replace_all_regex(
    FigureCaptions,
    pattern = c("<p>", "</p>"),
    replacement = c("", ""),
    vectorize = FALSE
  )) %>% as.data.table()

# Choose which type of report to render, or render both
report_types <- c("PDF", "HTML")

# iterate through every possible MA
# apply checks for coral, sav, etc. within .Rmd doc
tic()
for(i in seq_len(nrow(MA_All))){
  ma <- MA_All[i, ]$ManagedAreaName
  ma_short <- MA_All[i, ]$ShortName
  ma_abrev <- MA_All[i, ]$Abbreviation
  # perform checks for habitats in each MA
  # Check which habitats to include in each MA
  in_sav <- ma %in% sav_managed_areas
  in_nekton <- ma %in% nekton_managed_areas
  in_coral <- ma %in% coral_managed_areas
  in_cw <- ma %in% cw_managed_areas
  in_discrete <- ma %in% disc_managed_areas
  # in_continuous <- ma %in% cont_managed_areas
  in_continuous <- any(str_detect(cont_managed_areas, ma))
  in_oyster <- ma %in% oyster_managed_areas
  
  #####################
  ### RENDER REPORT ### ----
  #####################

  if(in_sav | in_nekton | in_coral | in_cw | in_discrete | in_continuous){
    
    # Render reports in output/Reports/ folder
    ma_report_out_dir <- paste0(report_out_dir)

    for(report_type in report_types){
      # Determine which description format to render
      descriptionColumn <- ifelse(report_type=="HTML", "Description", "DescriptionLatex")
      # descriptionColumn <- "Description"
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
    # if(i!=7) next
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
  # # Render a final, combined Atlas report
  # # Contains all potential habitats/indicators
  # rmarkdown::render(input = "AtlasReportTemplate_Combined.Rmd",
  #                   output_format = "html_document",
  #                   output_file = "AtlasReport_combined.html",
  #                   output_dir = paste0(report_out_dir, "/AtlasReports"),
  #                   clean=TRUE)
  # rmarkdown::render(input = "AtlasReportTemplate_Combined.Rmd",
  #                   output_format = "word_document",
  #                   output_file = "AtlasReport_combined.docx",
  #                   output_dir = paste0(report_out_dir, "/AtlasReports"),
  #                   clean=TRUE)
  # unlink(paste0(report_out_dir, "/AtlasReports/AtlasReport_combined.md"))
}
