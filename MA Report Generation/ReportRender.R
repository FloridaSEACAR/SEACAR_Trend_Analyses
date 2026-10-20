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

# Determine whether to provide sample location maps
show_sample_location_maps <- FALSE

# Determine whether to display status and trends tables in reports
show_status_and_trends_tables <- TRUE

# Determine whether to display descriptive snippets for each habitat
show_descriptive_snippets <- TRUE

# Determine whether to display indicator-level descriptive snippets (nutrients, water clarity, etc.)
show_descriptive_indicator_snippets <- TRUE

#### CREATE TABLE DESCRIPTION OUTPUT ####
# Combines all habitat-specific table descriptions into a final file for use in these reports and on SEACAR Atlas
# Requires the Atlas-Tools repository be in the same root folder as SEACAR_Trend_Analyses repository
source("../../Atlas-Tools/tableDescriptions/run.R", echo = T, chdir = T)
#### RUN MODEL AGGREGATION PROCEDURES ####
if(show_status_and_trends_tables){
  source("../../Atlas-Tools/ModelAggregation/run.R", echo = T, chdir = T)
}
# The output from model aggregation procedures is loaded within ReportTemplate.Rmd in Status and Trends tables

# Load in the newly created table description output
td_files <- list.files("../../Atlas-Tools/tableDescriptions/output/", full=T, pattern = ".xlsx")
TableDescriptions <- setDT(openxlsx::read.xlsx(td_files[which.max(file.info(td_files)$mtime)]) %>%
                             mutate(DescriptionLatex = gsub("&#8805;", ">=", Description)))
cat(glue("Using file: {td_files[which.max(file.info(td_files)$mtime)]} \n"))
rm(td_files)
#########################################

## Render AtlasReports? - provide overview in similar format to Atlas - helpful for review
render_atlas_reports <- FALSE

## Display banners? - choose whether to display banner images in introduction
display_banners <- TRUE

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
  species_by_habitat <- list() # Collect species by habitat, separating into used in analysis or not
  species_reject <- c("Vallisneria americana", "Najas guadalupensis",
                      "Hydrilla verticillata", "Potamogeton pusillus",
                      "Zannichellia palustris")
  cols_to_group <- c("SpeciesID", "CommonIdentifier", "SpeciesName", "GenusName", 
                     "Group1ID", "SpeciesGroup1", "Group2ID", "SpeciesGroup2")
  
  if(in_sav){
    dataFile <- species_data[["SAV"]] %>% filter(str_detect(ManagedAreaName, ma))
    # List of species used in analysis
    sp_used_in_analysis <- dataFile %>% 
      filter(str_detect(ManagedAreaName, ma),
             SpeciesGroup1 %in% c("Seagrass", "Macroalgae", "Total SAV"),
             !CommonIdentifier %in% species_reject) %>% 
      pull(CommonIdentifier) %>% 
      unique()
    # Create grouping of all species and their respective groups
    species_grouped <- dataFile %>%
      group_by(across(all_of(cols_to_group))) %>%
      reframe(analysis = NA_character_) %>% 
      mutate(used_in_analysis = CommonIdentifier %in% sp_used_in_analysis,
             SpeciesGroup2 = as.character(SpeciesGroup2))
    # Return data table
    species_by_habitat[["Submerged Aquatic Vegetation"]] <- species_grouped
  }
  
  if(in_coral){
    dataFile <- species_data[["Coral"]] %>% filter(str_detect(ManagedAreaName, ma))
    # List of species used in analysis (species richness)
    sp_used_in_analysis_sr <- dataFile %>% 
      filter(str_detect(ManagedAreaName, ma),
             SpeciesGroup1 %in% c("Grazers and reef dependent species", "Reef fish"),
             ParameterName=="Presence/Absence") %>% 
      pull(CommonIdentifier) %>% 
      unique()
    # List of species used in analysis (percent cover)
    sp_used_in_analysis_pc <- dataFile %>% 
      filter(str_detect(ManagedAreaName, ma),
             SpeciesGroup1 %in% c("Octocorals","Milleporans","Scleractinians"),
             ParameterName=="Percent Cover") %>% 
      pull(CommonIdentifier) %>%
      unique()
    # Create grouping of all species and their respective groups
    species_grouped <- dataFile %>%
      group_by(across(all_of(cols_to_group))) %>%
      reframe() %>% 
      mutate(
        used_in_analysis = CommonIdentifier %in% c(sp_used_in_analysis_sr, 
                                                   sp_used_in_analysis_pc),
        analysis = case_when(
          CommonIdentifier %in% sp_used_in_analysis_sr ~ "Species Richness",
          CommonIdentifier %in% sp_used_in_analysis_pc ~ "Percent Cover",
          TRUE ~ NA_character_
        ),
        SpeciesGroup2 = as.character(SpeciesGroup2)
      )
    # Return data table
    species_by_habitat[["Coral Reef"]] <- species_grouped
  }
  
  if(in_cw){
    dataFile <- species_data[["CW"]] %>% filter(str_detect(ManagedAreaName, ma))
    # List of species used in analysis
    sp_used_in_analysis <- dataFile %>% 
      filter(str_detect(ManagedAreaName, ma),
             SpeciesGroup1 %in% c("Marsh", "Marsh succulents", "Mangroves and associates")) %>% 
      pull(CommonIdentifier) %>% 
      unique()
    # Create grouping of all species and their respective groups
    species_grouped <- dataFile %>%
      group_by(across(all_of(cols_to_group))) %>%
      reframe(analysis = NA_character_) %>% 
      mutate(used_in_analysis = CommonIdentifier %in% sp_used_in_analysis,
             SpeciesGroup2 = as.character(SpeciesGroup2))
    # Save dataframe with all species
    species_by_habitat[["Coastal Wetlands"]] <- species_grouped
  }
  
  if(in_nekton){
    dataFile <- species_data[["Nekton"]] %>% filter(str_detect(ManagedAreaName, ma))
    # List of species used in analysis
    sp_used_in_analysis <- dataFile %>% 
      filter(str_detect(ManagedAreaName, ma) & !is.na(SpeciesGroup2)) %>% 
      pull(CommonIdentifier) %>% 
      unique()
    
    # Create grouping of all species and their respective groups
    species_grouped <- dataFile %>%
      group_by(across(all_of(cols_to_group))) %>%
      reframe(analysis = NA_character_) %>% 
      mutate(used_in_analysis = CommonIdentifier %in% sp_used_in_analysis,
             SpeciesGroup2 = as.character(SpeciesGroup2))
    
    # Save dataframe with all species
    species_by_habitat[["Nekton"]] <- species_grouped
  }
  
  legend_key <- c()
  display_species <- list()
  
  habs <- names(species_by_habitat)
  # Analysis-specific symbols
  analysis_symbols <- data.table(
    "PDF" = c("\\alpha", "\\beta", "\\gamma", "\\delta", "\\epsilon",
              "\\zeta", "\\eta", "\\theta", "\\iota", "\\kappa", "\\lambda",
              "\\mu", "\\nu", "\\xi"),
    "HTML" = c("&alpha;", "&beta;", "&gamma;", "&delta;", "&epsilon;",
               "&zeta;", "&eta;", "&theta;", "&iota;", "&kappa;", "&lambda;",
               "&mu;", "&nu;", "&xi;")
  )
  
  superscript_wrapper <- function(text, report_type){
    if(report_type=="PDF"){
      paste0("\\textsuperscript{", text, "}")
    } else {
      paste0("<sup>", text, "</sup>")
    }
  }
  
  for(i in seq_along(habs)){
    hab <- habs[[i]]
    
    all_species <- species_by_habitat[[hab]] %>%
      ungroup() %>%
      filter(!is.na(CommonIdentifier), nzchar(CommonIdentifier)) %>%
      group_by(across(any_of(cols_to_group))) %>%
      summarise(
        used_in_analysis = any(used_in_analysis),
        analyses = list(sort(unique(analysis[!is.na(analysis)]))),
        .groups = "drop"
      ) %>%
      mutate(SpeciesGroup1 = if_else(is.na(SpeciesGroup1), "NA", SpeciesGroup1)) %>%
      arrange(CommonIdentifier)
    
    legend_key <- c(
      legend_key,
      paste0(i, " - ", hab)
    )
    
    num_sp_groups_in_habitat <- sort(unique(unlist(all_species$SpeciesGroup1)))
    num_sp_groups_in_habitat <- num_sp_groups_in_habitat[num_sp_groups_in_habitat!="NA"]
    
    if(length(num_sp_groups_in_habitat) > 0){
      if(length(num_sp_groups_in_habitat) > length(analysis_symbols[[report_type]])){
        stop(
          "Only ", length(analysis_symbols[[report_type]]),
          " analysis symbols are defined, but ", hab, " contains ",
          length(num_sp_groups_in_habitat), " species groups"
        )
      }
      
      # Assign one symbol to each named analysis.
      symbol_lookup <- setNames(
        analysis_symbols[[report_type]][seq_along(num_sp_groups_in_habitat)],
        num_sp_groups_in_habitat
      )
      
      if(report_type=="PDF"){
        symbol_lookup[] <- paste0("$", symbol_lookup, "$") #[] syntax keeps vector names
      }
      
      all_species <- all_species %>%
        rowwise() %>%
        mutate(
          analysis_tag = if_else(SpeciesGroup1=="NA", "", paste0(unname(symbol_lookup[SpeciesGroup1]), collapse = "")),
          species_tag = superscript_wrapper(paste0(i, analysis_tag), report_type),
          species_with_tag = paste0(CommonIdentifier, species_tag)
        ) %>%
        ungroup()
      
      # Add one legend entry per named analysis.
      legend_key <- c(
        legend_key,
        vapply(
          num_sp_groups_in_habitat,
          FUN.VALUE = character(1),
          FUN = function(analysis_name){
            analysis_symbol <- symbol_lookup[[analysis_name]]
            paste0(i, superscript_wrapper(analysis_symbol, report_type),
                   " - ", hab, " - ", analysis_name)
          }
        )
      )
    } else {
      # Use alpha consistently for the single analysis
      symbol <- analysis_symbols[[report_type]][[1]]
      # Account for LaTeX format and add math operator $
      symbol_tag <- if(report_type == "PDF"){
        paste0("$", symbol, "$")
      } else {
        symbol
      }
      
      # Apply correct species tags to each species name
      all_species <- all_species %>%
        mutate(
          species_tag = if_else(used_in_analysis, 
                                superscript_wrapper(paste0(i, symbol_tag), report_type),
                                superscript_wrapper(as.character(i), report_type)),
          species_with_tag = paste0(CommonIdentifier, species_tag)
        )
      
      legend_key <- c(
        legend_key,
        paste0(i, superscript_wrapper(symbol_tag, report_type),
               " - ", hab, " - Used in analysis"
        )
      )
    }
    display_species[[hab]] <- all_species
  }
  
  # Unbind all individual habitats into 1 file, allow NA values to be reported
  display_species_all <- bind_rows(display_species) %>%
    mutate(SpeciesGroup1 = if_else(is.na(SpeciesGroup1), "NA", SpeciesGroup1))
  
  # Show legend key above species list table
  cat("  \n")
  cat(paste(unique(legend_key), collapse = "; "))
  cat("  \n")
  cat("  \n")
    
  display_species_vector <- sort(unlist(display_species_all$species_with_tag, use.names=FALSE))
  
  # Pad the vector so matrix() does not recycle species
  n_cols <- 3
  n_missing <- (-length(display_species_vector)) %% n_cols
  
  if(n_missing > 0){
    display_species_vector <- c(display_species_vector, rep("", n_missing))
  }
  
  all_species_m <- matrix(
    display_species_vector,
    ncol = n_cols,
    byrow = FALSE
  )
  
  if(report_type=="PDF"){
    print(
      knitr::kable(all_species_m, format = "latex", escape = F, longtable = T, 
                   booktabs = T,linesep = "", col.names = rep("", n_cols)) %>%
        kableExtra::kable_styling(latex_options = "repeat_header")
    )
  } else {
    print(
      knitr::kable(all_species_m, format = "html", escape = F, col.names = rep("", n_cols))
    )
  }
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

# Read in discrete data overview to provide program overview
disc_programs <- readRDS("../WQ_Cont_Discrete/output/tables/disc/data_output_disc.rds") %>% 
  group_by(ProgramID, ProgramName, ManagedAreaName) %>% reframe() %>% 
  arrange(ManagedAreaName) %>% as.data.table()

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

# Load in ManagedAreaName introductory text
# Save from here: https://floridadep.sharepoint.com/:x:/g/fco/seacar/IQBti7CxRiMiS5ATQoRWaV8VAcvBjKAh3NDD2eVLnpXrFyY?e=9wBzKM
# into /data/ subfolder
ma_intros <- openxlsx::read.xlsx("data/Atlas_ManagedArea_IntroductionText.xlsx")

# Choose which type of report to render, or render both
report_types <- c("PDF", "HTML")
# report_types <- c("HTML")
# report_types <- c("PDF")

# iterate through every possible MA
# apply checks for coral, sav, etc. within .Rmd doc
tic()
for(i in seq_len(nrow(MA_All))){
  ma <- MA_All[i, ]$ManagedAreaName
  ma_short <- MA_All[i, ]$ShortName
  ma_abrev <- MA_All[i, ]$Abbreviation
  if(ma_abrev != "KJCAP") next
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
