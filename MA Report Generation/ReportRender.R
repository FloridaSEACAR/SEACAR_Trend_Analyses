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
library(sf)
library(fontawesome)
library(gridExtra)
library(ggpubr)
library(glue)
library(kableExtra)
library(distill)
library(dplyr)
library(RColorBrewer)

# Gets directory of this script and sets it as the working directory
wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

# Create folder paths if they don't yet exist
folder_paths <- c("output", "output/Figures","output/Figures/BB","output/Reports", 
  "output/Data", "output/models", "output/tables","output/Data/Nekton", "output/Data/SAV","output/Data/Coral", 
  "output/Data/Coral/PercentCover", "output/Data/Coral/SpeciesRichness",
  "output/tables", "output/tables/disc", "output/tables/cont", "output/tables/SAV", "output/Data/CoastalWetlands",
  "output/maps","output/maps/discrete","output/Figures/BB/maps", "output/Density", "output/Shell_Height", "output/Percent_Live",
  "data/GLMMs","data/GLMMs/AllDates","data/GLMMs/AllDates/Data", "output/Reports/HTML", "output/Reports/PDF")
for (path in folder_paths){if(!dir.exists(path)){dir.create(path)}}

#Set output directory
out_dir <- "output"
report_out_dir <- "output/Reports"

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE, na.strings = "")

#Gets the desired file locations
#Imports SEACAR data file path information as variable "seacar_data_location"
source("scripts/SEACAR_data_location.R")

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

oyster_file_in <- str_subset(hab_files, "Oyster")
oyster_file_short <- tail(str_split(oyster_file_in, "/")[[1]],1)

cont_files <- str_subset(files, "_NUT_cont")

############################
### call in source files ### -----
############################

# source("scripts/WQ_Discrete_Data_Creation.R") # creates source files (.rds objects) for discrete WQ
# source("scripts/WQ_Continuous_Data_creation.R") # creates source files (.rds objects) for continuous WQ
source("scripts/WQ_Continuous.R")
source("scripts/WQ_Discrete.R")
source("scripts/Nekton.R")
source("scripts/CoastalWetlands.R")
# source("scripts/SAV.R") # creates source files (.rds objects) for SAV
# source("scripts/load_shape_files.R")
# source("scripts/SAV_scope_plots.R")
source("scripts/SAV-Functions.R")
source("scripts/Coral.R")
# source("scripts/WQ_KendallTau_Stats_Combine.R")
# Imports SAV4 created by SAV.R above
SAV4 <- readRDS("output/data/SAV/SAV4.rds")
############################

seacar_palette <- c("#005396", "#0088B1", "#00ADAE", "#65CCB3", "#AEE4C1", 
                    "#FDEBA8", "#F8CD6D", "#F5A800", "#F17B00")

################
## file names ##
# Pulls file names from discrete and cont. file list .txt rendered during .RDS object creation
wq_discrete_file <- fread("output/tables/disc/disc_file_list.txt", sep='|')
wq_discrete_files <- wq_discrete_file %>% 
  pivot_longer(cols=names(wq_discrete_file)) %>%
  pull(unique(value))

wq_cont_file <- fread("output/tables/cont/cont_file_list.txt", sep='|')
wq_cont_files <- wq_cont_file %>%
  pivot_longer(cols=names(wq_cont_file)) %>%
  pull(unique(value))
# Removes file path and isolates file names
wq_cont_files_short <- lapply(wq_cont_files, function(x){tail(str_split(x, "/")[[1]],1)})

#################
#################

# Subset for MAs
# MA_All <- MA_All[c(14,5,32,27,9,33)]
# MA_All <- MA_All[c(14)]
# MA_All <- MA_All[33]

# Choose which type of report to render, or render both
report_types <- c("PDF", "HTML")
# report_types <- c("PDF")

# iterate through every possible MA
# apply checks for coral, sav, etc. within .Rmd doc
for (i in seq_len(nrow(MA_All))) {
  
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
  
  #####################
  ### RENDER REPORT ### ----
  #####################
  
  if(in_sav | in_nekton | in_coral | in_cw | in_discrete | in_continuous){
    
    # Render reports in their own subfolders
    # ma_report_out_dir <- paste0(report_out_dir, "/", ma_abrev)
    # Render reports in output/Reports/ folder
    ma_report_out_dir <- paste0(report_out_dir)
    
    for(type in report_types){
      ma_report_out_dir <- paste0(report_out_dir,"/",type)
      
      file_out <-  paste0(ma_abrev, "_Report")
      format_string <- paste0(tolower(type),"_document")
      
      rmarkdown::render(input = "ReportTemplate.Rmd",
                        output_format = format_string,
                        output_file = paste0(file_out, ".", tolower(type)),
                        output_dir = ma_report_out_dir,
                        clean=TRUE)
      
      #Removes unwanted files created in the rendering process
      unlink(paste0(ma_report_out_dir, "/", file_out, ".md"))
      unlink(paste0(ma_report_out_dir, "/", file_out, ".tex"))
      unlink(paste0(ma_report_out_dir, "/", file_out, "_files"), recursive=TRUE)
    }
  }
}

# Render index.html directory to list on GitHub pages
rmarkdown::render(input = "IndexTemplate.Rmd",
                  output_format = "html_document",
                  output_file = "index.html",
                  clean=TRUE)