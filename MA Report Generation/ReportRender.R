#Load libraries
library(knitr)
library(readr)
library(tidyverse)
library(data.table)
library(dplyr)
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
library(extrafont)
library(bookdown)
loadfonts()

# Gets directory of this script and sets it as the working directory
wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

# Create folder paths if they don't yet exist
folder_paths <- c("output", "output/Figures","output/Figures/BB","output/Reports", 
  "output/Data", "output/models", "output/tables","output/Data/Nekton", "output/Data/SAV",
  "output/Data/Coral", "output/Data/Coral/PercentCover", "output/Data/Coral/SpeciesRichness",
  "output/tables", "output/tables/disc", "output/tables/cont", "output/tables/SAV", "output/Data/CoastalWetlands",
  "output/maps", "output/Figures/BB/maps")
for (path in folder_paths){if(!dir.exists(path)){dir.create(path)}}

#Set output directory
out_dir <- "output"
report_out_dir <- "output/Reports"

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
                na.strings = "")
MA_All[, `:=` (Abbreviation = abbreviate(ManagedAreaName, minlength = 1),
               # Abbreviation2 = gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', ManagedAreaName, perl = TRUE),
               Region = fcase(str_detect(ManagedAreaName, "Alligator|Apalachicola|Bend|Pickens|Rocky|^St\\.|Yellow|Nature|Rainbow|Jackson"), "NW",
                              str_detect(ManagedAreaName, "Pinellas|Boca|Cockroach|Terra|Lemon|Haze|Gasparilla|Pine|Matlacha|Estero|Rookery|Romano"), "SW",
                              str_detect(ManagedAreaName, "Coupon|Lignumvitae|Biscayne|Keys|Coral"), "SE",
                              str_detect(ManagedAreaName, "Loxahatchee|Jensen|Lucie|Indian|Banana|Mosquito|Wekiva|Oklawaha|Tomoka|Pellicer|Guana|Nassau|Clinch"), "NE"))]

#Gets the desired file locations
files <- list.files("data", full=TRUE)
files <- str_subset(files, "All_")

cw_file_in <- list.files("data", pattern="All_CW", full=TRUE)
cw_file_short <- sub("data/", "", cw_file_in)

coral_file_in <- list.files("data", pattern="All_CORAL", full=TRUE)
coral_file_short <- sub("data/", "", coral_file_in)

nekton_file_in <- list.files("data", pattern="All_NEKTON", full=TRUE)
nekton_file_short <- sub("data/", "", nekton_file_in)

sav_file_in <- list.files("data", pattern="All_SAV", full=TRUE)
sav_file_short <- sub("data/", "", sav_file_in)

############################
### call in source files ### -----
############################
# creates source files (.rds objects) for discrete WQ
# source("scripts/WQ_Discrete_Data_Creation.R")
# source("scripts/WQ_Continuous_Data_creation.R")
source("scripts/WQ_Continuous.R")
source("scripts/WQ_Discrete.R")
source("scripts/Nekton.R")
source("scripts/CoastalWetlands.R")
# # creates source files (.rds objects) for SAV
# source("scripts/SAV.R")
# source("scripts/SAV_scope_plots.R")
source("scripts/SAV-Functions.R")
source("scripts/Coral.R")
############################

################
## file names ##
################

wq_discrete_file <- fread("output/tables/disc/disc_file_list.txt", sep='|')
wq_discrete_files <- wq_discrete_file %>% 
  pivot_longer(cols=names(wq_discrete_file)) %>%
  pull(unique(value))

wq_cont_file <- fread("output/tables/cont/cont_file_list.txt", sep='|')
wq_cont_files <- wq_cont_file %>%
  pivot_longer(cols=names(wq_cont_file)) %>%
  pull(unique(value))

#################
#################

# Subset for MAs
# MA_All <- MA_All[c(14,5,32,27,9,33)]
# MA_All <- MA_All[c(14,5,32)]

# iterate through every possible MA
# apply checks for coral, sav, etc. within .Rmd doc
for (i in seq_len(nrow(MA_All))) {
  
  ma <- MA_All[i, ]$ManagedAreaName
  ma_short <- MA_All[i, ]$ShortName
  
  # MA abbreviation
  ma_abrev <- MA_All[i, ]$Abbreviation
  
  # perform checks for habitats in each MA
  # Check which habitats to include in each MA
  in_sav <- ma_abrev %in% sav_managed_areas
  in_nekton <- ma %in% nekton_managed_areas
  in_coral <- ma %in% coral_managed_areas
  in_cw <- ma %in% cw_managed_areas
  in_discrete <- ma %in% disc_managed_areas
  in_continuous <- ma %in% cont_managed_areas
  
  #####################
  ### RENDER REPORT ### ----
  #####################
  
  if(in_sav | in_nekton | in_coral | in_cw | in_discrete | in_continuous){
    
    ma_report_out_dir <- paste0(report_out_dir, "/", ma_abrev, "/")
    
    file_out <-  paste0(ma_abrev, "_Report")
    
    rmarkdown::render(input = "ReportTemplate.Rmd",
                      output_format = "pdf_document",
                      output_file = paste0(file_out, ".pdf"),
                      output_dir = ma_report_out_dir,
                      clean=TRUE)
    # rmarkdown::render(input = "ReportTemplate.Rmd",
    #                   output_format = "html_document",
    #                   output_file = paste0(file_out, ".html"),
    #                   output_dir = ma_report_out_dir,
    #                   clean=TRUE)
    
    #Removes unwanted files created in the rendering process
    # unlink(paste0(ma_report_out_dir, "/", file_out, ".md"))
    # unlink(paste0(ma_report_out_dir, "/", file_out, "_files"), recursive=TRUE)
    
  }
}
