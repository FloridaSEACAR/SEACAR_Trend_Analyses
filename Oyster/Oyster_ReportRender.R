# The purpose of this script is to automate the production of Rmd documents for oyster analysis.
# Created by J.E. Panzik (jepanzik@usf.edu) for SEACAR

## WHEN RUNNING IN RSTUDIO:
## Set working directory to "Source File Location" in "Session" menu

## To ensure this script runs smoothly, please run in a fresh session of R
## Some other libraries cause this script not to work properly

#Load libraries
library(knitr)
library(readr)
library(dplyr)
library(data.table)
library(rstudioapi)
library(SEACAR)
library(stringr)

source("../SEACAR_data_location.R")

##### Which analysis to run? Select one, can only be run individually (by ManagedAreaName: "ma" or by OIMMP: "oimmp")
# analysis <- "oimmp"
analysis <- "ma"
#####

analysis_col <- ifelse(analysis=="ma", "ManagedAreaName", "OIMMP")

# Gets directory of this script and sets it as the working directory
wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

# Determine whether to run Oyster EDA plots (T or F)
EDA <- TRUE

# Determine whether to generate Oyster sampling maps (for SEACAR Atlas)
create_maps <- TRUE

# Determine whether to generate Oyster spatio-temporal scope plots
create_scope_plots <- TRUE

# Spatio-temporal plots are created by scrip located in SEACAR_Trend_Analyses parent folder (multi-habitat)
if(create_scope_plots){
  analyze_type <- analysis
  analyze_hab <- "oyster"
  source("../AllHabitats_Spatiotemporal_Scope_Plots.R", echo = T, chdir = T)
}

if(EDA){
  source("Oyster_EDA.R")
}

# Source in scripts to run Oyster analyses
# Oyster_Models_Clean_parallel.R to run all models and generate plots
source("Oyster_Models_parallel.R")
# Oyster_ResultsCompile.R to combine all results into single file (for Atlas)
source("Oyster_ResultsCompile.R", echo=T)
# Generate SD plots and maps for OIMMP-only (for now)
if(analysis=="oimmp"){
  source("OIMMP_SupplementalFigures.R")
}

if(create_maps){
  source("Oyster_Create_Maps.R")
}

#Set output directory
out_dir <- "output"

#Sets the list of parameter names to cycle through. This can be edited to limit the number of parameters.
#Easiest way to edit is to comment out undesired parameters.
#If only one parameter is desired, comment out all other parameters and delete comma after remaining parameter
param_name <- "All_Oyster_Parameters"

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- SEACAR::ManagedAreas

# Load in figure captions
figureCaptions <- SEACAR::FigureCaptions

#Gets the files with the file names containing the desired parameter
file_in <- str_subset(list.files("C:/SEACAR Data/SEACARdata/", full.names = TRUE), "OYSTER")

#Gets the specific file used and removes the directory names
file_short <- str_split(file_in, "/")[[1]][4]

##### Generate Table Descriptions
# Load in oyster stats file (output from Oyster_ResultsCompile.R)
oy_stats_file_loc <- paste0("output/", analysis_col, "/Oyster_All_GLMM_Stats.txt")
oyster_stats <- fread(oy_stats_file_loc) %>% distinct() %>% as.data.table()
# Empty table to store results
descriptionTable <- data.table()
# Loop through available managed areas
for(ma in unique(oyster_stats[[analysis_col]])){
  if(analysis=="ma"){
    dataFile <- oyster_stats[ManagedAreaName==ma, ]
  } else {
    dataFile <- oyster_stats[OIMMP==ma, ]
  }
  # Save description in excel workbook
  descriptionText <- SEACAR::generate_description(data = dataFile, habitat = "Oyster")
  descriptionTable <- bind_rows(descriptionTable, descriptionText)
}

# Write .csv of text results
if(analysis=="oimmp"){
  descriptionTable <- descriptionTable %>% dplyr::rename(OIMMP = ManagedAreaName)
  fwrite(descriptionTable, file = "output/oyster_tableDescriptions_OIMMP.csv")
} else {
  fwrite(descriptionTable, file = "output/oyster_tableDescriptions.csv")
}

##### Render Reports
if(analysis=="ma"){
  file_out <-  "Oyster_AllParameters_Report"
  input_template <- "Oyster.Rmd"
} else if(analysis=="oimmp"){
  file_out <- "Oyster_OIMMP_AllParameters_Report"
  input_template <- "OIMMP_Template.Rmd"
}

for(file_type in c("PDF", "HTML")){
  descriptionColumn <- ifelse(file_type=="PDF", "DescriptionLatex", "DescriptionHTML")
  tableFormat <- ifelse(file_type=="PDF", "latex", "simple")
  rmarkdown::render(input = input_template, 
                    output_format = paste0(tolower(file_type),"_document"),
                    output_file = paste0(file_out, ".", tolower(file_type)),
                    output_dir = out_dir,
                    clean=TRUE)
}

#Removes unwanted files created in the rendering process
unlink(paste0(out_dir, "/", file_out, ".md"))
unlink(paste0(out_dir, "/", file_out, "_files"), recursive=TRUE)
unlink(paste0(file_out, ".log"))
