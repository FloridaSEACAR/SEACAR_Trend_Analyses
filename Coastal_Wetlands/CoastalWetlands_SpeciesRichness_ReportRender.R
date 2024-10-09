# This script renders .PDF reports using CoastalWetlands_SpeciesRichness.Rmd

#Load libraries
library(knitr)
library(readr)
library(dplyr)
library(data.table)
library(utils)
library(rstudioapi)
library(stringr)

# Gets directory of this script and sets it as the working directory
wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

# Call in CW_Plot_Render to render all plots, placing them in output folder
source("CW_Plot_Render.R")

#Set output directory
out_dir <- "output"

#Sets abbreviation or label to be used in file names
param_file <- "SpeciesRichness"

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
                na.strings = "")

#Renders CoastalWetlands_SpeciesRichness.Rmd and writes the report to a pdf and 
#Word document stored in output directory
file_out <-  paste0("CoastalWetlands_", param_file, "_Report")

rmarkdown::render(input = "CoastalWetlands_SpeciesRichness.Rmd", 
                  output_format = "pdf_document",
                  output_file = paste0(file_out, ".pdf"),
                  output_dir = out_dir,
                  clean=TRUE)

#Removes unwanted files created in the rendering process
unlink(paste0(out_dir, "/", file_out, ".md"))
unlink(paste0(out_dir, "/", file_out, "_files"), recursive=TRUE)