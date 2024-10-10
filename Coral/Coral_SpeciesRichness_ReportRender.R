#The purpose of this script is to render the report for Coral species richness.
#Load libraries
library(knitr)
library(readr)
library(dplyr)
library(data.table)
library(utils)
library(rstudioapi)

# Gets directory of this script and sets it as the working directory
wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

source("../SEACAR_data_location.R")

#Set output directory
out_dir <- "output/SpeciesRichness"

# Sets coral file to only care about "SpeciesRichness" files
param_name <- "SpeciesRichness"

#Sets abbreviation or label to be used in file names
param_file <- "SpeciesRichness"

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
                na.strings = "")

#Gets the files with the file names containing the desired parameter
file_in <- list.files(seacar_data_location, pattern="All_CORAL", full=TRUE)

#Gets the specific file used and removes the directory names
file_short <- tail(str_split(file_in, "/")[[1]], 1)

#Renders Coral_SpeciesRichness.Rmd and writes the report to a pdf and 
#Word document stored in output directory
file_out <-  paste0("Coral_", param_file, "_Report")

rmarkdown::render(input = "Coral_SpeciesRichness.Rmd", 
                  output_format = "pdf_document",
                  output_file = paste0(file_out, ".pdf"),
                  output_dir = out_dir,
                  clean=TRUE)

#Removes unwanted files created in the rendering process
unlink(paste0(out_dir, "/", file_out, ".md"))
unlink(paste0(out_dir, "/", file_out, "_files"), recursive=TRUE)
