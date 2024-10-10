#The purpose of this script is to render the percent cover analysis for Coral.


## WHEN RUNNING IN RSTUDIO:
## Set working directory to "Source File Location" in "Session" menu


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
out_dir <- "output/PercentCover"

# Sets coastal wetland file to only care about "All Parameters" file
param_name <- "PercentCover"

#Sets abbreviation or label to be used in file names
param_file <- "PC"

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
                na.strings = "")

#Gets the files with the file names containing the desired parameter
file_in <- list.files(seacar_data_location, pattern="All_CORAL", full=TRUE)

#Gets the specific file used and removes the directory names
file_short <- tail(str_split(file_in, "/")[[1]], 1)

#Renders Coral_PercentCover.Rmd and writes the report to a pdf and 
#Word document stored in output directory
file_out <-  paste0("Coral_", param_file, "_Report")

rmarkdown::render(input = "Coral_PC.Rmd", 
                  output_format = "pdf_document",
                  output_file = paste0(file_out, ".pdf"),
                  output_dir = out_dir,
                  clean=TRUE)
#Removes unwanted files created in the rendering process
unlink(paste0(out_dir, "/", file_out, ".md"))
unlink(paste0(out_dir, "/", file_out, "_files"), recursive=TRUE)