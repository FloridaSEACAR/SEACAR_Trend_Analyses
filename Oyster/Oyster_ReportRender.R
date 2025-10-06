# The purpose of this script is to automate the production of Rmd documents for oyster analysis.
# Created by J.E. Panzik (jepanzik@usf.edu) for SEACAR

## WHEN RUNNING IN RSTUDIO:
## Set working directory to "Source File Location" in "Session" menu

## To ensure this script runs smoothly, please run in a fresh session of R
## Some other libraries cause this script not to work properly

## THIS SCRIPT WILL ONLY RUN WITH brms version 2.16.3 or lower. DOES NOT WORK WITH brms 2.17.0
# The above is no longer the case, now working with brms version 2.22.0

#Load libraries
library(knitr)
library(readr)
library(dplyr)
library(data.table)
library(rstudioapi)

# Gets directory of this script and sets it as the working directory
wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

# Determine whether to run Oyster EDA plots (T or F)
EDA <- FALSE

# Determine whether to generate Oyster sampling maps (for SEACAR Atlas)
create_maps <- TRUE

if(EDA){
  source("Oyster_EDA.R")
}

# Source in scripts to run Oyster analyses
# Oyster_Models_Clean_parallel.R to run all models and generate plots
source("Oyster_Models_parallel.R")
# Oyster_ResultsCompile.R to combine all results into single file (for Atlas)
source("Oyster_ResultsCompile.R", echo=T)

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

# Load in table descriptions
tableDesc <- SEACAR::TableDescriptions %>%
  mutate(DescriptionHTML = Description,
         DescriptionLatex = stringi::stri_replace_all_regex(
           Description,
           pattern = c("<i>", "</i>", "&#8805;"),
           replacement = c("*", "*", ">="),
           vectorize = FALSE
         )) %>%
  as.data.table()
# Load in figure captions
figureCaptions <- SEACAR::FigureCaptions

#Gets the files with the file names containing the desired parameter
file_in <- str_subset(list.files("C:/SEACAR Data/SEACARdata/", full.names = TRUE), "OYSTER")

#Gets the specific file used and removes the directory names
file_short <- str_split(file_in, "/")[[1]][4]

#Renders Oyster.Rmd for each parameter combination and writes the
#report to an html and Word document stored in output directory
file_out <-  paste0("Oyster_AllParameters_Report")

for(file_type in c("PDF", "HTML")){
  descriptionColumn <- ifelse(file_type=="PDF", "DescriptionLatex", "DescriptionHTML")
  tableFormat <- ifelse(file_type=="PDF", "latex", "simple")
  rmarkdown::render(input = "Oyster.Rmd", 
                    output_format = paste0(tolower(file_type),"_document"),
                    output_file = paste0(file_out, ".", tolower(file_type)),
                    output_dir = out_dir,
                    clean=TRUE)  
}

#Removes unwanted files created in the rendering process
unlink(paste0(out_dir, "/", file_out, ".md"))
unlink(paste0(out_dir, "/", file_out, "_files"), recursive=TRUE)
unlink(paste0(file_out, ".log"))
