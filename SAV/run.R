# The purpose of this file is to initiate all SAV analyses
# This includes model fitting, statistical summaries, plot & map generation
# Processing occurs in separate SAV scripts, those files are sourced here..
# Finally, a report is created containing all relevant SAV maps and plots
# Set the following options:

#######
# Create sample location maps? (for MA Report Generation & Atlas)
create_maps <- FALSE
# Render SAV report?
render_reports <- TRUE
# Choose whether to generate spatio-temporal scope plots for SAV locations
scope_plots <- TRUE
#######

# Set working directory
library(rstudioapi)
library(SEACAR)
library(stringr)
library(data.table)
library(tidyverse)
wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

## Perform filtering operations and run SAV models, export summary stats, save plots
source("SAV_BB_script_website.R")

## SAV Map generation
# Maps are used within ManagedArea Reports
if(create_maps){
  source("SAV_Create_Maps.R", echo=TRUE)
}

## SAV temporal scope plot generation
# Source in external SAV_scope_plots.R to run scope plot generation
if(scope_plots){
  source("SAV_scope_plots.R", echo=TRUE)
}

##### Generate Table Descriptions
# Order species according to their order on the Atlas / plots
species_order <- c("Total SAV","Total seagrass","Halophila spp.","Halophila, unk.",
                   "Johnson's seagrass","Manatee grass","Paddle grass","Shoal grass",
                   "Star grass","Turtle grass","Widgeon grass","Attached algae","Drift algae")
# Import stats results (output from SAV_BBpct_LME_tableconvert.R)
sav_stats <- fread("output/website/SAV_BBpct_LMEresults_All.txt") %>% distinct() %>%
  mutate(Period = paste0(EarliestYear, " - ", LatestYear)) %>%
  filter(!Species=="No grass in quadrat") %>%
  mutate(Species = factor(Species, levels = species_order)) %>%
  arrange(ManagedAreaName, Species) %>% as.data.table()

# Lower-case species names, exception for acronym SAV
sav_stats[Species!="Total SAV", `:=` (Species = tolower(Species))]
sav_stats[Species=="Total SAV", `:=` (Species = "total SAV")]

# Empty table to store results
descriptionTable <- data.table()
# Loop through available managed areas
for(ma in unique(sav_stats$ManagedAreaName)){
  # Save description in excel workbook
  descriptionText <- generate_description(data = sav_stats[ManagedAreaName==ma, ], habitat = "SAV")
  descriptionTable <- bind_rows(descriptionTable, descriptionText)
}

# Write .csv of text results
fwrite(descriptionTable, file = "output/sav_tableDescriptions.csv")

## Render reports
#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- SEACAR::ManagedAreas

# Load in figure captions
figureCaptions <- SEACAR::FigureCaptions

# Declare SAV File
files <- list.files(seacar_data_location, full.names = T)
file_in <- str_subset(files, "All_SAV")

# Short file name for display in report
file_short <- tail(str_split(file_in, "/")[[1]],1)

file_out <-  "SAV_Report"
if(render_reports){
  for(file_type in c("PDF", "HTML")){
    descriptionColumn <- ifelse(file_type=="PDF", "DescriptionLatex", "DescriptionHTML")
    tableFormat <- ifelse(file_type=="PDF", "latex", "simple")
    rmarkdown::render(input = "SAV_ReportSummary.Rmd",
                      output_format = paste0(tolower(file_type),"_document"),
                      output_file = paste0(file_out, ".", tolower(file_type)),
                      output_dir = "output",
                      clean=TRUE)
  }
  #Removes unwanted files created in the rendering process
  unlink(paste0("output/", file_out, ".md"))
  unlink(paste0("output/", file_out, "_files"), recursive=TRUE)
}
