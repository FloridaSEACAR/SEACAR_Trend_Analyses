# The purpose of this file is to initiate all SAV analyses
# This includes model fitting, statistical summaries, plot & map generation
# Processing occurs in separate SAV scripts, those files are sourced here..
# Finally, a report is created containing all relevant SAV maps and plots
# Set the following options:

#######
# Create sample location maps? (for MA Report Generation & Atlas)
create_maps <- TRUE
# Render SAV report?
render_reports <- TRUE
# Choose whether to generate spatio-temporal scope plots for SAV locations
scope_plots <- FALSE
#######

# Set working directory
library(rstudioapi)
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

## Render reports

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
                na.strings = "")

# Declare CW File
files <- list.files(seacar_data_location, full.names = T)
file_in <- str_subset(files, "All_SAV")

# Short file name for display in report
file_short <- tail(str_split(file_in, "/")[[1]],1)

file_out <-  "SAV_Report"
if(render_reports){
  rmarkdown::render(input = "SAV_ReportSummary.Rmd", 
                    output_format = "pdf_document",
                    output_file = paste0(file_out, ".pdf"),
                    output_dir = "output",
                    clean=TRUE)
  #Removes unwanted files created in the rendering process
  unlink(paste0("output/", file_out, ".md"))
  unlink(paste0("output/", file_out, "_files"), recursive=TRUE)
}
