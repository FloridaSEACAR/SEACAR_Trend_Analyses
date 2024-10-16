# This script renders .PDF reports using WC_ReportTemplate.Rmd
library(tictoc)
library(data.table)

# save_plots variable == TRUE will render plots within WC_Plot_Render, 
# save_plots variable == FALSE (only use if plots have already been created, to save time)
save_plots <- FALSE

tic()
# Render plots and generate necessary statistics files
source("WC_Plot_Render.R")
toc()

# Load files from .rds (if WC_Plot_Render.R has already been run and you wish to save time)
rds_output <- "output/rds/"
files_to_load <- c("skt_stats_cont", "skt_stats_disc", "data_output_disc",
                   "cont_plot_data", "managed_area_df")
for(file in files_to_load){
  eval(call("<-", as.name(file), readRDS(paste0(rds_output, file, ".rds"))))
}

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, 
                stringsAsFactors = FALSE, na.strings = "")

# Get list of managed areas to create reports for
all_managed_areas <- unique(managed_area_df$ManagedAreaName)

# Get list of managed areas with continuous WQ data
cont_managed_areas <- skt_stats_cont[!is.na(ProgramID), unique(ManagedAreaName)]

# Get list of available plot files created by WC_Plot_Render.R
cont_plots <- list.files("output/WQ_Continuous/", full.names = T)
disc_plots <- list.files("output/WQ_Discrete/", full.names = T)

tic()
# Loop through list of managed areas
for(ma in all_managed_areas[18:42]){
  print(ma)
  # determine which analyses to run for each MA
  # variables will be input into RMD file
  ma_df <- managed_area_df %>% filter(ManagedAreaName == ma)
  p_inc <- unique(ma_df$Parameter)
  d_inc <- unique(ma_df$Depth)
  a_inc <- unique(ma_df$Activity)
  
  discrete_data <- data_output_disc[ManagedAreaName==ma, ]
  skt_data <- skt_stats_disc[ManagedAreaName==ma, ]
  
  # Shortened names for managed areas
  ma_short <- MA_All[ManagedAreaName==ma, Abbreviation]
  # record region name
  region <- MA_All[ManagedAreaName==ma, Region]
  # output path for managed area reports
  output_path <- "output/Reports/"
  file_out <- paste0(ma_short,"_WC_Report")
  ### RENDERING ###
  rmarkdown::render(input = "WC_ReportTemplate.Rmd",
                    output_format = "pdf_document",
                    output_file = paste0(file_out, ".pdf"),
                    output_dir = output_path,
                    clean=TRUE)
  unlink(paste0(output_path, file_out, ".md"))
  unlink(paste0(output_path, file_out, ".tex"))
  unlink(paste0(output_path, file_out, "_files"), recursive=TRUE)
}
toc()
