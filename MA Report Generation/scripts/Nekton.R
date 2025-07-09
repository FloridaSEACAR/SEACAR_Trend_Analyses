library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(tidyr)
library(gridExtra)
library(ggpubr)
library(scales)

# Read in results file from Nekton analyses
MA_Ov_Stats_nek <- fread("../Nekton/output/Nekton_SpeciesRichness_MA_Overall_Stats.txt")
MA_Y_Stats_nek <- fread("../Nekton/output/Nekton_SpeciesRichness_MA_Yr_Stats.txt")
nekton_managed_areas <- unique(c(
  unique(MA_Ov_Stats_nek$ManagedAreaName),
  unique(MA_Y_Stats_nek$ManagedAreaName)
))

# Removes entries from the overall statistics that do not have data.
# Based on presence or absence of EarliestYear
MA_Ov_Stats_nek <- MA_Ov_Stats_nek[!is.na(MA_Ov_Stats_nek$EarliestYear), ]

plot_nekton <- function(ma, ma_abrev, MA_Y_Stats = "MA_Y_Stats_nek", MA_Ov_Stats = "MA_Ov_Stats_nek", nekton_plots, report_type){
  format_type <- ifelse(report_type=="HTML", "simple", "latex")
  # Creates a data table object to be shown underneath plots in report
  ResultTable <- MA_Ov_Stats[ManagedAreaName==ma,]
  
  # Removes location, gear, and parameter information because it is in plot
  # labels
  ResultTable <- ResultTable[,-c("AreaID", "ManagedAreaName",
                                 "ProgramIDs", "Programs", "GearType_Plot",
                                 "ParameterName")]
  # Renames StandardDeviation to StDev to save horizontal space
  ResultTable <- ResultTable %>%
    dplyr::rename("StDev"="StandardDeviation")
  # Converts all non-integer values to 2 decimal places for space
  ResultTable$Min <- round(ResultTable$Min, digits=2)
  ResultTable$Max <- round(ResultTable$Max, digits=2)
  ResultTable$Median <- round(ResultTable$Median, digits=2)
  ResultTable$Mean <- round(ResultTable$Mean, digits=2)
  ResultTable$StDev <- round(ResultTable$StDev, digits=2)
  
  ResultTable <- ResultTable %>% 
    mutate("Gear Type" = paste0(GearType, " (", GearSize_m, ")"),
           "Period of Record" = paste0(EarliestYear, " - ", LatestYear)) %>%
    rename("Sample Count" = N_Data,
           "Number of Years" = N_Years,
           "Median N of Taxa" = Median,
           "Mean N of Taxa" = Mean) %>%
    select("Gear Type", "Sample Count", "Number of Years", "Period of Record", 
           "Median N of Taxa", "Mean N of Taxa")
  
  # Grab relevant table description for a given plot
  desc <- TableDescriptions[ManagedAreaName==ma & IndicatorName=="Nekton", get(descriptionColumn)]
  # Table title
  table_title <- "Nekton Species Richness"
  
  # Convert results table into kable format
  result_table <- kable(ResultTable, format=format_type,
                        caption=table_title,
                        row.names = FALSE, digits = 5,
                        booktabs = T, linesep = "", escape = F, longtable = F) %>%
    row_spec(row = 0, italic = TRUE) %>%
    kable_styling(latex_options = c("scale_down", "HOLD_position"))
  # Locate plot
  plot_loc <- str_subset(nekton_plots, paste0("_", ma_abrev, ".png"))
  
  # Print plots
  cat("  \n")
  # fig_caption <- paste0("Figure for Nekton Species Richness in ", ma)
  fig_caption <- FigureCaptions[HabitatName=="Water Column" & IndicatorName=="Nekton", FigureCaptions]
  subchunkify(cat("![", fig_caption, "](", plot_loc,")"))
  # cat("![](", plot_loc,")")
  
  cat("  \n")
  print(result_table)
  cat("  \n")
  cat(desc)
  cat("  \n")
  
  ### Nekton sample location maps
  # Locate map
  map_loc <- str_subset(nekton_map_locs, paste0("_", ma_abrev, "_map"))
  # captions / label
  cat("  \n")
  cat("\\newpage")
  caption <- paste0("Map showing location of nekton sampling locations within the boundaries of *", ma, 
                    "*. The bubble size on the maps above reflect the amount of data available at each sampling site.  \n")
  cat("  \n")
  # Print map
  subchunkify(cat("![", caption, "](", map_loc,")"))
  cat("  \n")
}