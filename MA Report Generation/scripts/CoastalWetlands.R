# Read in necessary outputs from CW habitat analysis
MA_Ov_Stats_cw <- fread("../Coastal_Wetlands/output/CoastalWetlands_SpeciesRichness_MA_Overall_Stats.txt")
MA_Ov_Stats_cw <- MA_Ov_Stats_cw[!is.na(MA_Ov_Stats_cw$EarliestYear), ]

MA_Y_Stats_cw <- fread("../Coastal_Wetlands/output/CoastalWetlands_SpeciesRichness_MA_Yr_Stats.txt")

#create global CoastalWetland MA_Include for use in ReportRender.R
cw_managed_areas <- unique(c(
  unique(MA_Ov_Stats_cw$ManagedAreaName),
  unique(MA_Y_Stats_cw$ManagedAreaName)
))

get_maps <- function(parameter, ma_abrev, type = "disc"){
  if(type %in% c("sav","nekton","cw")){
    pattern <- paste0(ma_abrev, "_map.png")
  } else {
    if(type %in% c("disc","cont")){
      param_short <- websiteParams[ParameterName==parameter, unique(ParameterShort)]
    } else {param_short <- parameter}
    pattern <- paste0(param_short, "_", ma_abrev, "_map.png") 
  }
  str_subset(get(paste0(type, "_maps")), pattern)
}

## Plotting function for use in ReportTemplate.Rmd ##
plot_cw <- function(ma, ma_abrev, MA_Ov_Stats = "MA_Ov_Stats_cw", MA_Y_Stats = "MA_Y_Stats_cw", cw_plots, report_type){
  format_type <- ifelse(report_type=="HTML", "simple", "latex")
  # Creates a data table object to be shown underneath plots in report
  ResultTable <- MA_Ov_Stats[ManagedAreaName==ma,]
  # Removes location, species group, and parameter information because it is
  # in plot labels
  ResultTable <- ResultTable[,-c("AreaID", "ManagedAreaName",
                                 "ProgramIDs", "Programs", "ParameterName")]
  # Renames StandardDeviation to StDev to save horizontal space
  ResultTable <- ResultTable %>%
    rename("StDev"="StandardDeviation")
  # Converts all non-integer values to 2 decimal places for space
  ResultTable$Min <- round(ResultTable$Min, digits=2)
  ResultTable$Max <- round(ResultTable$Max, digits=2)
  ResultTable$Median <- round(ResultTable$Median, digits=2)
  ResultTable$Mean <- round(ResultTable$Mean, digits=2)
  ResultTable$StDev <- round(ResultTable$StDev, digits=2)
  
  ResultTable <- ResultTable %>%
    mutate("Period of Record" = paste0(EarliestYear, " - ", LatestYear)) %>%
    select(SpeciesGroup1, N_Data, N_Years, "Period of Record", Median, Mean) %>%
    rename(
      "Species Group" = SpeciesGroup1,
      "Sample Count" = N_Data,
      "Number of Years" = N_Years,
      "Median N of Taxa" = Median,
      "Mean N of Taxa" = Mean
    )
  
  # Grab relevant table description for a given plot
  desc <- TableDescriptions[ManagedAreaName==ma & HabitatName=="Coastal Wetlands", get(descriptionColumn)]
  # Table title
  table_title <- "Coastal Wetlands Species Richness"
  
  # Convert results table into kable format
  result_table <- kable(ResultTable, format=format_type,
                        caption=table_title,
                        row.names = FALSE, digits = 5,
                        booktabs = T, linesep = "", escape = F, longtable = F) %>%
    row_spec(row = 0, italic = TRUE) %>%
    kable_styling(latex_options = c("scale_down", "HOLD_position"))
  # Locate plot
  plot_loc <- str_subset(cw_plots, paste0("_", ma_abrev, ".png"))
  
  # Print plots
  cat("  \n")
  fig_caption <- FigureCaptions[HabitatName=="Coastal Wetlands" & IndicatorName=="Species Composition", FigureCaptions]
  subchunkify(cat("![", fig_caption, "](", plot_loc,")"))
  
  cat("  \n")
  print(result_table)
  cat("  \n")
  cat(desc)
  cat("  \n")
  
  ### CW sample location maps
  # Locate map
  map_loc <- str_subset(cw_map_locs, paste0("_", ma_abrev, "_map"))
  # captions / label
  cat("  \n")
  cat("\\newpage")
  caption <- paste0("Map showing location of coastal wetlands sampling locations within the boundaries of *", ma, 
                    "*. The bubble size on the maps above reflect the amount of data available at each sampling site.  \n")
  cat("  \n")
  # Print map
  subchunkify(cat("![", caption, "](", map_loc,")"))
  cat("  \n")
}