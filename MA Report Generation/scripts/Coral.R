# Read in necessary outputs from CW habitat analysis
# Percent Cover
MA_Ov_Stats_pc <- fread("../Coral/output/PercentCover/Coral_PC_MA_Overall_Stats.txt")
MA_Ov_Stats_pc <- MA_Ov_Stats_pc[!is.na(MA_Ov_Stats_pc$EarliestYear), ]
lme_plot_pc <- fread("../Coral/output/PercentCover/Coral_PC_LME_Stats.txt")

# Species Richness
MA_Y_Stats_sr <- fread("../Coral/output/SpeciesRichness/Coral_SpeciesRichness_MA_Yr_Stats.txt")
MA_Ov_Stats_sr <- fread("../Coral/output/SpeciesRichness/Coral_SpeciesRichness_MA_Overall_Stats.txt")

#create global CoastalWetland MA_Include for use in ReportRender.R
cw_managed_areas <- unique(c(
  unique(MA_Ov_Stats_cw$ManagedAreaName),
  unique(MA_Y_Stats_cw$ManagedAreaName)
))

# Managed areas containing Coral
coral_pc_MA_Include <- unique(MA_Ov_Stats_pc[SufficientData==TRUE, ]$ManagedAreaName)
coral_sr_MA_Include <- unique(MA_Ov_Stats_sr$ManagedAreaName)
coral_managed_areas <- unique(c(coral_pc_MA_Include, coral_sr_MA_Include))

plot_coral_pc <- function(ma, ma_abrev, lme_plot = lme_plot_pc, coral_pc_plots, report_type){
  format_type <- ifelse(report_type=="HTML", "simple", "latex")
  # Creates a data table object to be shown underneath plots in report
  ResultTable <- lme_plot[ManagedAreaName==ma,]
  ResultTable <- ResultTable %>% rowwise() %>%
    mutate(`Statistical Trend` = checkTrends(p = LME_p, Slope = LME_Slope, 
                                             SufficientData = SufficientData),
           `Period of Record` = paste0(EarliestYear, " - ", LatestYear)) %>% 
    ungroup() %>% rename(`LME Intercept` = LME_Intercept, `LME Slope` = LME_Slope, 
                         `p` = LME_p) %>% 
    select(`Statistical Trend`, `Period of Record`, 
           `LME Intercept`, `LME Slope`, p) %>% as.data.table()
  
  # Convert for kable/latex format (no "_" allowed)
  names(ResultTable) <- gsub("_", "-", names(ResultTable))
  
  # Grab relevant table description for a given plot
  desc <- TableDescriptions[ManagedAreaName==ma & HabitatName=="Coral/Coral Reef" & ParameterName=="Percent Cover", get(descriptionColumn)]
  # Table title
  table_title <- "Coral Percent Cover"
  
  # Convert results table into kable format
  result_table <- kable(ResultTable, format=format_type,
                        caption=table_title,
                        row.names = FALSE, digits = 5,
                        booktabs = T, linesep = "", escape = F, longtable = F) %>%
    kable_styling(latex_options = c("scale_down", "HOLD_position"))
  # Locate plot
  plot_loc <- str_subset(coral_pc_plots, paste0("_", ma_abrev, ".png"))
  
  # Print plots
  cat("  \n")
  # fig_caption <- paste0("Figure for Coral Percent Cover in ", ma)
  fig_caption <- FigureCaptions[HabitatName=="Coral/Coral Reef" & IndicatorName=="Percent Cover", FigureCaptions]
  subchunkify(cat("![", fig_caption, "](", plot_loc,")"))
  # cat("![](", plot_loc,")")
  
  cat("  \n")  
  print(result_table)
  cat("  \n")
  cat(desc)
  cat("  \n")
  
  ### Coral sample location maps
  # Locate map
  map_loc <- str_subset(coral_map_locs, paste0("_PC_", ma_abrev, "_map"))
  # captions / label
  cat("  \n")
  cat("\\newpage")
  caption <- paste0("Map showing location of coral percent cover sampling locations within the boundaries of *", ma, 
                    "*. The bubble size on the maps above reflect the amount of data available at each sampling site.  \n")
  cat("  \n")
  # Print map
  subchunkify(cat("![", caption, "](", map_loc,")"))
  cat("  \n")
}

plot_coral_sr <- function(ma, ma_abrev, MA_Ov_Stats = MA_Ov_Stats_sr, coral_sr_plots, report_type){
  format_type <- ifelse(report_type=="HTML", "simple", "latex")
  # Creates a data table object to be shown underneath plots in report
  ResultTable <- MA_Ov_Stats[ManagedAreaName==ma,]
  ResultTable <- ResultTable %>%
    mutate(`Period of Record` = paste0(EarliestYear, " - ", LatestYear)) %>%
    rename(`Sample Count` = N_Data, `Number of Years` = N_Years, 
           `Median N of Taxa` = Median, `Mean N of Taxa` = Mean) %>% 
    select(`Sample Count`, `Number of Years`, `Period of Record`, 
           `Median N of Taxa`, `Mean N of Taxa`) %>% as.data.table()
  
  # Convert for kable/latex format (no "_" allowed)
  names(ResultTable) <- gsub("_", "-", names(ResultTable))
  
  # Grab relevant table description for a given plot
  desc <- TableDescriptions[ManagedAreaName==ma & HabitatName=="Coral/Coral Reef" & ParameterName=="Presence/Absence", get(descriptionColumn)]
  # Table title
  table_title <- "Coral Species Richness"
  
  # Convert results table into kable format
  result_table <- kable(ResultTable, format=format_type,
                        caption="Coral Species Richness",
                        row.names = FALSE, digits = 5,
                        booktabs = T, linesep = "", escape = F, longtable = F) %>%
    row_spec(row = 0, italic = TRUE) %>%
    kable_styling(latex_options = c("scale_down", "HOLD_position"))
  # Locate plot
  plot_loc <- str_subset(coral_sr_plots, paste0("_", ma_abrev, ".png"))
  
  # Print plots
  cat("  \n")
  # fig_caption <- paste0("Figure for Coral Species Richness in ", ma)
  fig_caption <- FigureCaptions[HabitatName=="Coral/Coral Reef" & IndicatorName=="Grazers and Reef Dependent Species", FigureCaptions]
  subchunkify(cat("![", fig_caption, "](", plot_loc,")"))
  # cat("![](", plot_loc,")")
  cat("  \n")
  print(result_table)
  cat("  \n")
  cat(desc)
  cat("  \n")
  
  ### Coral sample location maps
  # Locate map
  map_loc <- str_subset(coral_map_locs, paste0("_SpeciesRichness_", ma_abrev, "_map"))
  # captions / label
  cat("  \n")
  cat("\\newpage")
  caption <- paste0("Map showing location of coral species richness sampling locations within the boundaries of *", ma, 
                    "*. The bubble size on the maps above reflect the amount of data available at each sampling site.  \n")
  cat("  \n")
  # Print map
  subchunkify(cat("![", caption, "](", map_loc,")"))
  cat("  \n")
}