# Read in oyster results file
oyster_stats <- fread("../Oyster/output/Oyster_All_GLMM_Stats.txt", sep='|')

# Function to add trend text
trendText <- function(modelEstimate, lowConfidence, upConfidence){
  increasing <- modelEstimate > 0
  trendPresent <- ifelse(lowConfidence < 0 & upConfidence < 0, TRUE, 
                         ifelse(lowConfidence > 0 & upConfidence > 0, TRUE, FALSE))
  trendStatus <- "No significant change"
  if(trendPresent){
    trendDirection <- ifelse(increasing, "increasing", "decreasing")
    trendStatus <- paste0("Significantly ", trendDirection, " trend")
  }
  return(trendStatus)
}
# Apply function, add credible interval
oyster_stats <- oyster_stats %>% rowwise() %>%
  mutate(
    "Trend Status" = ifelse(!is.na(ModelEstimate), trendText(ModelEstimate, LowerConfidence, UpperConfidence), NA),
    "Credible Interval" = ifelse(!is.na(LowerConfidence), paste0(round(LowerConfidence, 2), " to ", round(UpperConfidence, 2)), NA)
  ) %>%
  ungroup() %>% as.data.table()

# List of managed areas with oyster plots
oyster_managed_areas <- oyster_stats[!is.na(Intercept), unique(ManagedAreaName)]

# Oyster plot locations
oy_figs <- list(
  "Density" = list.files("../Oyster/output/Density/Figures", pattern = ".png", full.names = T),
  "Percent Live" = list.files("../Oyster/output/Percent_Live/Figures", pattern = ".png", full.names = T),
  "Shell Height" = list.files("../Oyster/output/Shell_Height/Figures", pattern = ".png", full.names = T)
)

# Dataframe to determine which figures are included
oyster_includes <- oyster_stats %>% 
  group_by(ManagedAreaName, ParameterName, SufficientData, HabitatType, Intercept) %>%
  filter(SufficientData, !is.na(Intercept)) %>%
  summarise(.groups = "keep") %>% as.data.table()

oy_file_loc <- function(ma_abrev, p, type){
  if(p=="Density"){
    pattern <- paste0("_GLMM_", ma_abrev,"_",type,"_raw.png")
  } else if(p=="Percent Live"){
    pattern <- paste0("_GLMM_", ma_abrev,"_",type,"_raw.png")
  } else if(p=="Shell Height"){
    pattern <- paste0("_GLMM_", ma_abrev, "_", type, ".png")
  }
  return(str_subset(oy_figs[[p]], pattern))
}

oyster_tables <- function(ma, p, type, format_type){
  subset <- oyster_stats[ManagedAreaName==ma & ParameterName==p & HabitatType==type,]
  if(p == "Shell Height"){
    select_cols <- c("ShellType", "SizeClass", "HabitatType", "Trend Status", "ModelEstimate", "StandardError", 
                     "Credible Interval")
  } else {
    select_cols <- c("ShellType", "HabitatType", "Trend Status", "ModelEstimate", "StandardError", 
                     "Credible Interval")
  }
  ResultTable <- subset %>% 
    select(select_cols) %>%
    rename("Shell Type" = "ShellType",
           "Habitat Type" = "HabitatType",
           "Estimate" = "ModelEstimate",
           "Standard Error" = "StandardError")
  
  # Grab relevant table description for a given plot
  desc <- TableDescriptions[ManagedAreaName==ma & HabitatName=="Oyster/Oyster Reef" & ParameterName==p, get(descriptionColumn)]
  # Table title
  table_title <- paste0("Model results for Oyster ", p, " - ", type)
  
  result_table <- kable(ResultTable, format = format_type,
                        caption = table_title,
                        row.names = FALSE, digits = 2,
                        booktabs = T, linesep = "", escape = F, longtable = F) %>%
    row_spec(row = 0, italic = TRUE) %>%
    kable_styling(latex_options = c("scale_down", "HOLD_position"))
  
  cat("  \n")  
  print(result_table)
  cat("  \n")
  cat(desc)
  cat("  \n")
}

plot_oyster <- function(ma, ma_abrev, oyster_includes, oy_figs, report_type){
  format_type <- ifelse(report_type=="HTML", "simple", "latex")
  subset <- oyster_includes[ManagedAreaName==ma, ]
  
  for(p in unique(subset$ParameterName)){
    # parameter subset
    p_subset <- subset[ParameterName==p, ]
    
    cat("  \n")
    cat(glue("## {p}"))
    cat("  \n")
    fig_caption <- FigureCaptions[HabitatName=="Oyster/Oyster Reef" & IndicatorName==p, FigureCaptions]
    
    hab_types <- unique(p_subset$HabitatType)
    for(type in hab_types){
      cat("  \n")
      cat(glue("### {type}"))
      cat("  \n")
      plot_loc <- oy_file_loc(ma_abrev, p, type)
      # fig_caption <- paste0("Figure for Oyster ", p, " in ", ma)
      subchunkify(cat("![", fig_caption, "](", plot_loc,")"))
      # cat("![](", plot_loc,")")
      cat("  \n")
      oyster_tables(ma, p, type, format_type = format_type)
      cat("  \n")
    }
    
    ### Oyster sample location maps
    # Short-hand parameter name to locate correct map
    p_short <- ifelse(p=="Shell Height", "SH", ifelse(p=="Density", "Dens", "PrcLive"))
    # Locate map
    map_loc <- str_subset(oyster_map_locs, paste0("_", p_short, "_", ma_abrev, "_map"))
    # captions / label
    cat("  \n")
    cat("\\newpage")
    caption <- paste0("Map showing location of oyster ", tolower(p), " sampling locations within the boundaries of *", ma, 
                      "*. The bubble size on the maps above reflect the amount of data available at each sampling site.  \n")
    cat("  \n")
    # Print map
    subchunkify(cat("![", caption, "](", map_loc,")"))
    cat("  \n")
  }
}
