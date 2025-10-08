# Contains all SAV-related functions
# Generates LMEresults tables for use in report
library(mgcv)
# library(tidymv)
library(tidygam)
library(data.table)
library(dplyr)

source("../SAV/load_shape_files.R")

# SEACAR Figure standards
plot_theme <- SEACAR::SEACAR_plot_theme()

# Import SAV LME Stats
sav_stats_table <- fread("../SAV/output/website/SAV_BBpct_LMEresults_All.txt", sep='|')

#create Period of Record column (mirroring atlas)
sav_stats_table$years <- paste0(sav_stats_table$EarliestYear," - ",sav_stats_table$LatestYear)
sav_stats_table$years[sav_stats_table$SufficientData==FALSE] <- NA

sav_managed_areas <- unique(sav_stats_table$ManagedAreaName)

# Plot locations
trendplots <- list.files("../SAV/output/website/images/trendplots", full.names = T)
barplots <- list.files("../SAV/output/website/images/barplots", full.names = T)
multiplots <- list.files("../SAV/output/website/images/multiplots", full.names = T)
# Generate list of MAs for each plottype
trendplot_malist <- c()
for(pl in trendplots){
  ma_p <- str_split(pl, "_")[[1]][3]
  trendplot_malist <- append(trendplot_malist, ma_p)
}
multiplot_malist <- c()
for(pl in multiplots){
  ma_p <- str_split(pl, "_")[[1]][3]
  multiplot_malist <- append(multiplot_malist, ma_p)
}
barplot_malist <- c()
for(pl in barplots){
  ma_p <- str_split(pl, "_")[[1]][3]
  barplot_malist <- append(barplot_malist, ma_p)
}

# SAV LMEResults Table Function
# For use in report generation
sav_trend_table <- function(ma, report_format){
  format_type <- ifelse(report_type=="HTML", "simple", "latex")
  table <- sav_stats_table[ManagedAreaName == ma, c("Species","StatisticalTrend","years","LME_Intercept","LME_Slope","p")] %>%
    select(Species,StatisticalTrend,years,LME_Intercept,LME_Slope,p)
  
  # Grab relevant table description for a given plot
  desc <- TableDescriptions[ManagedAreaName==ma & HabitatName=="Submerged Aquatic Vegetation", get(descriptionColumn)]
  # Table title
  table_title <- paste0("Percent Cover Trend Analysis for ", ma)
  
  sav_kable <- table %>%
    kable(format=format_type,caption=table_title, booktabs = T, linesep = "",
          col.names = c("CommonName","Trend Significance (0.05)","Period of Record","LME-Intercept","LME-Slope","p")) %>%
    row_spec(0, italic=TRUE) %>%
    kable_styling(latex_options=c("scale_down","hold_position"))
  
  cat("  \n")
  print(sav_kable)
  cat("  \n")
  cat(desc)
  cat("  \n")
}

sav_scope_plots <- function(ma, ma_abrev, sav_scope_locs){
  file_loc <- str_subset(sav_scope_locs, paste0("_",ma_abrev,"_map"))
  if(length(file_loc)==0) return()
  caption <- paste0("Maps showing the temporal scope of SAV sampling sites within the boundaries of *", ma, "* by Program name.")
  cat("  \n")
  subchunkify(cat("![", caption, "](", file_loc,")"))
  # cat("![](", file_loc,")")
  cat("  \n")
  cat("Click [here](https://github.com/FloridaSEACAR/SEACAR_Trend_Analyses/tree/main/MA%20Report%20Generation/output/SAV-Temporal-Scope-Plots) to view spatio-temporal plots on GitHub.")
  cat("  \n")
}

#Managed areas that should have Halophila species combined:
ma_halspp <- c("Banana River Aquatic Preserve", "Indian River-Malabar to Vero Beach Aquatic Preserve", 
               "Indian River-Vero Beach to Ft. Pierce Aquatic Preserve", "Jensen Beach to Jupiter Inlet Aquatic Preserve",
               "Loxahatchee River-Lake Worth Creek Aquatic Preserve", "Mosquito Lagoon Aquatic Preserve", 
               "Biscayne Bay Aquatic Preserve", "Florida Keys National Marine Sanctuary")

plot_sav_trendplot <- function(ma, ma_abrev, plot_type, plot_list, malist){
  if(ma_abrev %in% malist){
    # Plot
    plot_loc <- str_subset(plot_list, paste0("_", ma_abrev, "_"))
    if(plot_type=="trendplots"){
      fig_caption <- paste0("Trends in median percent cover for various seagrass species in ", ma, " - simplified")
    } else if(plot_type=="multiplots"){
      fig_caption <- FigureCaptions[HabitatName=="Submerged Aquatic Vegetation" & IndicatorName=="Percent Cover", FigureCaptions]
    } else if(plot_type=="barplots"){
      fig_caption <- paste0("Frequency of occurrence for various seagrass species in ", ma)
    }
    
    cat("  \n")
    subchunkify(cat("![", fig_caption, "](", plot_loc,")"))
    # cat("![](", plot_loc,")")
    cat("  \n")
    
    # Table
    if(plot_type=="trendplots"){
      sav_trend_table(ma, report_format = report_format)
      cat("  \n")
    }
  }
}

sp_to_skip <- c("Drift algae", "Total seagrass", "Attached algae", "Total SAV", "No grass in quadrat")

ggplot_gam <- function(ma, hal = "all", pal = "Dark2") {
  
  data <- SAV4 %>% filter(ManagedAreaName==ma)
  
  if (nrow(data) > 0 ){
    if (hal == "combined"){
      species <- unique(data$analysisunit)
      au_col <- "analysisunit"
    } else if(hal == "only"){
      species <- str_subset(unique(data$analysisunit_halid), "Halophila")
      au_col <- "analysisunit_halid"
    } else if(hal == "none"){
      species <- str_subset(unique(data$analysisunit_halid), "Halophila", negate = TRUE)
      au_col <- "analysisunit_halid"
    } else {
      if(ma %in% ma_halspp){
        species <- unique(data$analysisunit)
        au_col <- "analysisunit"
      } else {
        species <- unique(data$analysisunit_halid)
        au_col <- "analysisunit_halid"
      }
    }
    
    min_years <- data %>% 
      group_by(!!sym(au_col)) %>% 
      summarise(n = n_distinct(Year)) %>% pull(n) %>% min()
    
    table_display <- data %>%
      group_by(!!sym(au_col)) %>%
      summarise(n = n_distinct(Year),
                YearRange = paste0(min(Year), " - ", max(Year)))
    
    # k_value <- ifelse(min_years > 2, min_years - 1, 2)
    k_value <- 3
    
    model_list <- list()
    
    for (i in 1:length(species)){
      s <- species[i]
      
      if (s %in% sp_to_skip){
        next
      } else {
        species_data <- data %>% filter(!!sym(au_col) == s, !is.na(BB_pct))
        # at least 10 years of data per species
        if (length(unique(species_data$Year)) >= 10){
          model_list[[s]] <- gam(BB_pct ~ s(relyear, k=k_value, fx = TRUE), data = species_data)
        }
      }
    }
    
    new_data <- expand.grid(relyear = seq(min(data$relyear), max(data$relyear), by = 1),
                            species = species)
    # model predict function
    get_predictions <- function(models, newdata) {
      preds <- lapply(names(models), function(sp) {
        pred_data <- newdata %>% filter(species == sp)
        pred <- predict.gam(models[[sp]], newdata=pred_data, type="link", se.fit=TRUE)
        data.frame(relyear=pred_data$relyear, species=sp, fit=pred$fit, lwr=pred$fit-1.96*pred$se.fit, upr=pred$fit+1.96*pred$se.fit)
      })
      
      bind_rows(preds)
    }
    
    predictions <- get_predictions(model_list, new_data)
    
    if (nrow(predictions) > 0){
      color_palette <- scale_color_manual(values = rainbow(length(unique(predictions$species))))
      
      # Scale x-axis data
      year_list <- data %>%
        filter(relyear %in% unique(predictions$relyear)) %>%
        group_by(relyear) %>%
        summarise(Year = list(unique(Year))) %>%
        unnest(Year)
      
      breaks_seq <- seq(from = min(year_list$relyear),
                        to = max(year_list$relyear),
                        by = 3)
      labels_seq <- seq(from = min(year_list$Year),
                        to = max(year_list$Year),
                        by = 3)
      
      plot <- ggplot(predictions, aes(x = relyear, y = fit, color = species)) +
        geom_ribbon(aes(ymin = lwr, ymax = upr, fill = species), alpha = 0.3, colour = NA) +
        geom_line() +
        labs(title = paste0("Median Percent Cover for seagrass species"),
             subtitle = ma,
             y = "Median Percent Cover",
             x = "Year") +
        scale_color_brewer(palette = pal, "Species") +
        scale_fill_brewer(palette = pal, "Species") +
        scale_x_continuous(breaks = breaks_seq, labels = labels_seq) +
        plot_theme
      
      print(plot)
      cat("  \n")
      cat(paste0("Generalized additive models for each species in ", ma, ". Species must have at least 10 years of data to be evaluated.  \n"))
      cat("  \n")
      cat("*Drift algae*, *Total seagrass*, *Attached algae*, *No grass in quadrat*, and *Total SAV* are excluded from the analyses.  \n")
      
      caption <- paste0("Amount of data for each species in ", ma)
      kable(table_display, format="simple", caption=caption, col.names= c("*Species*", "*Years of Data*", "*Year Range*"))
      cat("\n")
    }
  }
}

sav_maps <- function(ma, ma_abrev, map_locs, report_type){
  format_type <- ifelse(report_type=="HTML", "simple", "latex")
  # Programs with SAV data for a given MA
  sav_programs <- SAV4 %>% filter(ManagedAreaName == ma) %>% distinct(ProgramID, ProgramName)
  sav_programs$ProgramID <- as.numeric(sav_programs$ProgramID)
  
  map_loc <- str_subset(map_locs, paste0("_", ma_abrev, "_map.png"))
  if(length(map_loc)==0) return()
  # captions / label
  caption = paste0("Map showing SAV sampling sites within the boundaries of *", 
                   ma, "*. The point size reflects the number of samples at a given sampling site.  \n")
  
  cat("  \n")
  subchunkify(cat("![", caption, "](", map_loc,")"))
  cat("  \n")
  
  # SAV program data tables (for all programs)
  table_caption <- "Program Information for Submerged Aquatic Vegetation"
  sav_table <- SAV4 %>% 
    filter(ManagedAreaName==ma) %>% 
    group_by(method, ProgramID, ProgramName) %>% 
    summarise(N_Data = n(),
              YearMin = min(Year),
              YearMax = max(Year),
              "Sample Locations" = length(unique(ProgramLocationID))) %>%
    select(ProgramID, ProgramName, N_Data, YearMin, YearMax, method, 
           "Sample Locations") %>% 
    as.data.table()
  names(sav_table) <- gsub("_","-",names(sav_table))
  
  # Program name
  # SAV table prep for latex styling
  ma_sav_kable <- kable(sav_table %>% select(-ProgramName), format=format_type,
                        caption=table_caption,
                        row.names = FALSE, digits = 4,
                        booktabs = T, linesep = "", escape = F, longtable = F) %>%
    row_spec(0, italic=TRUE) %>%
    kableExtra::kable_styling(latex_options = c("scale_down", "HOLD_position"))
  # Display table
  print(ma_sav_kable)
  cat("  \n")
  # Display ProgramName below data table
  cat("\n **Program names:** \n \n")
  for(p_id in sort(unique(sav_table$ProgramID))){
    p_name <- sav_table[ProgramID==p_id, ]$ProgramName
    cat(paste0("*",p_id,"*", " - ",p_name, knitcitations::citep(bib[[paste0("SEACARID", p_id)]]), "  \n"))
  }
  cat("  \n")
}

# Function to locate which SAV_WC files are available for a given MA
# Provides a link to github to access reports
sav_wc <- list.files("../../SEACAR_Trend_Analyses/SAV_WC_Analysis/output/", ".png")
sav_wc_loc <- function(ma){
  ma_abrev <- MA_All[ManagedAreaName==ma, Abbreviation]
  # Find available plots
  avail <- str_subset(sav_wc, ma_abrev)
  if(length(avail)>0){
    # Extract parameter names
    params <- sapply(avail, function(x) str_split_1(x, "_")[[2]])
    # Rename parameters for cleaner display
    lookup <- c(CDOM = "Colored Disolved Organic Matter",
                Chla = "Chlorophyll a",
                DissolvedOxygen = "Dissolved Oxygen",
                DissolvedOxygenSaturation = "Dissolved Oxygen Saturation",
                pH = "pH", Salinity = "Salinity", Secchidepth = "Secchi Depth",
                Temperature = "Water Temperature", TN = "Total Nitrogen", 
                TSS = "Total Suspended Solids", Turbidity = "Turbidity")
    full_names <- lookup[params]
    
    cat("## SAV Water Column Analysis")
    cat("  \n")
    cat(glue("The following parameters are available for {ma} within the SAV_WC_Report:"))
    cat("  \n")
    cat("  \n")
    for(p in full_names){
      cat(paste0("* ", p, "\n"))
      cat("  \n")
    }
    cat("  \n")
    cat("Access the reports here: [DRAFT_SAV_WC_Report_2024-11-20.pdf](https://floridaseacar.github.io/SEACAR_Trend_Analyses/SAV_WC_Analysis/DRAFT_SAV_WC_Report_2024-11-20.pdf)")
    cat("  \n")    
  }
}
