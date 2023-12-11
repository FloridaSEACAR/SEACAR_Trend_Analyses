# Contains all SAV-related functions
# Generates LMEresults tables for use in report
library(mgcv)
library(tidymv)
library(data.table)
library(dplyr)

# SAV LMEResults Table Generation
# This script is designed to read the file names for the LME results of the BBpct analysis,
# import each one, extract the intercept, slope, and p values, produce them for display in reports

#List all of the files in the "tables" directory that are LME results
files <- list.files("output/tables/SAV", pattern="lmeresults", full.names=TRUE)

#Include only those that are BBpct
files <- files[grep("BBpct", files)]

#For loop cycles through each file name
for (i in 1:length(files)) {
  #Get filename from list
  filename <- files[i]
  
  #Read in file
  table <- readRDS(filename)
  
  #Keep only rows that are values with "fixed" in the effect column
  table <- table[table$effect=="fixed" & !is.na(table$effect),]
  
  #For each managed area and species, get the LME intercept, slope, and p values
  table <- table %>%
    group_by(managed_area, species) %>%
    summarise(LME_Intercept = estimate[term == "(Intercept)"],
              LME_Slope = estimate[term == "relyear"],
              p = p.value[term == "relyear"], .groups = "keep")
  
  #If this is the first file, the table from above is stored as the output table
  #If not the first file, the table is added to the end of the output table
  if(i==1) {
    output <- table
  } else {
    output <- bind_rows(output, table)
  }
}

#Add statistical trend column to denote where p<=0.05 and whether LME_slope increase or decreasing
output$StatisticalTrend <- ifelse(output$p <= 0.05 & output$LME_Slope > 0, "Significantly increasing trend",
                                  ifelse(output$p <= 0.05 & output$LME_Slope <0, "Significantly decreasing trend", "No significant trend"))

#Change column names to better match other outputs
output <- setnames(output, c("managed_area", "species"), c("ManagedAreaName", "Species"))

# round P-val, LME_slope and LME_intercept
output$p <- round(output$p,4)
output$LME_Intercept <- round(output$LME_Intercept,4)
output$LME_Slope <- round(output$LME_Slope,4)

#Loads data file with list on managed area names and corresponding area IDs and short names
# MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
#                 na.strings = "")

stats <- fread("output/Data/SAV/SAV_BBpct_Stats.txt", sep = "|", header = TRUE, stringsAsFactors = FALSE,
               na.strings = "")
setnames(stats, c("analysisunit"), c("Species"))

stats <-  merge.data.frame(stats, output,
                           by=c("ManagedAreaName", "Species"), all=TRUE)

stats <- merge.data.frame(MA_All[,c("AreaID", "ManagedAreaName")],
                          stats, by=c("ManagedAreaName"), all=TRUE)

stats <- as.data.table(stats[order(stats$ManagedAreaName, stats$Species), ])
stats <- stats %>% select(AreaID, everything())

stats$EarliestYear[stats$EarliestYear=="Inf"] <- NA
stats$LatestYear[stats$LatestYear=="-Inf"] <- NA

#filling remaining values in StatisticalTrend column
stats$StatisticalTrend[stats$SufficientData==FALSE] <- "Insufficient data to calculate trend"
stats$StatisticalTrend[stats$SufficientData==TRUE & is.na(stats$LME_Slope)] <- "Model did not fit the available data"

#drop rows where ManagedArea does not contain data
sav_stats_table <- stats[!apply(stats[, -c(1, 2), drop = FALSE], 1, function(row) all(is.na(row))), ]

#create Period of Record column (mirroring atlas)
sav_stats_table$years <- paste0(sav_stats_table$EarliestYear," - ",sav_stats_table$LatestYear)
sav_stats_table$years[sav_stats_table$SufficientData==FALSE] <- NA

#Write output table to a pipe-delimited txt file
# fwrite(stats, "output/tables/SAV/SAV_BBpct_LMEresults_All.txt", sep="|")

# SAV LMEResults Table Function
# For use in report generation
sav_trend_table <- function(ma){
  table <- sav_stats_table[ManagedAreaName == ma, c("Species","StatisticalTrend","years","LME_Intercept","LME_Slope","p")]
  
  caption <- paste0("Percent Cover Trend Analysis for ", ma)
  
  sav_kable <- table %>%
    kable(format="simple",caption=caption,
          col.names = c("*Species*","*Trend Significance* (0.05)","*Period of Record*","*LME_Intercept*","*LME_Slope*","*p*")) %>%
    kable_styling()
  
  print(sav_kable)
  cat("\n")
}

source("scripts/load_shape_files.R")

##############################
### SAV PLOTTING FUNCTIONS ###
##############################

#Managed areas that should have Halophila species combined:
ma_halspp <- c("Banana River Aquatic Preserve", "Indian River-Malabar to Vero Beach Aquatic Preserve", 
               "Indian River-Vero Beach to Ft. Pierce Aquatic Preserve", "Jensen Beach to Jupiter Inlet Aquatic Preserve",
               "Loxahatchee River-Lake Worth Creek Aquatic Preserve", "Mosquito Lagoon Aquatic Preserve", 
               "Biscayne Bay Aquatic Preserve", "Florida Keys National Marine Sanctuary")

# files <- list.files(here::here("output/Figures/BB/")) #get file list
files <- list.files("output/Figures/BB/", pattern = ".rds") #get file list
trendplots <- stringr::str_subset(files, "_trendplot") #identify map file
trendplots <- stringr::str_subset(trendplots, "_BBpct_")

# mods <- list.files(here::here("output/models/"))
mods <- list.files("output/models/")
models2 <- str_subset(mods, paste0(str_sub(trendplots[1], 1, str_locate_all(trendplots[1], "_")[[1]][2])))

malist <- c()
for(pl in trendplots){
  ma_p <- str_split(pl, "_")[[1]][3]
  malist <- append(malist, ma_p)
}

# failedmodslist <- readRDS(here::here("output/models/failedmodslist.rds"))
failedmodslist <- readRDS("output/models/failedmodslist.rds")

find_exact_matches <- function(pattern, filenames) {
  regex <- paste0("(_|^)", pattern, "(_|$)")
  matched_files <- str_subset(filenames, regex)
  return(matched_files)
}

plot_sav_trendplot <- function(ma,ma_abrev){
  if(ma_abrev %in% malist){
    plot_file <- lapply(ma_abrev, find_exact_matches, filenames = trendplots)
    plot <- readRDS(paste0("output/Figures/BB/", plot_file))
    print(plot)
    cat("  \n")
    
    #############
    sav_trend_table(ma)
    cat("  \n")
    #############
    
  }
}

barplots <- stringr::str_subset(files, "_barplot") #identify map file

malist2 <- c()
for(pl in barplots){
  ma_p <- str_split(pl, "_")[[1]][3]
  malist2 <- append(malist2, ma_p)
}


plot_sav_barplot <- function(ma_abrev){
  if(ma_abrev %in% malist2){
    plot_file <- lapply(ma_abrev, find_exact_matches, filenames = barplots)
    plot <- readRDS(paste0("output/Figures/BB/", plot_file))
    print(plot)
  }
}

sav_managed_areas <- unique(c(malist, malist2))

sp_to_skip <- c("Drift algae", "Total seagrass", "Attached algae", "Total SAV")

ggplot_gam <- function(ma, hal = "all") {
  
  SAV4 <- readRDS("output/Data/SAV/SAV4.rds")
  
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
    
    # k_value <- ifelse(min_years > 2, min_years - 1, 2)
    # print(k_value)
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
        geom_ribbon(aes(ymin = lwr, ymax = upr, fill = species), alpha = 0.2) + 
        # geom_line() +
        labs(title = paste0("BB_pct over Years for Seagrass Species in ", ma),
             y = "Braun-Blanquet Percentage",
             x = "Year") +
        color_palette +
        scale_fill_manual(values = rainbow(length(unique(predictions$species)))) +
        scale_x_continuous(breaks = breaks_seq, labels = labels_seq) +
        plot_theme
      
      print(plot)
    }
  }
  
  rm(SAV4)
}

sav_maps <- function(ma, ma_abrev){
  
  map_output <- "output/maps/"
  SAV4 <- readRDS("output/Data/SAV/SAV4.rds")
  locs_pts_rcp <- readRDS("output/Data/SAV/locs_pts_rcp.rds")
  locs_lns_rcp <- readRDS("output/Data/SAV/locs_lns_rcp.rds")
  
  # Grab a list of programs within SAV data for each MA
  sav_programs <- SAV4 %>% filter(ManagedAreaName == ma) %>% distinct(ProgramID)
  
  # grab sample coordinates from those programs
  coord_df <- locs_pts_rcp %>% filter(ProgramID %in% sav_programs$ProgramID)
  
  # frame to plot coordinates, allows for bubble size display of n_samples
  sav_df <- SAV4 %>% filter(ManagedAreaName == ma, ProgramID %in% sav_programs$ProgramID) %>%
    group_by(ProgramLocationID) %>%
    summarise(n_data = n()) %>%
    rename(ProgramLoc = ProgramLocationID)
  
  # merge frames together prior to plotting
  sav_df <- merge(sav_df, coord_df)
  
  # locate shape file for a given MA
  ma_shape <- find_shape(ma)
  
  # get coordinates to set zoom level
  shape_coordinates <- get_shape_coordinates(ma_shape)
  
  # setting color palette
  pal <- colorFactor("plasma", sav_df$ProgramID)
  
  # leaflet map
  map <- leaflet(sav_df, options = leafletOptions(zoomControl = FALSE,attributionControl=FALSE)) %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
    addPolygons(data=ma_shape, color="#4e809c", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.2) %>%
    addCircleMarkers(lat=~Latitude_D, lng=~Longitude_, color=~pal(ProgramID), weight=0.5, radius=sqrt(sav_df$n_data), fillOpacity=0.3) %>%
    addLegend(pal=pal, values=~ProgramID, labFormat=labelFormat(prefix="Program "), title="") %>%
    fitBounds(lng1=shape_coordinates$xmin,
              lat1=shape_coordinates$ymin,
              lng2=shape_coordinates$xmax,
              lat2=shape_coordinates$ymax)
  
  # map output filepath
  map_out <- paste0(map_output, ma_abrev, "_sav.png")
  
  # save file as png
  mapshot(map, file = map_out)
  
  # draw .png with ggplot
  p1 <- ggdraw() + draw_image(map_out, scale = 1)
  
  rm(SAV4, locs_pts_rcp, locs_lns_rcp)

  print(plot_grid(p1))
}

sav_scope_plots <- function(ma_abrev){
  scope_files <- list.files("output/Figures/BB/maps/", pattern = "_map_bypr.rds")
  
  ma_scope_file <- lapply(ma_abrev, find_exact_matches, filenames = scope_files)
  
  base <- readRDS(paste0("output/Figures/BB/maps/", ma_scope_file))

  print(base)
}