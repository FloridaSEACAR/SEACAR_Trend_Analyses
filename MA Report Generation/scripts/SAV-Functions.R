# Contains all SAV-related functions
# Generates LMEresults tables for use in report
library(mgcv)
library(tidymv)
library(tidygam)
library(data.table)
library(dplyr)

# SAV LMEResults Table Generation
# This script is designed to read the file names for the LME results of the BBpct analysis,
# import each one, extract the intercept, slope, and p values, produce them for display in reports
# originally from SAV_BBpct_LME_tableconvert.R

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
MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
                na.strings = "")

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

# Change Unidentified Halophila to Halophila, unk.
sav_stats_table[Species=="Unidentified Halophila", Species := "Halophila, unk."]

#Write output table to a pipe-delimited txt file
fwrite(sav_stats_table, "output/Data/SAV/SAV_BBpct_LMEresults_All.txt", sep="|")

# SAV LMEResults Table Function
# For use in report generation
sav_trend_table <- function(ma, table_format = "latex"){
  table <- sav_stats_table[ManagedAreaName == ma, c("Species","StatisticalTrend","years","LME_Intercept","LME_Slope","p")] %>%
    mutate(CommonName = modify_species_labels(Species, usenames="common")) %>%
    mutate(CommonName = ifelse(CommonName==Species, NA, CommonName)) %>%
    select(Species,CommonName,StatisticalTrend,years,LME_Intercept,LME_Slope,p)
  
  caption <- paste0("Percent Cover Trend Analysis for ", ma)
  
  sav_kable <- table %>%
    kable(format=table_format,caption=caption, booktabs = T, linesep = "",
          col.names = c("Species","CommonName","Trend Significance (0.05)","Period of Record","LME-Intercept","LME-Slope","p")) %>%
    row_spec(0, italic=TRUE) %>%
    kable_styling(latex_options=c("scale_down","HOLD_position"),
                  position = "center")
  
  if(table_format=="latex"){
    print(sav_kable)
  } else if(table_format=="html"){
    sav_kable
  }
}

# source(here::here("scripts/load_shape_files.R"))

##############################
### SAV PLOTTING FUNCTIONS ###
##############################

#Managed areas that should have Halophila species combined:
ma_halspp <- c("Banana River Aquatic Preserve", "Indian River-Malabar to Vero Beach Aquatic Preserve", 
               "Indian River-Vero Beach to Ft. Pierce Aquatic Preserve", "Jensen Beach to Jupiter Inlet Aquatic Preserve",
               "Loxahatchee River-Lake Worth Creek Aquatic Preserve", "Mosquito Lagoon Aquatic Preserve", 
               "Biscayne Bay Aquatic Preserve", "Florida Keys National Marine Sanctuary")

files <- list.files(here::here("output/Figures/BB/")) #get file list
trendplots <- stringr::str_subset(files, "_trendplot") #identify map file
trendplots <- stringr::str_subset(trendplots, "_BBpct_")

mods <- list.files(here::here("output/models/"))
models2 <- str_subset(mods, paste0(str_sub(trendplots[1], 1, str_locate_all(trendplots[1], "_")[[1]][2])))

malist <- c()
for(pl in trendplots){
  ma_p <- str_split(pl, "_")[[1]][3]
  malist <- append(malist, ma_p)
}

failedmodslist <- readRDS(here::here("output/models/failedmodslist.rds"))

find_exact_matches <- function(pattern, filenames) {
  regex <- paste0("(_|^)", pattern, "(_|$)")
  matched_files <- str_subset(filenames, regex)
  return(matched_files)
}

plot_sav_trendplot <- function(ma,ma_abrev,type){
  if(ma_abrev %in% malist){
    plot_file <- lapply(ma_abrev, find_exact_matches, filenames = trendplots)
    plot <- readRDS(here::here(paste0("output/Figures/BB/", plot_file)))
    print(plot)
    cat("  \n")

    #############
    table_format <- ifelse(type=="PDF", "latex", "html")
    sav_trend_table(ma, table_format = table_format)
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
    plot <- readRDS(here::here(paste0("output/Figures/BB/", plot_file)))
    print(plot)
    cat("  \n")
  }
}

# Multiplots

multiplots <- str_subset(files, "_multiplot")

multiplot_list <- c()
for(pl in multiplots){
  ma_p <- str_split(pl, "_")[[1]][3]
  multiplot_list <- append(multiplot_list, ma_p)
}

plot_sav_multiplot <- function(ma, ma_abrev){
  if(ma_abrev %in% multiplot_list){
    plot_file <- unlist(lapply(ma_abrev, find_exact_matches, filenames = multiplots))
    if(length(plot_file)>1){plot_file <- str_subset(plot_file, "_BBpct_")}
    plot <- readRDS(here::here(paste0("output/Figures/BB/", plot_file)))
    caption <- paste0("Median percent cover by species in *", ma, "*. Linear mixed-effects models are applied to each species to produce species trends. The trendlines are then isolated and reproduced below for ease of viewing. The LME results are available in table form beneath the supplemental trendplot below.")
    cat("  \n")
    print(plot)
    cat("  \n")
    cat(caption)
  }
}

sp_to_skip <- c("Drift algae", "Total seagrass", "Attached algae", "Total SAV", "No grass in Quadrat")

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
      cat("*Drift algae*, *Total seagrass*, *Attached algae*, *No grass in Quadrat*, and *Total SAV* are excluded from the analyses.  \n")
      
      caption <- paste0("Amount of data for each species in ", ma)
      kable(table_display, format="simple", caption=caption, col.names= c("*Species*", "*Years of Data*", "*Year Range*"))
      cat("\n")
    }
  }
}

sav_maps <- function(ma, ma_abrev){
  
  map_output <- "output/maps/"
  
  # Grab a list of programs within SAV data for each MA
  sav_programs <- SAV4 %>% filter(ManagedAreaName == ma) %>% distinct(ProgramID, ProgramName)
  sav_programs$ProgramID <- as.numeric(sav_programs$ProgramID)
  
  # grab sample coordinates from those programs
  pt_coord_df <- locs_pts_rcp %>% filter(ProgramID %in% sav_programs$ProgramID)
  ln_coord_df <- locs_lns_rcp %>% filter(ProgramID %in% sav_programs$ProgramID)
  
  # frame to plot coordinates, allows for bubble size display of n_samples
  # grouping by LocationID yields better results, PLID doesn't always match (BBAP)
  sav_df <- SAV4 %>% filter(ManagedAreaName == ma, ProgramID %in% sav_programs$ProgramID) %>%
    group_by(LocationID) %>%
    summarise(n_data = n())
  
  # sav_df <- SAV4 %>% filter(ManagedAreaName == ma, ProgramID %in% sav_programs$ProgramID) %>%
  #   group_by(ProgramLocationID) %>%
  #   summarise(n_data = n()) %>%
  #   rename(ProgramLoc = ProgramLocationID)
  
  pt_ln_df <- bind_rows(pt_coord_df, ln_coord_df)
  
  sav_df <- merge(sav_df, pt_ln_df)
  sav_df <- sav_df[order(sav_df$n_data, decreasing=TRUE), ]
  
  # locate shape file for a given MA
  ma_shape <- find_shape(ma)
  
  # get coordinates to set zoom level
  shape_coordinates <- get_shape_coordinates(ma_shape)
  
  # color palette set up to match coloring on SAV_Scope_plots
  color_values <- subset(prcols, names(prcols) %in% unique(sav_df$ProgramName))
  # rename list names as ProgramID instead of ProgramName (display ID in map legend)
  # names(color_values) <- sapply(
  #   names(color_values), 
  #   function(x){sav_df %>% filter(ProgramName==x) %>% distinct(ProgramID)})
  
  # setting color palette
  pal <- colorFactor(palette = color_values, levels = names(color_values))
  
  # create empty map template with shape file
  # previous shape col - #4E809C
  map <- leaflet(sav_df, options = leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
    addPolygons(data=ma_shape, color="black", weight = 1, smoothFactor = 0.5, opacity = 0.8, fillOpacity = 0.1) %>%
    addLegend(pal=pal, values=~ProgramName, labFormat=labelFormat(prefix=""), title="") %>%
    fitBounds(lng1=shape_coordinates$xmin,
              lat1=shape_coordinates$ymin,
              lng2=shape_coordinates$xmax,
              lat2=shape_coordinates$ymax)
  
  # set sav_df as SF geo-object
  sav_df <- st_as_sf(sav_df)
  
  # subsetting for lines vs points (coordinate vs transect)
  pts <- sav_df %>% filter(!is.na(Longitude_))
  lns <- sav_df %>% filter(!is.na(RawLineStr))
  
  # add transects and points where available
  if(nrow(pts)>0){
    # set pt-size weighting (some MAs have large amounts of samples)
    pt_weight_setting <- ifelse(mean(pts$n_data)>40, 3, 1)
    map <- map %>%
      addCircleMarkers(data = pts,
                       lat=~Latitude_D, lng=~Longitude_,
                       color=~pal(pts$ProgramName), weight=0.5, 
                       radius=sqrt(pts$n_data)/pt_weight_setting, 
                       fillOpacity=0.6)
  }
  
  if(nrow(lns)>0){
    # set ln-size weighting
    ln_weight_setting <- ifelse(sqrt(mean(lns$n_data))>100, 10, 
                                ifelse(sqrt(mean(lns$n_data))>20, 2, 1))
    map <- map %>%
      addPolylines(data = lns,
                   weight = sqrt(lns$n_data)/ln_weight_setting,
                   color = ~pal(lns$ProgramName),smoothFactor = 0.5,
                   stroke = TRUE, opacity = 0.6)
  }
  
  # map output filepath
  map_out <- paste0(map_output, ma_abrev, "_sav.png")
  
  # save file as png
  mapshot(map, file = map_out)
  
  # draw .png with ggplot
  p1 <- ggdraw() + draw_image(map_out, scale = 1)
  
  # captions / label
  caption = paste0("Map showing SAV sampling sites within the boundaries of *", 
                   ma, "*. The point size reflects the number of samples at a given sampling site.  \n")
  
  print(p1)
  cat("  \n")
  cat(caption)
  cat("  \n")
  
  # SAV program data tables
  cat("  \n")
  
  for (p_id in sav_programs$ProgramID){
    
    p_name <- sav_programs[ProgramID==p_id, ]$ProgramName
    
    caption <- paste0(p_name, " - *Program ", p_id,"*")
    
    ma_sav <- SAV4 %>% filter(ManagedAreaName==ma, ProgramID==p_id) %>% 
      group_by(method) %>% 
      summarise(N_Data = n(),
                YearMin = min(Year),
                YearMax = max(Year),
                "Sample Locations" = length(unique(ProgramLocationID))) %>%
      select(N_Data, YearMin, YearMax, method, "Sample Locations") %>%
      kable(format="simple", caption=caption, col.names = c("*N_Data*","*YearMin*","*YearMax*","*Collection Method*","*Sample Locations*")) %>%
      kable_styling()
    
    print(ma_sav)
  }
}

sav_scope_plots <- function(ma, ma_abrev){

  scope_files <- list.files(here::here("output/Figures/BB/maps"))
  
  ma_scope_file <- lapply(ma_abrev, find_exact_matches, filenames = scope_files)
  
  base <- readRDS(paste0("output/Figures/BB/maps/",ma_scope_file))
  
  print(base)
  
  cat("  \n")
  caption <- paste0("Maps showing the temporal scope of SAV sampling sites within the boundaries of *", ma, "* by Program name.")
  cat(caption)
  cat("  \n")
}