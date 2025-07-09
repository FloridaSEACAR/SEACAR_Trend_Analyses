cont_rds_loc <- "../WQ_Cont_Discrete/output/tables/cont/"

# Cont. Data exports do not contain full parameter names or units
# Create dataframe containing that info
cont_params_long <- c("Dissolved Oxygen","Dissolved Oxygen Saturation","pH",
                      "Salinity","Turbidity","Water Temperature")
cont_params_short <- c("DO","DOS","pH","Sal","Turb","TempW")
cont_param_units <- c("mg/L","%","pH","ppt","NTU","Degrees C")
cont_regions <- c("NE","NW","SE","SW")

cont_param_df <- data.table(param_short = cont_params_short,
                            parameter = cont_params_long,
                            unit = cont_param_units)

#################
### FUNCTIONS ###
#################

# For loading continuous data
# Load Data Table Function
load_cont_data_table <- function(param, region, table) {
  
  # Declaring RDS file list of respective tables
  files <- list.files(cont_rds_loc,pattern = "\\.rds$", full.names=T)
  file_path <- paste0("_",param,"_", region,"_", table) 
  
  # subset file list to select desired table RDS file
  table_file <- paste0(str_subset(files, file_path))
  
  # importing RDS files
  df <- readRDS(table_file)
  
  return(df)
}

# Station coordinates
coordinates_df <- list()

for (p in cont_params_short){
  for (region in cont_regions){
    # coordinates table
    df <- load_cont_data_table(p, region, "Station_Coordinates")
    coordinates_df <- bind_rows(coordinates_df, df)
  }
}
rm(df)

# add 1 to years_of_data (result of subtracting years)
coordinates_df$years_of_data <- coordinates_df$years_of_data + 1

station_coordinates <- setDT(coordinates_df %>% group_by(ManagedAreaName, lat, lon) %>%
  distinct(ProgramLocationID))

coordinates_df$ManagedAreaName[coordinates_df$ManagedAreaName=="St. Andrews State Park Aquatic Preserve"] <- "St. Andrews Aquatic Preserve"
coordinates_df$ManagedAreaName[coordinates_df$ManagedAreaName=="Southeast Florida Coral Reef Ecosystem Conservation Area"] <- "Kristin Jacobs Coral Aquatic Preserve"
station_coordinates$ManagedAreaName[station_coordinates$ManagedAreaName=="St. Andrews State Park Aquatic Preserve"] <- "St. Andrews Aquatic Preserve"
station_coordinates$ManagedAreaName[station_coordinates$ManagedAreaName=="Southeast Florida Coral Reef Ecosystem Conservation Area"] <- "Kristin Jacobs Coral Aquatic Preserve"

cont_managed_areas <- unique(station_coordinates$ManagedAreaName)

# Provides a table for stations with Cont. Data
# and which stations passed the tests
station_count_table <- function(coordinates_df, station_coordinates, ma, ma_abrev, report_type){
  format_type <- ifelse(report_type=="HTML", "simple", "latex")
  cat("  \n")
  cat("  \n")
  cat(paste0("**Continuous monitoring locations in ", ma, "**"))
  
  stations <- coordinates_df[ManagedAreaName == ma, .(ProgramLocationID, ProgramID, ProgramName, Use_In_Analysis)]
  
  programs_by_ma <- unique(stations$ProgramID)
  
  n_years <- coordinates_df %>% 
    filter(ManagedAreaName==ma) %>%
    group_by(ProgramID, ProgramName, ProgramLocationID, years_of_data, Use_In_Analysis) %>%
    summarise(Parameters = list(Parameter)) %>%
    arrange(ProgramID, ProgramLocationID) %>%
    rename("Years of Data" = "years_of_data","Use in Analysis" = "Use_In_Analysis") %>%
    ungroup()
  
  caption <- "Station overview for Continuous parameters by Program"
  
  station_kable <- kable(n_years %>% select(-ProgramName), 
                         format=format_type,caption=caption, booktabs = T, linesep = "") %>%
    row_spec(0, italic=TRUE) %>%
    kable_styling(latex_options=c("scale_down","HOLD_position"))
  
  print(station_kable)
  cat("\n")
  # Display ProgramName below data table
  cat("\n **Program names:** \n \n")
  for(p_id in sort(unique(n_years$ProgramID))){
    p_name <- unique(n_years[n_years$ProgramID==p_id, ]$ProgramName)
    cat(paste0("*",p_id,"*", " - ",p_name, knitcitations::citep(bib[[paste0("SEACARID", p_id)]]), "  \n"))
  }
  
  ############
  ### maps ###
  ############
  
  # function to account for overlapping map labels
  adjust_label_position <- function(df, buffer_distance) {
    # Calculate distances between all points
    distances <- distm(df[, c("lon", "lat")])
    df$labelDirection <- "right"
    
    for (i in 1:nrow(df)) {
      for (j in 1:nrow(df)) {
        if (i != j && distances[i, j] < buffer_distance) {
          if (df$lon[j] > df$lon[i]) {
            df$labelDirection[i] <- "left"
            break
          }
        }
      }
    }
    return(df)
  }
  
  map_output <- "output/maps/"
  
  # Set up coordinate table with station information
  # assign stationID starting N ->  S
  df_coord <- station_coordinates[ManagedAreaName == ma, ] %>%
    arrange(desc(lat)) %>%
    as.data.frame(row.names = 1:nrow(.)) %>%
    mutate(stationID = row_number())
  
  # testing implementation without labels
  # df_coord <- adjust_label_position(df_coord, buffer_distance = 6000)
  df_coord <- merge(df_coord, stations)
  
  # Unique stationid associations to color and label legend
  stationIDs <- df_coord %>% distinct(stationID, ProgramLocationID, Use_In_Analysis) %>%
    arrange(stationID)
  
  iconSet <- awesomeIconList(
    `Use In Analysis` = makeAwesomeIcon(
      icon = "glyphicon glyphicon-stats", library = "glyphicon", iconColor = "black", markerColor = "green"
    )
  )
  
  # create basemap
  map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
    addProviderTiles(providers$CartoDB.PositronNoLabels)
  
  for(sta_name in unique(df_coord$ProgramLocationID)){
    
    lati <- df_coord %>% filter(ProgramLocationID==sta_name) %>% distinct(lat) %>% pull(lat)
    long <- df_coord %>% filter(ProgramLocationID==sta_name) %>% distinct(lon) %>% pull(lon)
    
    used <- df_coord %>% filter(ProgramLocationID==sta_name) %>% 
      distinct(Use_In_Analysis) %>% pull(Use_In_Analysis)
    stationID <- stationIDs %>% filter(ProgramLocationID==sta_name) %>% pull(stationID)
    
    icons <- awesomeIcons(
      # icon = ifelse(length(used)>1, "glyphicon-stats", ifelse(
      #   stations %>% filter(ProgramLocationID==sta_name) %>% pull(Use_In_Analysis) == TRUE, 
      #   "glyphicon-stats", 
      #   "glyphicon-none"
      # )),
      text = stationID,
      fontFamily = "Arial",
      iconColor = 'black',
      library = 'glyphicon',
      markerColor = ifelse(length(used)>1, "green", ifelse(
        stations %>% filter(ProgramLocationID==sta_name) %>% pull(Use_In_Analysis) == TRUE, 
        "green", 
        "orange"
      ))
    )
    
    # Add markers for each station
    map <- map %>%
      addAwesomeMarkers(lng=long, lat=lati, icon=icons)
    
    # Only show labels if they will fit (10 or less)
    # if(length(unique(df_coord$ProgramLocationID)) > 10){
    #   map <- map %>%
    #     addAwesomeMarkers(lng=long, lat=lati, icon=icons)
    # } else {
    #   map <- map %>%
    #     addAwesomeMarkers(lng=long, lat=lati, label=sta_name,icon=icons,
    #                       labelOptions = labelOptions(
    #                         noHide = T, 
    #                         direction = label_dir,
    #                         style = list("font-size" = "16px",
    #                                      "background-color" = "rgba(255,255,255,.5)"),
    #                         offset = c(offset_val, 0)
    #                       ))      
    # }
    
    # set zoom level if only 2 or less stations available
    if(length(unique(df_coord$ProgramLocationID)) <= 2) {
      map <- map %>%
        setView(lng = long, lat = lati, zoom = 12)
    }
  }
  
  # add legend for MAs with more than 1 station
  if(nrow(df_coord) > 1){
    map <- map %>%
      addLegendAwesomeIcon(iconSet = iconSet,
                           position = 'topright')
  }
  
  # Add miniMap (inset to show context within MA)
  # Add scale-bar (metric & imperial)
  map <- map %>%
    addMiniMap(centerFixed = c(mean(df_coord$lat),mean(df_coord$lon)), 
               zoomLevelOffset = -5, 
               position = 'topright', 
               tiles = providers$CartoDB.PositronNoLabels) %>%
    addScaleBar(position = "bottomright",
                options = scaleBarOptions(metric=TRUE))
  
  # Create legend for station labels
  labels <- c()
  colors <- c()
  for(sid in stationIDs$stationID){
    plid <- stationIDs %>% filter(stationID==sid) %>% pull(ProgramLocationID)
    use <- stationIDs %>% filter(stationID==sid) %>% pull(Use_In_Analysis)
    
    lab <- paste0(sid, " - ", plid)
    labels <- c(labels, lab)
    col <- ifelse(use, "green", "orange")
    colors <- c(colors, col)
  }
  
  map <- map %>% 
    addLegend(
      "bottomright",
      title = "Station Labels",
      labels = labels,
      colors = colors
    )
  
  map_out <- paste0(map_output, ma_abrev, ".png")
  
  # save file as png
  mapshot(map, file = map_out)
  
  # draw .png with ggplot
  p1 <- ggdraw() + draw_image(map_out, scale = 1)
  
  caption <- paste0("Map showing continuous water quality sampling locations within the boundaries of *", ma, "*. 
                    Sites marked as *Use In Analysis* (green) are featured in this report.  \n")
  
  subchunkify(cat("![", caption, "](", map_out,")"))
  
  # print(p1)
  cat("  \n")
  # cat(caption)
  cat("  \n\n")
}

plot_cont_combined <- function(param, parameter, region, ma, ma_abrev, report_type){
  format_type <- ifelse(report_type=="HTML", "simple", "latex")
  Mon_YM_Stats <- as.data.table(load_cont_data_table(param, region, "Mon_YM_Stats"))
  Mon_YM_Stats$ManagedAreaName[Mon_YM_Stats$ManagedAreaName=="St. Andrews State Park Aquatic Preserve"] <- "St. Andrews Aquatic Preserve"
  Mon_YM_Stats$ManagedAreaName[Mon_YM_Stats$ManagedAreaName=="Southeast Florida Coral Reef Ecosystem Conservation Area"] <- "Kristin Jacobs Coral Aquatic Preserve"
  Mon_YM_Stats <- Mon_YM_Stats[ManagedAreaName == ma & ParameterName == parameter, ]
  
  skt_stats <- as.data.table(load_cont_data_table(param, region, "skt_stats"))
  skt_stats$ManagedAreaName[skt_stats$ManagedAreaName=="St. Andrews State Park Aquatic Preserve"] <- "St. Andrews Aquatic Preserve"
  skt_stats$ManagedAreaName[skt_stats$ManagedAreaName=="Southeast Florida Coral Reef Ecosystem Conservation Area"] <- "Kristin Jacobs Coral Aquatic Preserve"
  skt_stats <- skt_stats[ManagedAreaName==ma, ]
  # Remove text-based "NA" values in p column
  if (nrow(skt_stats[skt_stats$p=="    NA", ]) > 0){
    skt_stats[skt_stats$p=="    NA", ]$p <- NA
  }
  skt_stats$p <- round(as.numeric(skt_stats$p), 4)
  
  if(length(unique(skt_stats[!is.na(ProgramID),ProgramLocationID]))>0){
    # Account for managed areas with large number of continuous sites
    # Too many to plot together, plot combined by Program
    if(length(unique(skt_stats$ProgramLocationID))>10){
      for(prog_n in 1:length(sort(unique(skt_stats$ProgramID)))){
        # pid is ProgramID, prog_n is the current program's number
        pid <- sort(unique(skt_stats$ProgramID))[prog_n]
        #Locate plot file, including prog_n
        plot_loc <- get_plot(ma_abrev = ma_abrev, parameter = parameter, type = "Continuous", pid = prog_n)
        
        ResultTable <- skt_stats %>% filter(ProgramID==pid) %>% rowwise() %>% mutate(
            `Statistical Trend` = checkTrends(`p` = p, Slope = SennSlope, SufficientData = SufficientData),
            `Period of Record` = paste0(EarliestYear, " - ", LatestYear)) %>%
          select(ProgramLocationID, `Statistical Trend`, N_Data, N_Years, `Period of Record`, Median, tau,
                 SennIntercept, SennSlope, p) %>%
          rename("Station" = ProgramLocationID, "Sample Count" = N_Data, 
                 "Years with Data" = N_Years, "Sen Intercept" = SennIntercept, 
                 "Sen Slope" = SennSlope) %>%
          mutate_if(is.numeric, ~round(., 2))
        setDT(ResultTable)
        ResultTable[is.na(ResultTable)] <- "-"
        
        title <- glue("### {parameter} - Continuous - Program {pid}")
        cat("  \n")
        cat(title)
        cat("  \n")
        
        # fig_caption <- paste0("Figure for ", parameter, " - Continuous - Program ", pid)
        fig_caption <- FigureCaptions[ParameterName==parameter & SamplingFrequency=="Continuous", FigureCaptions]
        subchunkify(cat("![", fig_caption, "](", plot_loc,")"))

        # cat("![](", plot_loc,")")
        cat("  \n")
        
        # Grab relevant table description for a given plot
        desc <- TableDescriptions[ManagedAreaName==ma & ParameterName==parameter & SamplingFrequency=="Continuous", get(descriptionColumn)]
        # Table title
        table_title <- paste0("Seasonal Kendall-Tau Results for ", parameter, " - Program ", pid)
        
        # Apply escape characters for some stations which will throw errors in LaTeX
        ResultTable$Station <- gsub("_", "-", ResultTable$Station)
        ResultTable$Station <- gsub("&", "-and-", ResultTable$Station)
        
        # Create and display final result table
        result_table <- kable(ResultTable, format=format_type,
                              caption = table_title,
                              row.names = FALSE, digits = 5, booktabs = T, linesep = "", escape = F, longtable = F) %>%
          kableExtra::kable_styling(latex_options = c("scale_down", "HOLD_position"))
        cat("  \n")
        print(result_table)
        cat("  \n")
        cat(desc)
        cat("  \n")
        cat("\n \n \n")    
      }
      ### Continuous sample location maps
      # Locate map
      map_loc <- str_subset(cont_map_locs, paste0("_", param, "_", ma_abrev, "_map"))
      # captions / label
      cat("  \n")
      cat("\\newpage")
      caption <- paste0("Map showing location of ", tolower(parameter), " continuous water quality sampling locations within the boundaries of *", ma, 
                        "*. The bubble size on the maps above reflect the amount of data available at each sampling site.  \n")
      cat("  \n")
      # Print map
      subchunkify(cat("![", caption, "](", map_loc,")"))
      cat("  \n")
    } else {
      #Locate plot file
      plot_loc <- get_plot(ma_abrev = ma_abrev, parameter = parameter, type = "Continuous", pid = "none")
      
      ResultTable <- skt_stats %>% rowwise() %>% mutate(
          `Statistical Trend` = checkTrends(`p` = p, Slope = SennSlope, SufficientData = SufficientData),
          `Period of Record` = paste0(EarliestYear, " - ", LatestYear)) %>%
        select(ProgramLocationID, `Statistical Trend`, N_Data, N_Years, `Period of Record`, Median, tau,
               SennIntercept, SennSlope, p) %>%
        rename("Station" = ProgramLocationID, "Sample Count" = N_Data, 
               "Years with Data" = N_Years, "Sen Intercept" = SennIntercept, 
               "Sen Slope" = SennSlope) %>%
        mutate_if(is.numeric, ~round(., 2))
      setDT(ResultTable)
      ResultTable[is.na(ResultTable)] <- "-"
      
      title <- glue("### {parameter} - Continuous")
      cat("  \n")
      cat(title)
      cat("  \n")
      
      # fig_caption <- paste0("Figure for ", parameter, " - Continuous")
      fig_caption <- FigureCaptions[ParameterName==parameter & SamplingFrequency=="Continuous", FigureCaptions]
      subchunkify(cat("![", fig_caption, "](", plot_loc,")"))
      # cat("![](", plot_loc,")")
      
      cat("  \n")
      
      # Grab relevant table description for a given plot
      desc <- TableDescriptions[ManagedAreaName==ma & ParameterName==parameter & SamplingFrequency=="Continuous", get(descriptionColumn)]
      # Table title
      table_title <- paste0("Seasonal Kendall-Tau Results for ", parameter, " - All Stations")
      
      # Apply escape characters for some stations which will throw errors in LaTeX
      ResultTable$Station <- gsub("_", "-", ResultTable$Station)
      ResultTable$Station <- gsub("&", "-and-", ResultTable$Station)
      
      # Create and display final result table
      result_table <- kable(ResultTable, format=format_type,
                            caption = table_title,
                            row.names = FALSE, digits = 5, booktabs = T, linesep = "", escape = F, longtable = F) %>%
        kableExtra::kable_styling(latex_options = c("scale_down", "HOLD_position"))
      
      print(result_table)
      cat("  \n")
      cat(desc)
      ### Continuous sample location maps
      # Locate map
      map_loc <- str_subset(cont_map_locs, paste0("_", param, "_", ma_abrev, "_map"))
      # captions / label
      cat("  \n")
      cat("\\newpage")
      caption <- paste0("Map showing location of ", tolower(parameter), " continuous water quality sampling locations within the boundaries of *", ma, 
                        "*. The bubble size on the maps above reflect the amount of data available at each sampling site.  \n")
      cat("  \n")
      # Print map
      subchunkify(cat("![", caption, "](", map_loc,")"))
      cat("  \n")
      cat("\n \n \n")    
    }
  }
}
