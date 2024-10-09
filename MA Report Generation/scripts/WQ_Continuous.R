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
  files <- list.files(here::here("output/tables/cont"),pattern = "\\.rds$")
  file_path <- paste0("_",param,"_", region,"_", table) 
  
  # subset file list to select desired table RDS file
  table_file <- paste0("output/tables/cont/",str_subset(files, file_path))
  
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

cont_managed_areas <- unique(station_coordinates$ManagedAreaName)

# Provides a table for stations with Cont. Data
# and which stations passed the tests
station_count_table <- function(coordinates_df, station_coordinates, ma, ma_abrev){
  
  cat("\\newpage")
  
  # create frame to show available stations
  # show how many are included/excluded in the report
  # stations <- coordinates_df %>%
  #   filter(ManagedAreaName==ma) %>%
  #   group_by(ProgramLocationID) %>%
  #   ungroup() %>%
  #   select(ProgramLocationID, ProgramID, ProgramName, Use_In_Analysis)
  
  stations <- coordinates_df[ManagedAreaName == ma, .(ProgramLocationID, ProgramID, ProgramName, Use_In_Analysis)]
  
  programs_by_ma <- unique(stations$ProgramID)
  
  # table
  for(prog in programs_by_ma){
    n_years <- coordinates_df %>% filter(ManagedAreaName==ma, ProgramID==prog) %>%
      group_by(ProgramLocationID, years_of_data, Use_In_Analysis, ProgramName) %>%
      summarise(params = list(Parameter)) %>%
      arrange(ProgramLocationID)
    
    p_name <- unique(n_years$ProgramName)
    caption <- paste0(p_name," (",prog,")")
    
    station_kable <- n_years %>% select(-ProgramName) %>%
      kable(format="latex",caption=caption, booktabs = T, linesep = "",
            col.names = c("ProgramLocationID","Years of Data","Use in Analysis","Parameters")) %>%
      row_spec(0, italic=TRUE) %>%
      kable_styling(latex_options=c("scale_down","HOLD_position"),
                    position = "center")
    
    print(station_kable)
    cat("\n")
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
  
  # create basemap
  map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
    addProviderTiles(providers$CartoDB.PositronNoLabels) # %>% addTiles()
  
  df_coord <- station_coordinates[ManagedAreaName == ma, ]
  
  df_coord <- adjust_label_position(df_coord, buffer_distance = 6000)
  
  df_coord <- merge(df_coord, stations)
  
  iconSet <- awesomeIconList(
    `Use In Analysis` = makeAwesomeIcon(
      icon = "glyphicon glyphicon-stats", library = "glyphicon", iconColor = "black", markerColor = "green"
    )
  )
  
  for (sta_name in unique(df_coord$ProgramLocationID)){
    
    lati <- df_coord %>% filter(ProgramLocationID==sta_name) %>% distinct(lat) %>% pull(lat)
    long <- df_coord %>% filter(ProgramLocationID==sta_name) %>% distinct(lon) %>% pull(lon)
    
    used <- df_coord %>% filter(ProgramLocationID==sta_name) %>% 
      distinct(Use_In_Analysis) %>% pull(Use_In_Analysis)
    
    label_dir <- df_coord$labelDirection[i]
    offset_val <- ifelse(label_dir=="right", 10, -10)
    
    icons <- awesomeIcons(
      icon = ifelse(length(used)>1, "glyphicon-stats", ifelse(
        stations %>% filter(ProgramLocationID==sta_name) %>% pull(Use_In_Analysis) == TRUE, 
        "glyphicon-stats", 
        "glyphicon-none"
      )),
      iconColor = 'black',
      library = 'glyphicon',
      markerColor = ifelse(length(used)>1, "green", ifelse(
        stations %>% filter(ProgramLocationID==sta_name) %>% pull(Use_In_Analysis) == TRUE, 
        "green", 
        "orange"
      ))
    )
    
    # Only show labels if they will fit (10 or less)
    if(length(unique(df_coord$ProgramLocationID)) > 10){
      map <- map %>%
        addAwesomeMarkers(lng=long, lat=lati, icon=icons)
    } else {
      map <- map %>%
        addAwesomeMarkers(lng=long, lat=lati, label=sta_name,icon=icons,
                          labelOptions = labelOptions(
                            noHide = T, 
                            direction = label_dir,
                            style = list("font-size" = "16px",
                                         "background-color" = "rgba(255,255,255,.5)"),
                            offset = c(offset_val, 0)
                          ))      
    }
    

    
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

  map_out <- paste0(map_output, ma_abrev, ".png")
  
  # save file as png
  mapshot(map, file = map_out)
  
  # draw .png with ggplot
  p1 <- ggdraw() + draw_image(map_out, scale = 1)
  
  caption <- paste0("Map showing Continuous Water Quality Monitoring sampling locations within the boundaries of ", ma, ". 
                    Sites marked as *Use In Analysis* are featured in this report.  \n")
  
  print(p1)
  cat("  \n")
  cat(caption)
  cat("  \n\n")
}

# Unified continuous plotting function
plot_cont <- function(p, y_labels, parameter, cont_data, ma){
  
  data <- cont_data[ManagedAreaName == ma, ]
  
  Mon_YM_Stats <- as.data.table(load_cont_data_table(p, region, "Mon_YM_Stats"))
  skt_stats <- as.data.table(load_cont_data_table(p, region, "skt_stats"))
  
  skt_stats <- skt_stats[ManagedAreaName==ma, ]
  
  # Checking for missing values
  Mon_YM_Stats <- Mon_YM_Stats[ManagedAreaName == ma & ParameterName == parameter, ]
  
  ### SKT STATS ###
  # Gets x and y values for starting point for trendline
  KT.Plot <- skt_stats %>%
    group_by(MonitoringID) %>%
    summarize(x=decimal_date(EarliestSampleDate),
              y=(x-EarliestYear)*SennSlope+SennIntercept)
  # Gets x and y values for ending point for trendline
  KT.Plot2 <- skt_stats %>%
    group_by(MonitoringID) %>%
    summarize(x=decimal_date(LastSampleDate),
              y=(x-EarliestYear)*SennSlope+SennIntercept)
  # Combines the starting and endpoints for plotting the trendline
  KT.Plot <- bind_rows(KT.Plot, KT.Plot2)
  rm(KT.Plot2)
  KT.Plot <- as.data.table(KT.Plot[order(KT.Plot$MonitoringID), ])
  KT.Plot <- KT.Plot[!is.na(KT.Plot$y),]
  setDT(KT.Plot)
  
  # unique monitoring location IDs for each managed area
  MonIDs <- unique(data$MonitoringID)
  n <- length(MonIDs)
  
  n_included <- data[Use_In_Analysis == TRUE, uniqueN(MonitoringID)]
  
  if (n_included > 0) {
    
    # Add heading for parameter if included
    subtitle <- glue("## {parameter} - Continuous Water Quality")
    cat(subtitle, "\n\n")
    
    # Begins looping through each monitoring location
    for (i in 1:length(MonIDs)) {
      id <- MonIDs[i]
      # Plot trendplots
      # Gets data to be used in plot for monitoring location
      plot_data <- Mon_YM_Stats[MonitoringID==id,]
      
      if (nrow(plot_data) > 0) {
        # Gets trendline data for monitoring location
        KT.plot_data <- KT.Plot[MonitoringID==id,]
        #Determine max and min time (Year) for plot x-axis
        t_min <- min(plot_data$Year)
        t_max <- max(plot_data$YearMonthDec)
        t_max_brk <- as.integer(round(t_max, 0))
        t <- t_max-t_min
        min_RV <- min(plot_data$Mean)
        # Creates break intervals for plots based on number of years of data
        if(t>=30){
          # Set breaks to every 10 years if more than 30 years of data
          brk <- -10
        }else if(t<30 & t>=10){
          # Set breaks to every 4 years if between 30 and 10 years of data
          brk <- -4
        }else if(t<10 & t>=4){
          # Set breaks to every 2 years if between 10 and 4 years of data
          brk <- -2
        }else if(t<4 & t>=2){
          # Set breaks to every year if between 4 and 2 years of data
          brk <- -1
        }else if(t<2){
          # Set breaks to every year if less than 2 years of data
          brk <- -1
          # Sets t_max to be 1 year greater and t_min to be 1 year lower
          # Forces graph to have at least 3 tick marks
          t_max <- t_max+1
          t_min <- t_min-1
        }
        # Get name of managed area
        MA_name <- skt_stats[MonitoringID==id, ManagedAreaName]
        # Get program location name
        Mon_name <- paste0(skt_stats[MonitoringID==id, ProgramName], 
                           " (", skt_stats[MonitoringID==id, ProgramID], ")")
        
        mon_name_short <- skt_stats[MonitoringID==id, ProgramLocationID]
        # Create plot object with data and trendline
        p1 <- ggplot(data=plot_data,
                     aes(x=YearMonthDec, y=Mean)) +
          geom_point(shape=21, size=3, color="#333333", fill="#cccccc",
                     alpha=0.75) +
          geom_line(data=KT.plot_data, aes(x=x, y=y),
                    color="#000099", linewidth=1.2, alpha=0.7) +
          labs(title=paste0(MA_name, "\n", mon_name_short),
               subtitle=parameter,
               x="Year", y=y_labels) +
          scale_x_continuous(limits=c(t_min-0.25, t_max+0.25),
                             breaks=seq(t_max_brk, t_min, brk)) +
          plot_theme
        
        # Creates ResultTable to display statistics below plot
        ResultTable <- skt_stats[MonitoringID==id, ] %>%
          select(RelativeDepth, N_Data, N_Years, Median, Independent, tau, p,
                 SennSlope, SennIntercept, ChiSquared, pChiSquared, Trend)
        # Create table object
        t1 <- ggtexttable(ResultTable, rows=NULL,
                          theme=ttheme(base_size=10)) %>%
          tab_add_footnote(text="p < 0.00005 appear as 0 due to rounding.\n
                              SennIntercept is intercept value at beginning of
                              record for monitoring location",
                           size=10, face="italic")
        
        ### Monitoring Station Name Label ###
        mon_title <- glue("### {mon_name_short}")
        cat(paste0(mon_title, "  \n",Mon_name))
        cat("  \n")
        
        # Arrange and display plot and statistic table
        print(ggarrange(p1, t1, ncol=1, heights=c(0.85, 0.15)))
        # Add extra space at the end to prevent the next figure from being too close
        cat("\n \n \n")
      }
      
      
    }
    
    # plot data for all monitoring locations combined
    # trend_plot("all")
  }
}

plot_cont_combined <- function(param, y_labels, parameter, cont_data, ma){
  data <- cont_data[ManagedAreaName == ma, ]
  
  Mon_YM_Stats <- as.data.table(load_cont_data_table(param, region, "Mon_YM_Stats"))
  skt_stats <- as.data.table(load_cont_data_table(param, region, "skt_stats"))
  
  skt_stats <- skt_stats[ManagedAreaName==ma, ]
  
  # Checking for missing values
  Mon_YM_Stats <- Mon_YM_Stats[ManagedAreaName == ma & ParameterName == parameter, ]
  
  if(length(unique(Mon_YM_Stats$ProgramLocationID))>1){
    
    ### SKT STATS ###
    # Gets x and y values for starting point for trendline
    KT.Plot <- skt_stats %>%
      group_by(MonitoringID, ProgramID, ProgramLocationID) %>%
      summarize(x=decimal_date(EarliestSampleDate),
                y=(x-EarliestYear)*SennSlope+SennIntercept)
    # Gets x and y values for ending point for trendline
    KT.Plot2 <- skt_stats %>%
      group_by(MonitoringID, ProgramID, ProgramLocationID) %>%
      summarize(x=decimal_date(LastSampleDate),
                y=(x-EarliestYear)*SennSlope+SennIntercept)
    # Combines the starting and endpoints for plotting the trendline
    KT.Plot <- bind_rows(KT.Plot, KT.Plot2)
    rm(KT.Plot2)
    KT.Plot <- as.data.table(KT.Plot[order(KT.Plot$MonitoringID), ])
    KT.Plot <- KT.Plot[!is.na(KT.Plot$y),]
    setDT(KT.Plot)
    
    # Account for managed areas with large number of continuous sites
    # Too many to plot together, plot combined by Program
    if(length(unique(Mon_YM_Stats$ProgramLocationID))>10){
      
      cat("### All Stations Combined by Program")
      cat("  \n")
      
      for(pid in unique(Mon_YM_Stats$ProgramID)){
        
        # all plots together
        plot_data <- Mon_YM_Stats[ManagedAreaName==ma & ProgramID==pid,]
        p_name <- unique(plot_data$ProgramName)
        
        #Determine max and min time (Year) for plot x-axis
        t_min <- min(plot_data$Year)
        t_max <- max(plot_data$YearMonthDec)
        t_max_brk <- as.integer(round(t_max, 0))
        t <- t_max-t_min
        min_RV <- min(plot_data$Mean)
        
        # Creates break intervals for plots based on number of years of data
        if(t>=30){
          # Set breaks to every 10 years if more than 30 years of data
          brk <- -10
        }else if(t<30 & t>=10){
          # Set breaks to every 4 years if between 30 and 10 years of data
          brk <- -4
        }else if(t<10 & t>=4){
          # Set breaks to every 2 years if between 10 and 4 years of data
          brk <- -2
        }else if(t<4 & t>=2){
          # Set breaks to every year if between 4 and 2 years of data
          brk <- -1
        }else if(t<2){
          # Set breaks to every year if less than 2 years of data
          brk <- -1
          # Sets t_max to be 1 year greater and t_min to be 1 year lower
          # Forces graph to have at least 3 tick marks
          t_max <- t_max+1
          t_min <- t_min-1
        }
        
        setDT(plot_data)
        # number of stations for shape-palette
        n <- length(KT.Plot[ProgramID==pid, unique(MonitoringID)])
        
        p1 <- ggplot(data=plot_data, aes(x=YearMonthDec, y=Mean, group=factor(ProgramLocationID))) +
          geom_point(aes(shape=ProgramLocationID), color="#cccccc" ,fill="#444444", size=3,alpha=0.9, show.legend = TRUE) +
          labs(title=paste0(ma,"\n",p_name,
                            "\nProgramID: ", pid),
               subtitle=paste0(parameter, " - Continuous"),
               x="Year", y=y_labels) +
          scale_x_continuous(limits=c(t_min-0.25, t_max+0.25),
                             breaks=seq(t_max_brk, t_min, brk)) +
          plot_theme +
          geom_line(data=KT.Plot[ProgramID==pid, ], aes(x=x, y=y, linetype=ProgramLocationID, color=ProgramLocationID), linewidth=1.2, alpha=0.7) +
          labs(shape  = "Station ID", linetype = "Station ID", color = "Station ID") +
          scale_shape_manual(values=1:n) +
          scale_linetype_manual(values=1:n) +
          scale_color_manual(values=1:n)
        
        ResultTable <- skt_stats %>%
          filter(ProgramID==pid) %>%
          mutate("Period of Record" = paste0(EarliestYear, " - ", LatestYear)) %>%
          select(ProgramLocationID, N_Data, N_Years, "Period of Record", Median, tau,
                 SennIntercept, SennSlope, p) %>%
          rename("Station" = ProgramLocationID) %>%
          mutate_if(is.numeric, ~round(., 2))
        setDT(ResultTable)
        # Remove text-based "NA" values in p column
        if (nrow(ResultTable[ResultTable$p=="    NA", ]) > 0){
          ResultTable[ResultTable$p=="    NA", ]$p <- "-"
        }
        ResultTable[is.na(ResultTable)] <- "-"
        
        t1 <- ggtexttable(ResultTable, rows=NULL,
                          theme=ttheme(base_size=10)) %>%
          tab_add_footnote(text="p < 0.00005 appear as 0 due to rounding.\n
                                  SennIntercept is intercept value at beginning of
                                  record for monitoring location",
                           size=10, face="italic")
        
        cat("    \n")
        cat("  \n")
        cat("  \n")
        print(p1)
        cat("  \n")
        
        result_table <- kable(ResultTable, format="simple",
                              caption=paste0("Seasonal Kendall-Tau Results for All Stations - ", parameter),
                              row.names = FALSE, digits = 5) %>%
          kable_styling(font_size=8)
        
        print(result_table)
        cat("\n \n \n")    
      }
    } else {
      # all plots together
      plot_data <- Mon_YM_Stats[ManagedAreaName==ma, ]
      
      #Determine max and min time (Year) for plot x-axis
      t_min <- min(plot_data$Year)
      t_max <- max(plot_data$YearMonthDec)
      t_max_brk <- as.integer(round(t_max, 0))
      t <- t_max-t_min
      min_RV <- min(plot_data$Mean)
      # Creates break intervals for plots based on number of years of data
      if(t>=30){
        # Set breaks to every 10 years if more than 30 years of data
        brk <- -10
      }else if(t<30 & t>=10){
        # Set breaks to every 4 years if between 30 and 10 years of data
        brk <- -4
      }else if(t<10 & t>=4){
        # Set breaks to every 2 years if between 10 and 4 years of data
        brk <- -2
      }else if(t<4 & t>=2){
        # Set breaks to every year if between 4 and 2 years of data
        brk <- -1
      }else if(t<2){
        # Set breaks to every year if less than 2 years of data
        brk <- -1
        # Sets t_max to be 1 year greater and t_min to be 1 year lower
        # Forces graph to have at least 3 tick marks
        t_max <- t_max+1
        t_min <- t_min-1
      }
      
      setDT(plot_data)
      KT.Plot$ProgramLocationID <- ""
      for (m in unique(KT.Plot$MonitoringID)){
        PLID <- plot_data[MonitoringID == m, unique(ProgramLocationID)]
        KT.Plot[MonitoringID == m, ProgramLocationID := PLID]
      }
      
      # number of stations for shape-palette
      n <- length(unique(KT.Plot$MonitoringID))
      
      p1 <- ggplot(data=plot_data, aes(x=YearMonthDec, y=Mean, group=factor(ProgramLocationID))) +
        geom_point(aes(shape=ProgramLocationID), color="#cccccc" ,fill="#444444", size=3,alpha=0.9, show.legend = TRUE) +
        labs(title=paste0(ma, "\nAll Stations"),
             subtitle=paste0(parameter, " - Continuous"),
             x="Year", y=y_labels) +
        scale_x_continuous(limits=c(t_min-0.25, t_max+0.25),
                           breaks=seq(t_max_brk, t_min, brk)) +
        plot_theme +
        geom_line(data=KT.Plot, aes(x=x, y=y, linetype=ProgramLocationID), color="#000099", linewidth=1.2, alpha=0.7) +
        labs(shape  = "Station ID", linetype = "Station ID") +
        scale_shape_manual(values=1:n)
      
      ResultTable <- skt_stats %>%
        mutate("Period of Record" = paste0(EarliestYear, " - ", LatestYear)) %>%
        select(ProgramLocationID, N_Data, N_Years, "Period of Record", Median, tau,
               SennIntercept, SennSlope, p) %>%
        rename("Station" = ProgramLocationID) %>%
        mutate_if(is.numeric, ~round(., 2))
      setDT(ResultTable)
      
      # Remove text-based "NA" values in p column
      if (nrow(ResultTable[ResultTable$p=="    NA", ]) > 0){
        ResultTable[ResultTable$p=="    NA", ]$p <- "-"
      }
      ResultTable[is.na(ResultTable)] <- "-"
      
      t1 <- ggtexttable(ResultTable, rows=NULL,
                        theme=ttheme(base_size=10)) %>%
        tab_add_footnote(text="p < 0.00005 appear as 0 due to rounding.\n
                                SennIntercept is intercept value at beginning of
                                record for monitoring location",
                         size=10, face="italic")
      
      title <- glue("### All Stations Combined")
      cat("  \n")
      cat(title)
      cat("  \n")
      print(p1)
      cat("  \n")
      
      result_table <- kable(ResultTable, format="simple",
                            caption=paste0("Seasonal Kendall-Tau Results for All Stations - ", parameter),
                            row.names = FALSE, digits = 5) %>%
        kable_styling(font_size=8)
      
      print(result_table)
      cat("\n \n \n")    
    }
  }
  
}