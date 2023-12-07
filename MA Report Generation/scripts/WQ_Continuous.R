# Cont. Data exports do not contain full parameter names or units
# Create dataframe containing that info
cont_params_long <- c("Dissolved Oxygen","Dissolved Oxygen Saturation","pH",
                      "Salinity","Turbidity","Water Temperature")
cont_params_short <- c("DO","DOS","pH","Sal","Turb","TempW")
cont_param_units <- c("mg/L","%","pH","ppt","NTU","Degrees C")
cont_regions <- c("NE","NW","SE","SW")

cont_param_df <- data.frame(param_short = cont_params_short,
                            parameter = cont_params_long,
                            unit = cont_param_units)

#################
### FUNCTIONS ###
#################

# For loading continuous data
# Load Data Table Function
load_cont_data_table <- function(param, region, table) {
  
  # Declaring RDS file list of respective tables
  # files <- list.files(here::here("output/tables/cont"),pattern = "\\.rds$")
  files <- list.files("output/tables/cont",pattern = "\\.rds$")
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

station_coordinates <- coordinates_df %>% group_by(ManagedAreaName, lat, lon) %>%
  distinct(ProgramLocationID)

cont_managed_areas <- unique(station_coordinates$ManagedAreaName)

# Provides a table for stations with Cont. Data
# and which stations passed the tests
station_count_table <- function(cont_data){
  
  cat("\\newpage")
  
  # create frame to show available stations
  # show how many are included/excluded in the report
  stations <- coordinates_df %>%
    filter(ManagedAreaName==ma) %>%
    group_by(ProgramLocationID) %>%
    ungroup() %>%
    select(ProgramLocationID, ProgramID, ProgramName, Use_In_Analysis)
  
  programs_by_ma <- unique(stations$ProgramID)
  
  # table
  for (prog in programs_by_ma){
    
    n_years <- coordinates_df %>% filter(ManagedAreaName==ma, ProgramID==prog) %>%
      group_by(ProgramLocationID) %>%
      distinct(ProgramLocationID, years_of_data, Use_In_Analysis, ProgramName) %>%
      arrange(ProgramLocationID)
    
    # n_years$Use_In_Analysis <- cell_spec(n_years$Use_In_Analysis, background = ifelse(n_years$Use_In_Analysis==TRUE, "green", "orange"))
    
    p_name <- unique(n_years$ProgramName)
    caption <- paste0(p_name," (",prog,")")
    
    station_kable <- n_years %>% select(-ProgramName) %>%
      kable(format="simple",caption=caption) %>%
      kable_styling()
    
    print(station_kable)
    cat("\n")
  }
  
  ## n stations total, n stations included (Use_In_Analysis)
  # n_stations <- nrow(stations)
  # n_stations_inc <- nrow(stations[stations$Use_In_Analysis==TRUE, ])
  
  ## print text statement
  # cat(paste0("There are ", n_stations, " stations in ", ma, ".  \n\n"))
  # cat(paste0(n_stations_inc, " out of ", n_stations, " are included in this report."))
  # cat("  \n\n")
  
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
  map <- leaflet() %>% addTiles()
  
  df_coord <- station_coordinates %>% filter(ManagedAreaName == ma)
  
  df_coord <- adjust_label_position(df_coord, buffer_distance = 6000)
  
  iconSet <- awesomeIconList(
    `Use In Analysis` = makeAwesomeIcon(
      icon = "glyphicon glyphicon-stats", library = "glyphicon", iconColor = "black", markerColor = "green"
    )
  )
  
  for (i in 1:nrow(df_coord)){
    lati <- df_coord$lat[i]
    long <- df_coord$lon[i]
    sta_name <- df_coord$ProgramLocationID[i]
    label_dir <- df_coord$labelDirection[i]
    offset_val <- ifelse(label_dir=="right", 10, -10)
    
    icons <- awesomeIcons(
      icon = ifelse(
        stations %>% filter(ProgramLocationID==sta_name) %>% pull(Use_In_Analysis) == TRUE, 
        "glyphicon-stats", 
        "glyphicon-none"
      ),
      iconColor = 'black',
      library = 'glyphicon',
      markerColor = ifelse(
        stations %>% filter(ProgramLocationID==sta_name) %>% pull(Use_In_Analysis) == TRUE, 
        "green", 
        "orange"
      )
    )
    
    map <- map %>%
      addAwesomeMarkers(lng=long, lat=lati, label=sta_name,icon=icons,
                        labelOptions = labelOptions(
                          noHide = T, 
                          direction = label_dir,
                          style = list("font-size" = "16px",
                                       "background-color" = "rgba(255,255,255,.5)"),
                          offset = c(offset_val, 0)
                        ))
    
    # set zoom level if only 1 station available
    if(nrow(df_coord) == 1) {
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

  map_out <- paste0(map_output, ma_abrev, ".png")
  
  # save file as png
  mapshot(map, file = map_out)
  
  # draw .png with ggplot
  p1 <- ggdraw() + draw_image(map_out, scale = 1)
  
  print(plot_grid(p1))
  cat("  \n\n")
}

# Unified continuous plotting function
plot_cont <- function(p, y_labels, parameter, cont_data) {
  
  data <- cont_data %>% filter(ManagedAreaName == ma)
  
  Mon_YM_Stats <- as.data.frame(load_cont_data_table(p, region, "Mon_YM_Stats"))
  skt_stats <- as.data.frame(load_cont_data_table(p, region, "skt_stats"))
  
  skt_stats <- skt_stats %>% 
    filter(ManagedAreaName==ma)
  
  # Checking for missing values
  Mon_YM_Stats <- Mon_YM_Stats %>%
    filter(ManagedAreaName == ma & ParameterName == parameter)
  
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
  
  # unique monitoring location IDs for each managed area
  MonIDs <- unique(data$MonitoringID)
  n <- length(MonIDs)
  
  n_included <- length(data %>%
                         group_by(MonitoringID) %>%
                         distinct(Use_In_Analysis) %>% 
                         filter(Use_In_Analysis == TRUE) %>% 
                         pull(MonitoringID))
  
  if (n_included > 0) {
    
    # Add heading for parameter if included
    subtitle <- glue("## {parameter}")
    cat(subtitle, "\n\n")
    
    # Begins looping through each monitoring location
    for (id in MonIDs) {
      
      ##################
      ### TRENDPLOTS ###
      ##################
      
      # Gets data to be used in plot for monitoring location
      plot_data <- Mon_YM_Stats[Mon_YM_Stats$MonitoringID==id,]
      
      if (nrow(plot_data) > 0) {
        # Gets trendline data for monitoring location
        KT.plot_data <- KT.Plot[KT.Plot$MonitoringID==id,]
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
          # Set breaks to every 5 years if between 30 and 10 years of data
          brk <- -5
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
        MA_name <- skt_stats$ManagedAreaName[skt_stats$MonitoringID==id]
        # Get program location name
        Mon_name <- paste0(skt_stats$ProgramID[skt_stats$MonitoringID==id],
                           "\n", skt_stats$ProgramName[skt_stats$MonitoringID==id], "\n",
                           skt_stats$ProgramLocationID[skt_stats$MonitoringID==id])
        mon_name_short <- skt_stats$ProgramLocationID[skt_stats$MonitoringID==id]
        # Create plot object with data and trendline
        p1 <- ggplot(data=plot_data,
                     aes(x=YearMonthDec, y=Mean)) +
          geom_point(shape=21, size=3, color="#333333", fill="#cccccc",
                     alpha=0.75) +
          geom_line(data=KT.plot_data, aes(x=x, y=y),
                    color="#000099", size=1.2, alpha=0.7) +
          labs(title=paste0(MA_name, "\n", Mon_name),
               subtitle=parameter,
               x="Year", y=y_labels) +
          scale_x_continuous(limits=c(t_min-0.25, t_max+0.25),
                             breaks=seq(t_max_brk, t_min, brk)) +
          plot_theme
        
        # Creates ResultTable to display statistics below plot
        ResultTable <- skt_stats[skt_stats$MonitoringID==id, ] %>%
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
        cat(mon_title, "\n\n")
        
        # Arrange and display plot and statistic table
        print(ggarrange(p1, t1, ncol=1, heights=c(0.85, 0.15)))
        # Add extra space at the end to prevent the next figure from being too close
        cat("\n \n \n")
      }
    }
    }
  }