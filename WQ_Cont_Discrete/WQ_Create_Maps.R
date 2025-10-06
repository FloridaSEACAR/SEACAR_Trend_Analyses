library(htmlwidgets)
library(htmltools)
library(glue)
library(leaflet)
library(leaflegend)
library(leaflet.providers)
library(rstudioapi)
library(tictoc)
library(mapview)

wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

# Load in a single discrete file to retrieve export date
temp <- fread(str_subset(list.files(seacar_data_location, full=T), "_NUT_Water_Temperature"))
exportDate <- unique(format(unique(temp$ExportVersion), "%m/%d/%Y"))
rm(temp)

##### Create maps ----
# style class for bottom-left leaflet/HTML metadata text
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
    font-family: Arial, Helvetica, sans-serif;
  }"))

# Function to set radius / circle size by # of samples (for legend)
calc_radius_cont <- function(n){sqrt(n)/20}
calc_radius_disc <- function(n){sqrt(n)}
## Discrete mapping function
plot_discrete_maps <- function(parameter, ma, discrete_df, ma_abrev, ma_shape, shape_coordinates, legend_loc = "topright"){
  # Subset from overall discrete dataframe
  subset_df <- discrete_df[ManagedAreaName==ma & ParameterName==parameter, ]
  if(!nrow(subset_df)==0){
    # Setting color palette
    # Determine unique ProgramName values in MA (for all params)
    all_values <- disc_programs[ManagedAreaName==ma, unique(ProgramName)]
    # Identify the values that are not manually assigned a palette color
    remaining_values <- setdiff(all_values, freq_programs)
    # Use remaining colors for these values
    base_remaining <- seacar_palette[-seq(1, length(freq_programs)*2, by = 2)]
    if(length(remaining_values) > length(base_remaining)){
      remaining_palette <- colorRampPalette(base_remaining)(length(remaining_values))
    } else {
      remaining_palette <- base_remaining[1:length(remaining_values)]
    }
    # Name remaining palette
    names(remaining_palette) <- sort(remaining_values)
    # Combine all values into a single palette
    full_mapping <- c(manual_colors, remaining_palette)
    # Create color-lookup palette function
    pal <- function(x){full_mapping[as.character(x)]}
    # Add color column to apply palette
    subset_df$color <- pal(subset_df$ProgramName)
    subset_df <- subset_df %>% arrange(match(color, seacar_palette))
    
    # Set up watermark text display
    ind <- websiteParams[ParameterName==parameter, unique(IndicatorName)]
    fig_text <- tags$div(HTML(glue("{ma} - Water Column - Discrete {ind} - {parameter} - Export Date: {exportDate}")),
                         style = "margin-bottom:10px;")
    
    # leaflet map
    map <- leaflet(subset_df, options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addPolygons(data=ma_shape, color="black", weight = 1, smoothFactor = 0.5, opacity = 0.8, fillOpacity = 0.1) %>%
      addCircleMarkers(lat=~Latitude_D, lng=~Longitude_, 
                       weight=1, color = "#000000", stroke = TRUE,
                       fillColor=~color, radius=sqrt(subset_df$n_data), 
                       fillOpacity=0.6) %>%
      addLegend(title = "Program", colors=pal(unique(subset_df$ProgramName)), labels=unique(subset_df$ProgramName),
                position = legend_loc) %>%
      addLegendSymbol(title = "ORCP Boundary", 
                      color = "#000000", fillColor = "#000000", fillOpacity = 0.1,
                      values = ma, shape = "rect", position = "topright") %>%
      fitBounds(lng1=shape_coordinates$xmin,
                lat1=shape_coordinates$ymin,
                lng2=shape_coordinates$xmax,
                lat2=shape_coordinates$ymax) %>%
      SEACAR::addCircleLegend(title = "Number of samples",
                              range = subset_df$n_data,
                              scaling_fun = calc_radius_disc,
                              fillColor = "#b3b3b3",
                              fillOpacity = 0.8,
                              weight = 1,
                              color = "#000000",
                              position = legend_loc,
                              type = "discrete") %>%
      addScaleBar(position = "bottomright",
                  options = scaleBarOptions(metric=TRUE)) %>%
      addControl(fig_text, position="bottomleft", className="map-title")
    
    # map output filepath
    map_output <- "output/maps/discrete/"
    param_short <- websiteParams[ParameterName==parameter, unique(ParameterShort)]
    ind_name <- websiteParams[ParameterShort==param_short & SamplingFrequency=="Discrete", unique(IndicatorShort)]
    map_out <- paste0(map_output, "discrete_", ind_name, "_", param_short, "_", ma_abrev, "_map.png")
    
    # save file as png
    mapview::mapshot(map, file = map_out, remove_controls = c("zoomControl"))
    print(paste0("Map created for ", parameter, " - ", ma))  
  }
}

## Continuous mapping function
plot_continuous_maps <- function(param_short, ma, cont_coordinates, ma_abrev, ma_shape){
  # Get list of stations for a given MA/param and their info
  ma_stations <- cont_coordinates[Parameter==param_short & ManagedAreaName==ma, ]
  # Create radius from N_data column
  ma_stations$rad <- sqrt(ma_stations$n_data)/20
  # Create color column using palette function
  ma_stations$color <- cont_pal(ma_stations$Entity)
  # Define alpha (transparency) - make exception for programs with low amounts of data (make them less transparent)
  ma_stations$alpha <- ifelse(ma_stations$n_data<=20000, 1, 0.6) # 0.6 is default
  # Discrete map uses `fitBounds` to set the view window around MA shape
  # Continuous will use focus level of the aggregate coordinates of the stations for some MA
  # Exceptions made for the following MA will focus map on their points rather than MA shape
  station_coord_view <- c("BBAP")
  if(ma_abrev %in% station_coord_view){
    shape_coordinates <- data.frame(
      "xmin" = min(ma_stations$lon),
      "ymin" = min(ma_stations$lat),
      "xmax" = max(ma_stations$lon),
      "ymax" = max(ma_stations$lat)
    )  
  } else {
    shape_coordinates <- SEACAR::get_shape_coordinates(ma_shape)
  }
  # data frame of Entity, ProgramName, color associations (arrange by Entity is key)
  cols <- ma_stations %>% group_by(ProgramName, Entity, color) %>% 
    reframe() %>% arrange(Entity)
  
  # Set up watermark text display
  parameter <- websiteParams[ParameterShort==param_short, unique(ParameterName)]
  ind <- websiteParams[ParameterName==parameter, unique(IndicatorName)]
  fig_text <- tags$div(HTML(glue("{ma} - Water Column - Continuous {ind} - {parameter} - Export Date: {exportDate}")))
  
  map <- leaflet(ma_stations, 
                 options = leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
    addPolygons(data=ma_shape, color="black", weight = 1, smoothFactor = 0.5, 
                opacity = 0.8, fillOpacity = 0.1) %>%
    addCircleMarkers(lng = ~lon, lat = ~lat, radius = ~rad, 
                     weight=1, color = "#000000", stroke = TRUE,
                     fillColor = ~color, fillOpacity = ~alpha) %>%
    addLegend(title = "Program", 
              colors=cont_pal(unique(cols$Entity)), 
              labels=unique(cols$ProgramName)) %>%
    addLegendSymbol(title = "ORCP Boundary", 
                    color = "#000000", fillColor = "#000000", fillOpacity = 0.1,
                    values = ma, shape = "rect", position = "topright") %>%
    fitBounds(lng1=shape_coordinates$xmin,
              lat1=shape_coordinates$ymin,
              lng2=shape_coordinates$xmax,
              lat2=shape_coordinates$ymax) %>%
    SEACAR::addCircleLegend(title = "Number of samples",
                            range = ma_stations$n_data,
                            scaling_fun = calc_radius_cont,
                            fillColor = "#b3b3b3",
                            fillOpacity = 0.8,
                            weight = 1,
                            color = "#000000",
                            position = "topright",
                            type = "continuous") %>%
    addScaleBar(position = "bottomright", options = scaleBarOptions(metric=TRUE)) %>% 
    addControl(fig_text, position="bottomleft", className="map-title")
  
  # map output filepath
  map_output <- "output/maps/continuous/"
  ind_name <- websiteParams[ParameterShort==param_short & SamplingFrequency=="Continuous", unique(IndicatorShort)]
  map_out <- paste0(map_output, "continuous_", ind_name, "_", param_short, "_", ma_abrev, "_map.png")
  
  # save file as png
  mapview::mapshot(map, file = map_out, remove_controls = c("zoomControl"))
  print(paste0("Map created for ", param_short, " - ", ma))
}

## Discrete ----------------------------------------
# Pre-process spatial data for greater efficiency
# Grab a list of programs for each discrete parameter for each MA
disc_programs <- data_output_disc %>% 
  group_by(ParameterName, ManagedAreaName) %>% 
  distinct(ProgramID, ProgramName) %>% as.data.table()

# grab sample coordinates from those programs
coord_df <- locs_pts_rcp %>% filter(ProgramID %in% disc_programs$ProgramID)

# frame to plot coordinates, allows for bubble size display of n_samples
ma_data <- data_output_disc %>%
  group_by(ParameterName, ManagedAreaName, ProgramID, ProgramName, ProgramLocationID) %>%
  summarise(n_data = n()) %>%
  rename(ProgramLoc = ProgramLocationID)

# merge frames together prior to plotting
discrete_df <- merge(ma_data, coord_df)
discrete_df <- discrete_df[order(discrete_df$n_data, decreasing=TRUE), ]

##### TEMPORARY #####
# Continuous site loaded into discrete data causes large bubbles on maps
discrete_df <- discrete_df[!discrete_df$ProgramLoc=="11NPSWRD_WQX-TIMU_BRD_CPK", ]
#####################

# Make ProgramNames shorter for map display
replacements <- c(
  "Aquatic Preserves" = "AP",
  "Aquatic Preserve" = "AP",
  "Water Quality" = "WQ",
  "Atlantic Oceanographic and Meteorological Laboratory \\(AOML\\)" = "AOML",
  "National Estuarine Research Reserve" = "NERR",
  "Aquatic Preserves Continuous Water Quality Monitoring" = "APCWQ",
  "Aquatic Preserve Continuous Water Quality Monitoring" = "APCWQ",
  "National Estuarine Research Reserve System-Wide Monitoring Program" = "NERR SWMP",
  "Oyster shell heights and taxonomic diversity in 2015-2017 among previously documented oiled and non-oiled reefs in Louisiana, Alabama, and the Florida panhandle" = "Oyster shell heights and taxonomic diversity in 2015-2017",
  "Southeast Area Monitoring and Assessment Program \\(SEAMAP\\)" = "SEAMAP",
  "A spatial model to improve site selection for seagrass restoration in shallow boating environments" = "A spatial model to improve site selection for seagrass restoration",
  "National Aquatic Resource Surveys, National Coastal Condition Assessment" = "Nat'l Aquatic Resource Surveys, Nat'l Coastal Condition Assessment",
  "Project COAST (Coastal Assessment Team) - Springs Coast Ecosystem Region" = "Project COAST - Springs Coast Ecosystem Region"
)
# Apply to overall programs for each MA
disc_programs <- disc_programs %>%
  mutate(ProgramName = purrr::reduce(
    names(replacements), 
    ~ str_replace_all(.x, .y, replacements[.y]),
    .init = ProgramName
  ))
# Apply to dataset
discrete_df <- discrete_df %>%
  mutate(ProgramName = purrr::reduce(
    names(replacements), 
    ~ str_replace_all(.x, .y, replacements[.y]),
    .init = ProgramName
  ))
setDT(discrete_df)

# setting color palette
seacar_palette <- SEACAR::seacar_palette2

# The most commonly-occurring programs (if in more than half of MAs) will get the same color across all MAs
freq_programs <- disc_programs %>% 
  group_by(ProgramID, ProgramName) %>% 
  reframe(count=length(unique(ManagedAreaName))) %>% 
  filter(count >= max(count) * 0.5) %>%
  pull(ProgramName) %>% sort()

# Select colors to manually assign, selecting every other color
manual_colors <- seacar_palette[seq(1, length(freq_programs)*2, by=2)]
# Name them according to the color values
names(manual_colors) <- freq_programs

## Discrete map generation; ----------
# Loop through available MAs
tic()
for(ma in unique(MA_All$ManagedAreaName)){
  # Abbreviated MA name for filenames
  ma_abrev <- MA_All[ManagedAreaName==ma, Abbreviation]
  # if(!ma_abrev %in% c("SAAP", "KJCAP")) next
  # find ma_shape and ma_coordinates for plotting
  ma_shape <- SEACAR::find_shape(rcp, ma)
  # get coordinates to set zoom level
  shape_coordinates <- SEACAR::get_shape_coordinates(ma_shape)
  # Hard-code legend locations for each MA (default to top-right if not in below vectors)
  top_left <- c("ABAP","CHAP","CBAP","FKNMS","GSCHAP","LKAP",
                "SMMAP","TCAP","NCAP", "KJCAP","SJBSBP")
  bottom_right <- c("AHAP","YRMAP","BBAP", "GSCHAP")
  bottom_left <- c("SAAP","IRVBFPAP","EBAP")
  # if(ma_abrev %in% top_left){
  #   legend_loc <- "topleft"
  # } else if(ma_abrev %in% bottom_right){
  #   legend_loc <- "bottomright"
  # } else if(ma_abrev %in% bottom_left){
  #   legend_loc <- "bottomleft"
  # } else {
  #   legend_loc <- "topright"
  # }
  legend_loc <- "topright"
  
  # Loop through all params for a given MA
  for(parameter in unique(discrete_df$ParameterName)){
    # Run mapping function for each MA
    plot_discrete_maps(parameter = parameter,
                       ma = ma,
                       discrete_df = discrete_df,
                       ma_abrev = ma_abrev,
                       ma_shape = ma_shape,
                       shape_coordinates = shape_coordinates,
                       legend_loc = legend_loc)
  }
  print(paste0(ma, " processing complete!"))
}
toc()

## Continuous ----------------------------------------
cont_coordinates <- data.table()
for(param in cont_params_short){
  for(region in c("NE","NW","SE","SW")){
    df <- readRDS(str_subset(cont_files, paste0(param, "_", region, "_Station_Coordinates")))
    cont_coordinates <- bind_rows(cont_coordinates, df)
  }
}
rm(df, region)
# add 1 to years_of_data (result of subtracting years)
cont_coordinates$years_of_data <- cont_coordinates$years_of_data + 1
# Rename programs: shorten program names, combine APCWQ and NERR SWMP into Entity
cont_coordinates <- cont_coordinates %>% mutate(Entity = ifelse(
  str_detect(ProgramName, "Aquatic Preserves Continuous Water Quality Monitoring|Aquatic Preserve Continuous Water Quality Monitoring"),
  "Aquatic Preserve Continuous Water Quality Program",
  ifelse(str_detect(ProgramName, "National Estuarine Research Reserve System-Wide Monitoring Program"),
         "National Estuarine Research Reserve SWMP",
         ProgramName))) %>% as.data.table()
# Rename and shorten other program names for display
rename_map <- c(
  "Atlantic Oceanographic and Meteorological Laboratory (AOML) South Florida Program Moored Instrument Array" = "AOML South Florida Program Moored Instrument Array",
  "Florida Keys National Marine Sanctuary Seagrass Monitoring Project" = "FKNMS Seagrass Monitoring Project",
  "FDEP Bureau of Survey and Mapping Continuous Water Quality Program" = "FDEP Bureau of Survey and Mapping Continuous WQ Program",
  "St. Johns River Water Management District Continuous Water Quality Programs" = "St. Johns River Water Management District Continuous WQ Programs",
  "Pensacola Bay Water Quality Monitoring Program" = "Pensacola Bay WQ Monitoring Program"
)
cont_coordinates[, Entity := fcoalesce(rename_map[Entity], Entity)]
cont_coordinates[, ProgramName := fcoalesce(rename_map[ProgramName], ProgramName)]
cont_coordinates <- cont_coordinates %>% 
  mutate(ProgramName = str_replace_all(ProgramName, c(
    "Aquatic Preserves Continuous Water Quality Monitoring" = "AP Continuous Water Quality Monitoring",
    "Aquatic Preserve Continuous Water Quality Monitoring" = "AP Continuous Water Quality Monitoring",
    "National Estuarine Research Reserve System-Wide Monitoring Program" = "NERR System-Wide Monitoring Program")))

# Hard-coded palette associations for each program name
cont_palette <- c(
  "AOML South Florida Program Moored Instrument Array" = "#964059",
  "Aquatic Preserve Continuous Water Quality Program" = "#E36B7E",
  "Continuous Bottom Temperature Measurements along the Florida Reef Tract" = "#EEA49A",
  "FDEP Bureau of Survey and Mapping Continuous WQ Program" = "#F7C7AA",
  "FKNMS Seagrass Monitoring Project" = "#F9E7BD",
  "National Data Buoy Center" = "#F1ECDE",
  "National Estuarine Research Reserve SWMP" = "#AAE6CB",
  "National Water Information System" = "#80E7E5",
  "Pensacola Bay WQ Monitoring Program" = "#89C7E9",
  "St. Johns River Water Management District Continuous WQ Programs" = "#7B9ED8",
  "USGS Coral Reef Ecosystem Studies (CREST) Project" = "#8D8AD2",
  "Water Temperature on Coral Reefs in the Florida Keys" = "#6B59AB"
)
# palette function
cont_pal <- function(x){cont_palette[as.character(x)]}

# Continuous map generation; ~15 mins ----------
tic()
for(ma in unique(cont_coordinates$ManagedAreaName)){
  ma_abrev <- MA_All[ManagedAreaName==ma, Abbreviation]
  # find ma_shape and ma_coordinates for plotting
  ma_shape <- SEACAR::find_shape(rcp, ma)
  for(param_short in cont_coordinates[ManagedAreaName==ma, unique(Parameter)]){
    plot_continuous_maps(param_short = param_short, 
                         ma = ma, 
                         cont_coordinates = cont_coordinates, 
                         ma_abrev = ma_abrev,
                         ma_shape = ma_shape)
  }
  print(paste0(ma, " processing complete"))
}
toc()

#Gets list of all image files in output/Figures and creates zip directory
map_list <- list.files("output/maps/", pattern=".png", full=FALSE, recursive=TRUE)
setwd("output/maps/")
zip("WQMaps", files=map_list)
setwd(wd)