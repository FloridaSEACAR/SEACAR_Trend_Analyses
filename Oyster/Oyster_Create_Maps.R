# Creates Oyster Reef sample location maps (SEACAR Atlas)
library(sf)
library(leaflet)
library(leaflegend)
library(htmlwidgets)
library(htmltools)
library(glue)
library(rstudioapi)

wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

source("../SEACAR_data_location.R")

##### Create maps ----
# Function to add circle legends to map
# From GitHub user PaulC91
# https://gist.github.com/PaulC91/31c05f84b25975047092f13c2474507a
addCircleLegend <- function(
    map, title = "", range, scaling_fun, ...,
    color, weight, fillColor, fillOpacity,
    position = c("topright", "bottomright", "bottomleft", "topleft"),
    data = leaflet::getMapData(map), layerId = NULL,
    type = "discrete") {
  
  # Function to determine the number of circles to display, account for different habitat types
  determine_range_size <- function(range, thresholds) {
    range_diff <- abs(min(range) - max(range))
    max_range <- max(range)
    min_range <- min(range)
    
    if (range_diff > thresholds$large_cutoff) {
      if (max_range <= thresholds$upper_limit | min_range >= max_range * thresholds$relative_threshold) {
        if (range_diff / max_range >= thresholds$tight_lower | range_diff / max_range <= thresholds$tight_upper) {
          return("small")
        } else {
          return("medium")
        }
      } else {
        return("large")
      }
    } else {
      return("small")
    }
  }
  
  # Define thresholds for continuous and discrete types
  thresholds_list <- list(
    continuous = list(large_cutoff = 3000, upper_limit = 70000, relative_threshold = 0.8, tight_lower = 0.95, tight_upper = 0.15),
    discrete = list(large_cutoff = 20, upper_limit = 120, relative_threshold = 0.8, tight_lower = 0.95, tight_upper = 0.15),
    cw = list(large_cutoff = 20, upper_limit = 120, relative_threshold = 0.8, tight_lower = 0.95, tight_upper = 0.15),
    coral = list(large_cutoff = 20, upper_limit = 120, relative_threshold = 0.8, tight_lower = 0.95, tight_upper = 0.15),
    nekton = list(large_cutoff = 20, upper_limit = 120, relative_threshold = 0.8, tight_lower = 0.95, tight_upper = 0.15),
    oyster = list(large_cutoff = 20, upper_limit = 120, relative_threshold = 0.8, tight_lower = 0.95, tight_upper = 0.15),
    sav = list(large_cutoff = 60, upper_limit = 120, relative_threshold = 0.6, tight_lower = 0.95, tight_upper = 0.15)
  )
  
  # Apply function based on type
  if (type %in% names(thresholds_list)) {
    range_size <- determine_range_size(range, thresholds_list[[type]])
  }
  
  if(range_size %in% c("medium","large")){range <- base::pretty(sort(range), 20)}
  range <- range[range != 0]
  min_n <- ceiling(min(range, na.rm = TRUE))
  med_n <- round(median(range, na.rm = TRUE), 0)
  max_n <- round(max(range, na.rm = TRUE), 0)
  if(range_size=="small"){n_range<-max_n} else if(range_size=="medium"){
    n_range<-c(min_n, max_n)} else {n_range<-c(min_n, med_n, max_n)}
  radii <- scaling_fun(n_range, ...)
  n_range <- scales::label_number()(n_range)
  
  circle_style <- glue::glue(
    "border-radius:50%;
    border: {weight}px solid {color};
    background: {paste0(fillColor, round(fillOpacity*100, 0))};
    position: absolute;
    bottom:1px;
    right:25%;
    left:50%;"
  )
  
  text_style <- glue::glue(
    "text-align: right;
    font-size: 11px;
    position: absolute;
    bottom:3px;
    right:1px;"
  )
  
  buffer <-  max(radii)
  
  size_map <- list(
    large = c(3, 2, 1),
    medium = c(2, 1),
    small = c(1)
  )
  
  # Logic to account for different size combinations, write HTML
  sizes <- size_map[[range_size]]
  
  circle_elements <- glue::glue_collapse(
    glue::glue(
      '<div class="legendCircle" style="width: {radii[s] * 2}px; height: {radii[s] * 2}px; margin-left: {-radii[s]}px; {circle_style}"></div>',
      s = sizes
    ),
    sep = "\n"
  )
  
  value_elements <- glue::glue_collapse(
    glue::glue(
      '<div><p class="legendValue" style="margin-bottom: {radii[s] * 2 - 12}px; {text_style}">{n_range[s]}</p></div>',
      s = sizes
    ),
    sep = "\n"
  )
  
  min_width <- radii[max(sizes)] * 2 + buffer
  min_height <- radii[max(sizes)] * 2 + 12
  
  circle_legend <- htmltools::HTML(glue::glue(
    '<div class="bubble-legend">
    <div id="legendTitle" style="text-align: center; font-weight: bold;">{title}</div>
    <div class="symbolsContainer" style="min-width: {min_width}px; min-height: {min_height}px;">
      {circle_elements}
      {value_elements}
    </div>
  </div>'
  ))
  
  return(
    leaflet::addControl(map, html = circle_legend, position = position, layerId = layerId)
  )
}

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
    clear: none;
  }"))

# Function to add linebreaks to long Program names
shorten_program_names <- function(program_names, cutoff=50){
  split_phrase <- function(p_name){
    words <- str_split(p_name, pattern=coll(" "))[[1]]
    # Apply three line breaks
    if(length(words)>=14){n_lines <- 3} else {n_lines<-2}
    split <- ceiling(length(words)/n_lines)
    if(n_lines==2){
      sentence <- paste(words[1:split], collapse=" ")
      sentence2 <- paste(words[(split+1):length(words)], collapse=" ")
      phrase <- paste0(sentence, "</br>", sentence2)
    } else if(n_lines==3){
      sentence <- paste(words[1:split], collapse=" ")
      sentence2 <- paste(words[(split+1):(split+split)], collapse=" ")
      sentence3 <- paste(words[(split+split+1):length(words)], collapse=" ")
      phrase <- paste0(sentence, "</br>", sentence2, "</br>", sentence3)
    }
    return(phrase)
  }
  long_names <- program_names[nchar(program_names)>cutoff]
  short_names <- program_names[nchar(program_names)<=cutoff]
  if(length(long_names)>0){
    adjusted <- lapply(long_names, split_phrase)
    return(c(short_names, unlist(adjusted)))
  } else {
    return(program_names)
  }
}

# Export date
exportDate <- max(format(unique(oysterraw$ExportVersion), "%m/%d/%Y"))
# Function to set radius / circle size by # of samples (for legend)
calc_radius_oyster <- function(n){sqrt(n)}

# Load in RCP shapefiles, make valid, apply transform
rcp <- st_read(paste0(seacar_shape_location, 
                      "/orcp_all_sites/ORCP_Managed_Areas.shp")) %>%
  st_make_valid() %>% st_transform(crs = 4326)
# Load in location point and line shapefiles
GeoDBdate <- "5Mar2025"
locs_pts <- st_read(paste0(seacar_shape_location, "/SampleLocations", GeoDBdate, "/seacar_dbo_vw_SampleLocation_Point.shp")) %>%
  st_make_valid() %>% st_transform(crs = 4326)
locs_lns <- st_read(paste0(seacar_shape_location, "/SampleLocations", GeoDBdate, "/seacar_dbo_vw_SampleLocation_Line.shp")) %>%
  st_make_valid() %>% st_transform(crs = 4326)
locs_pts_rcp <- locs_pts[rcp, , op = st_intersects]
locs_lns_rcp <- locs_lns[rcp, , op = st_intersects]

###############
## FUNCTIONS ##
###############

# Allows location of shapefile for each MA
# Updated RCP shapefiles (including NCAP)
find_shape <- function(rcp, ma){return(rcp %>% filter(LONG_NAME==ma))}

# Gets coordinate min and max from shapefile
# This allows for accurately setting view on the map
get_shape_coordinates <- function(ma_shape){
  bbox_list <- lapply(st_geometry(ma_shape), st_bbox)
  maxmin <- as.data.frame(matrix(unlist(bbox_list),nrow=nrow(ma_shape)))
  names(maxmin) <- names(bbox_list[[1]])
  return(maxmin)
}

# Load in Oyster data file
oyster <- fread(
  str_subset(list.files("C:/SEACAR Data/SEACARdata/", 
                        full.names = TRUE), "OYSTER"), 
  sep="|", na.strings=c("NULL"))

# Grab all unique oyster sample locations
oyster_programs <- oyster %>% 
  group_by(ManagedAreaName, ProgramID, ProgramName, ProgramLocationID, 
           LocationID, IndicatorName, ParameterName, OriginalLatitude, OriginalLongitude) %>%
  reframe(n_data = n())

# grab sample coordinates from those programs
pt_coord_df <- locs_pts %>% filter(ProgramID %in% unique(oyster_programs$ProgramID))
ln_coord_df <- locs_lns %>% filter(ProgramID %in% unique(oyster_programs$ProgramID))
# Bind points and lines together
pt_ln_df <- bind_rows(pt_coord_df, ln_coord_df)
# Combine oyster sample location info with shapefiles (points and lines)
oyster_df <- merge(oyster_programs, pt_ln_df)

# seacar color palette
seacar_palette <- c("#964059", "#E05E7B", "#E98C86", "#F1B8AB", "#F8CAAA",
                    "#F8E6B9", "#FEEEE1", "#DAE9DA", "#8BE4C2", "#7EE7E8",
                    "#8FD0EC", "#6FA1DD", "#889BD1", "#8F83D3", "#6B59AB")

# Establish parameter / indicators which are available on Atlas
oyster_params <- data.table(
  "indicator" = c("Density", "Percent Live", "Size Class"),
  "param" = c("Density", "Percent Live", "Shell Height"),
  "indicator_short" = c("Dens", "PrcLive", "SH")
)

# Abbreviated MA names where "focus point" of map is shifted right (for legend display)
right_shift <- c("ABAP")
# Set location of legend (default top right)
legend_loc <- "topright"

for(ma in unique(oyster$ManagedAreaName)){
  # Filter data for a given MA
  oyster_df_ma <- oyster_df %>% filter(ManagedAreaName==ma)
  # Set up palette for this MA
  ma_oyster_programs <- unique(oyster_df_ma$ProgramName)
  oyster_palette <- seacar_palette[seq(1, length(seacar_palette), by = length(seacar_palette) / length(ma_oyster_programs))]
  names(oyster_palette) <- ma_oyster_programs
  # palette function
  oyster_pal <- function(x){oyster_palette[as.character(x)]}
  # Create radius from N_data column
  oyster_df_ma$rad <- calc_radius_oyster(oyster_df_ma$n_data)
  # Apply palette to each program
  oyster_df_ma$color <- oyster_pal(oyster_df_ma$ProgramName)
  # Define alpha (transparency) - make exception for programs with low amounts of data (make them less transparent)
  oyster_df_ma$alpha <- ifelse(oyster_df_ma$n_data<=10, 1, 0.6) # 0.6 is default
  # Get abbreviated MA name
  ma_abrev <- MA_All[ManagedAreaName==ma, Abbreviation]
  # Transform APAB & ANERR bubble sizes differently
  if(ma_abrev %in% c("ABAP", "ANERR")){calc_radius_oyster <- function(n){sqrt(n)/3}} 
  else {calc_radius_oyster <- function(n){sqrt(n)}}
  # locate shape file for a given MA
  ma_shape <- find_shape(rcp, ma)
  # get coordinates to set zoom level
  shape_coordinates <- get_shape_coordinates(ma_shape)
  # apply right shift
  if(ma_abrev %in% right_shift){
    shape_coordinates$xmin <- shape_coordinates$xmin + 0.2
  }
  for(ind in unique(oyster_df_ma$IndicatorName)){
    parameter <- oyster_params[indicator==ind, unique(param)]
    ind_short <- oyster_params[indicator==ind, indicator_short]
    fig_text <- tags$div(HTML(glue("{ma} - Oyster Reef - {ind} - {parameter} - Export Date: {exportDate}")),
                         style = "margin-bottom:10px;")
    oyster_df_ma_p <- oyster_df_ma %>% filter(IndicatorName==ind, ParameterName==parameter)
    # Exclude where shell height values don't have plots
    # if(ind=="Shell Height"){
    #   oyster_df_ma_p <- oyster_df_ma_p %>% filter()
    # }
    if(nrow(oyster_df_ma_p)<1) next
    labs <- shorten_program_names(unique(oyster_df_ma_p$ProgramName), cutoff=60)
    # Create map (without pts or lines for now)
    map <- leaflet(oyster_df_ma_p, options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addPolygons(data=ma_shape, color="black", weight = 1, smoothFactor = 0.5, 
                  opacity = 0.8, fillOpacity = 0.1) %>%
      addLegend(title = "Program", 
                colors=oyster_pal(unique(oyster_df_ma_p$ProgramName)), 
                labels=labs, position=legend_loc) %>%
      addLegendSymbol(title = "ORCP Boundary", 
                      color = "#000000", fillColor = "#000000", fillOpacity = 0.1,
                      values = ma, shape = "rect", position = "topright") %>%
      fitBounds(lng1=shape_coordinates$xmin,
                lat1=shape_coordinates$ymin,
                lng2=shape_coordinates$xmax,
                lat2=shape_coordinates$ymax) %>% 
      addCircleLegend(title = "Number of samples",
                      range = oyster_df_ma_p$n_data,
                      scaling_fun = calc_radius_oyster,
                      fillColor = "#b3b3b3",
                      fillOpacity = 0.8,
                      weight = 1,
                      color = "#000000",
                      position = "topright",
                      type = "oyster") %>%
      addControl(fig_text, position="bottomleft", className="map-title") %>%
      addScaleBar(position = "bottomright",
                  options = scaleBarOptions(metric=TRUE)) %>%
      addControl(
        html = '<div><img src="https://upload.wikimedia.org/wikipedia/commons/8/84/North_Pointer.svg" style="width:25px; opacity:0.6;"></div>',
        position = "bottomright",
        className = "map-title"
      )
    
    # set oyster_df_ma_p as SF geo-object
    oyster_df_ma_p <- st_as_sf(oyster_df_ma_p)
    # subsetting for lines vs points (coordinate vs transect)
    pts <- oyster_df_ma_p %>% filter(!is.na(Longitude_))
    lns <- oyster_df_ma_p %>% filter(!is.na(RawLineStr))
    
    # add transects and points where available
    if(nrow(pts)>0){
      map <- map %>%
        addCircleMarkers(data = pts,
                         lat=~Latitude_D, lng=~Longitude_,
                         weight=1, color = "#000000", stroke = TRUE,
                         fillColor = ~color,
                         radius=calc_radius_oyster(pts$n_data), 
                         fillOpacity=~alpha)
    }
    
    if(nrow(lns)>0){
      # set ln-size weighting
      ln_weight_setting <- 3
      map <- map %>%
        addPolylines(data = lns,
                     weight = calc_radius_oyster(lns$n_data)*ln_weight_setting,
                     color = ~color, smoothFactor = 0.5,
                     stroke = TRUE, opacity = ~alpha)
    }
    
    # map output filepath
    map_output <- "output/maps/"
    short_i <- gsub(" ", "", ind)
    short_p <- gsub(" ", "", parameter)
    file_name <- paste0("Oyster_", ind_short, "_", ma_abrev, "_map.png")
    map_out <- paste0(map_output, file_name)
    
    # save file as png
    mapview::mapshot(map, file = map_out, remove_controls = NULL)
    print(paste0("Map created for Oyster Reef - ", ma, " - ", ind, " - ", parameter))
  }
}

#Gets list of all image files in output/Figures and creates zip directory
map_list <- list.files("output/maps/", pattern=".png", full=FALSE)
setwd("output/maps/")
zip("OysterMaps", files=map_list)
setwd(wd)
