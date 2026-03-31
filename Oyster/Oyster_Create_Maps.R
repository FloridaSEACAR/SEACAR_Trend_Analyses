# Creates Oyster Reef sample location maps (SEACAR Atlas)
library(sf)
library(leaflet)
library(leaflegend)
library(htmlwidgets)
library(htmltools)
library(glue)
library(rstudioapi)
library(SEACAR)

wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

source("../SEACAR_data_location.R")

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

# Load in Oyster data file
oyster <- fread(str_subset(list.files(seacar_data_location, full.names = TRUE), "OYSTER"), 
                sep="|", na.strings=c("NULL"))

# Export date
exportDate <- max(format(unique(oyster$ExportVersion), "%m/%d/%Y"))
# Function to set radius / circle size by # of samples (for legend)
calc_radius_oyster <- function(n){sqrt(n)}

# Load in RCP shapefiles, make valid, apply transform
rcp <- SEACAR::GeoData$`RCP Boundaries`
# Load in OIMMP shapefiles (***TEMP: add to SEACAR package)
oimmp_boundaries <- sf::st_read("C:/SEACAR Data/SEACARshapes/RCP/BoundaryUpdate2025oct3/ORCP_MA_Coral_MAbuff_CHIMMP_OIMMP_2025oct3.shp") %>% 
  rename("OIMMP" = "Region") %>% st_make_valid() %>% st_transform(crs = 4326) %>%
  group_by(OIMMP) %>% summarise()
oimmp_boundaries <- oimmp_boundaries %>% filter(!OIMMP=="9999")
# Load in location point and line shapefiles
locs_pts <- SEACAR::GeoData$pointLocations
locs_lns <- SEACAR::GeoData$lineLocations
if(analysis=="ma"){
  locs_pts_rcp <- locs_pts[rcp, , op = st_intersects]
  locs_lns_rcp <- locs_lns[rcp, , op = st_intersects]
} else {
  locs_pts_rcp <- locs_pts[oimmp_boundaries, , op = st_intersects]
  locs_lns_rcp <- locs_lns[oimmp_boundaries, , op = st_intersects]
}

# Declare column name for proper retrieval by analysis type
analysis_col <- ifelse(analysis=="ma", "ManagedAreaName", "OIMMP")

# Grab all unique oyster sample locations
oyster_programs <- oyster %>% 
  group_by(!!sym(analysis_col), ProgramID, ProgramName, ProgramLocationID, 
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
seacar_palette <- SEACAR::seacar_palette2

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

for(loc in unique(oyster[[analysis_col]])){
  if(is.na(loc)) next
  # Filter data for a given loc
  oyster_df_ma <- oyster_df %>% filter(!!sym(analysis_col)==loc)
  # Set up palette for this loc
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
  # Get abbreviated MA name, if OIMMP use OIMMP name (with underscores)
  ma_abrev <- ifelse(analysis=="ma", MA_All[ManagedAreaName==loc, Abbreviation], gsub(" ", "_", loc))
  
  # # Transform APAB & ANERR bubble sizes differently
  # if(ma_abrev %in% c("ABAP", "ANERR", "Apalachicola_Bay")){
  #   calc_radius_oyster <- function(n){sqrt(n)/3}
  # } else {
  #   calc_radius_oyster <- function(n){sqrt(n)}
  # }
  calc_radius_oyster <- function(n){log(n)}
  
  # locate shape file for a given loc
  if(analysis=="ma"){
    ma_shape <- SEACAR::find_shape(rcp, loc)
  } else {
    ma_shape <- oimmp_boundaries %>% filter(OIMMP==loc)
  }
  for(ind in unique(oyster_df_ma$IndicatorName)){
    parameter <- oyster_params[indicator==ind, unique(param)]
    ind_short <- oyster_params[indicator==ind, indicator_short]
    if(analysis=="ma"){
      fig_text <- tags$div(HTML(glue("{loc} - Oyster Reef - {ind} - {parameter} - Export Date: {exportDate}")),
                           style = "margin-bottom:10px;")
    } else {
      fig_text <- tags$div(HTML(glue("OIMMP: {loc} - Oyster Reef - {ind} - {parameter} - Export Date: {exportDate}")),
                           style = "margin-bottom:10px;")
    }
    oyster_df_ma_p <- oyster_df_ma %>% filter(IndicatorName==ind, ParameterName==parameter)
    
    # get coordinates to set zoom level
    # OIMMP: center around points, MA: center around MA boundary
    if(analysis=="ma"){
      shape_coordinates <- SEACAR::get_shape_coordinates(ma_shape)
    } else {
      shape_coordinates <- data.frame(xmin = min(oyster_df_ma_p$OriginalLongitude),
                                      ymin = min(oyster_df_ma_p$OriginalLatitude),
                                      xmax = max(oyster_df_ma_p$OriginalLongitude), 
                                      ymax = max(oyster_df_ma_p$OriginalLatitude))
    }
    
    # apply right shift
    if(ma_abrev %in% right_shift){
      shape_coordinates$xmin <- shape_coordinates$xmin + 0.2
    }
    
    # Exclude where shell height values don't have plots
    # if(ind=="Shell Height"){
    #   oyster_df_ma_p <- oyster_df_ma_p %>% filter()
    # }
    if(nrow(oyster_df_ma_p)<1) next
    progs <- sort(unique(oyster_df_ma_p$ProgramName))
    labs <- shorten_program_names(progs, cutoff=60)
    # Title for boundary based on analysis type
    boundary_title <- ifelse(analysis=="ma", "ORCP Boundary", "OIMMP Boundary")
    # Create map (without pts or lines for now)
    map <- leaflet(oyster_df_ma_p, options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addPolygons(data=ma_shape, color="black", weight = 1, smoothFactor = 0.5, 
                  opacity = 0.8, fillOpacity = 0.1) %>%
      addLegend(title = "Program", 
                colors=oyster_pal(progs), 
                labels=labs, position=legend_loc) %>%
      addLegendSymbol(title = boundary_title, 
                      color = "#000000", fillColor = "#000000", fillOpacity = 0.1,
                      values = loc, shape = "rect", position = "topright") %>%
      fitBounds(lng1=shape_coordinates$xmin,
                lat1=shape_coordinates$ymin,
                lng2=shape_coordinates$xmax,
                lat2=shape_coordinates$ymax) %>% 
      SEACAR::addCircleLegend(title = "Number of samples",
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
    
    # Add inset map to OIMMP maps
    if(analysis=="oimmp"){
      map <- map %>%
        addMiniMap(centerFixed = c(mean(oyster_df_ma_p$OriginalLatitude),mean(oyster_df_ma_p$OriginalLongitude)), 
                   zoomLevelOffset = -5, 
                   position = 'topleft', 
                   tiles = providers$CartoDB.PositronNoLabels)
    }
    
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
    map_output <- ifelse(analysis=="ma", "output/maps/", "output/maps/OIMMP/")
    short_i <- gsub(" ", "", ind)
    short_p <- gsub(" ", "", parameter)
    file_name <- paste0("Oyster_", ind_short, "_", ma_abrev, "_map.png")
    map_out <- paste0(map_output, file_name)
    
    # save file as png
    mapview::mapshot(map, file = map_out, remove_controls = NULL)
    print(paste0("Map created for Oyster Reef - ", loc, " - ", ind, " - ", parameter))
  }
}

# Create OIMMP boundary overview for OIMMP Reports
if(analysis=="OIMMP"){
  temp_oimmp_boundaries <- oimmp_boundaries
  # Order OIMMP boundaries from NW to NE
  temp_oimmp_boundaries$OIMMP <- factor(temp_oimmp_boundaries$OIMMP, levels = c(
    "Northwest Florida", "Apalachicola Bay", "Big Bend", "Tampa and Sarasota Bays",
    "Southwest Florida", "Biscayne Bay and Florida Keys", "Central and Southeast Florida", "Northeast Florida"
  ))
  # arrange for display on map + legend
  temp_oimmp_boundaries <- temp_oimmp_boundaries[order(temp_oimmp_boundaries$OIMMP), ] 
  # palette function
  oimmp_pal <- colorFactor(oimmp_palette, unique(temp_oimmp_boundaries$OIMMP))
  # Create map
  oimmp_map <- leaflet(temp_oimmp_boundaries, options = leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
    addPolygons(data=temp_oimmp_boundaries, color = "black", fillColor=~oimmp_pal(OIMMP), 
                weight = 1, smoothFactor = 0.5, 
                opacity = 1, fillOpacity = 0.5) %>%
    addLegend(title = "OIMMP Boundary",
              colors=oimmp_pal(unique(temp_oimmp_boundaries$OIMMP)),
              labels=unique(temp_oimmp_boundaries$OIMMP)) %>%
    addControl("OIMMP Boundaries in SEACAR", position="bottomleft", className="map-title") %>%
    addScaleBar(position = "bottomright",
                options = scaleBarOptions(metric=TRUE)) %>%
    addControl(
      html = '<div><img src="https://upload.wikimedia.org/wikipedia/commons/8/84/North_Pointer.svg" style="width:25px; opacity:0.6;"></div>',
      position = "bottomright",
      className = "map-title"
    )
  # save file as png
  mapview::mapshot(oimmp_map, file = paste0(map_output, "/OIMMP_Regions.png"), remove_controls = NULL)
  rm(temp_oimmp_boundaries)
}

#Gets list of all image files in output/Figures and creates zip directory
map_list <- list.files(map_output, pattern=".png", full=FALSE)
setwd(map_output)
zip(ifelse(analysis=="oimmp", "OysterMaps_OIMMP", "OysterMaps"), files=map_list)
setwd(wd)
