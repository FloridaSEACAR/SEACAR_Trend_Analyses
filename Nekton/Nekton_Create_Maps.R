# Creates Nekton sample location maps (SEACAR Atlas)
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
# Export date
exportDate <- max(format(unique(nekton$ExportVersion), "%m/%d/%Y"))
# Function to set radius / circle size by # of samples (for legend)
calc_radius_nekton <- function(n){sqrt(n)/2}

# Load in RCP shapefiles, make valid, apply transform
rcp <- SEACAR::GeoData$`RCP Boundaries`
# Load in location point and line shapefiles
locs_pts <- SEACAR::GeoData$pointLocations
locs_lns <- SEACAR::GeoData$lineLocations
locs_pts_rcp <- locs_pts[rcp, , op = st_intersects]
locs_lns_rcp <- locs_lns[rcp, , op = st_intersects]

# Grab all unique Coral sample locations
nekton_programs <- nekton %>% 
  group_by(ManagedAreaName, ProgramID, ProgramName, ProgramLocationID, 
           LocationID, IndicatorName, ParameterName, OriginalLatitude, OriginalLongitude) %>%
  reframe(n_data = n())

# grab sample coordinates from those programs
pt_coord_df <- locs_pts %>% filter(ProgramID %in% unique(nekton_programs$ProgramID))
ln_coord_df <- locs_lns %>% filter(ProgramID %in% unique(nekton_programs$ProgramID))
# Bind points and lines together
pt_ln_df <- bind_rows(pt_coord_df, ln_coord_df)
# Combine coral sample location info with shapefiles (points and lines)
nekton_df <- merge(nekton_programs, pt_ln_df)

# seacar color palette
seacar_palette <- SEACAR::seacar_palette2

all_nekton_programs <- unique(nekton$ProgramName)
nekton_palette <- seacar_palette[seq(1, length(seacar_palette), by = length(seacar_palette) / length(all_nekton_programs))]
names(nekton_palette) <- all_nekton_programs
# palette function
nekton_pal <- function(x){nekton_palette[as.character(x)]}

shorten_program_names <- function(program_names, cutoff=50){
  split_phrase <- function(p_name){
    words <- str_split(p_name, pattern=coll(" "))[[1]]
    split <- length(words)/2
    sentence <- paste(words[1:split], collapse=" ")
    sentence2 <- paste(words[split+1:split], collapse=" ")
    phrase <- paste0(sentence, "</br>", sentence2)
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

for(ma in unique(nekton$ManagedAreaName)){
  # Filter data for a given MA
  nekton_df_ma <- nekton_df %>% filter(ManagedAreaName==ma)
  # Unique programs for a given MA
  ma_nekton_programs <- unique(nekton_df_ma$ProgramName)
  # Create radius from N_data column
  nekton_df_ma$rad <- sqrt(nekton_df_ma$n_data)
  # Apply palette to each program
  nekton_df_ma$color <- nekton_pal(nekton_df_ma$ProgramName)
  # Define alpha (transparency) - make exception for programs with low amounts of data (make them less transparent)
  nekton_df_ma$alpha <- ifelse(nekton_df_ma$n_data<=10, 1, 0.6) # 0.6 is default
  # Get abbreviated MA name
  ma_abrev <- MA_All[ManagedAreaName==ma, Abbreviation]
  # locate shape file for a given MA
  ma_shape <- SEACAR::find_shape(rcp, ma)
  # get coordinates to set zoom level
  shape_coordinates <- SEACAR::get_shape_coordinates(ma_shape)
  # Set legend location
  legend_loc <- "topright"
  # Apply linebreaks to long program names
  labs <- shorten_program_names(ma_nekton_programs, cutoff=60)
  
  # Set up watermark text display
  ind <- "Nekton"
  parameter <- "Presence/Absence"
  fig_text <- tags$div(HTML(glue("{ma} - Water Column - {ind} - {parameter} - Export Date: {exportDate}")),
                       style = "margin-bottom:10px;")
  
  # Create map (without pts or lines for now)
  map <- leaflet(nekton_df_ma, options = leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
    addPolygons(data=ma_shape, color="black", weight = 1, smoothFactor = 0.5, 
                opacity = 0.8, fillOpacity = 0.1) %>%
    addLegend(title = "Program", 
              colors=nekton_pal(unique(nekton_df_ma$ProgramName)), 
              labels=labs, position=legend_loc) %>%
    addLegendSymbol(title = "ORCP Boundary", 
                    color = "#000000", fillColor = "#000000", fillOpacity = 0.1,
                    values = ma, shape = "rect", position = "topright") %>%
    fitBounds(lng1=shape_coordinates$xmin,
              lat1=shape_coordinates$ymin,
              lng2=shape_coordinates$xmax,
              lat2=shape_coordinates$ymax) %>%
    SEACAR::addCircleLegend(title = "Number of samples",
                            range = nekton_df_ma$n_data,
                            scaling_fun = calc_radius_nekton,
                            fillColor = "#b3b3b3",
                            fillOpacity = 0.8,
                            weight = 1,
                            color = "#000000",
                            position = "topright",
                            type = "nekton") %>%
    addControl(fig_text, position="bottomleft", className="map-title") %>%
    addScaleBar(position = "bottomright",
                options = scaleBarOptions(metric=TRUE)) %>%
    addControl(
      html = '<div><img src="https://upload.wikimedia.org/wikipedia/commons/8/84/North_Pointer.svg" style="width:25px; opacity:0.6;"></div>',
      position = "bottomright",
      className = "map-title"
    )
  
  # set nekton_df_ma as SF geo-object
  nekton_df_ma <- st_as_sf(nekton_df_ma)
  # subsetting for lines vs points (coordinate vs transect)
  pts <- nekton_df_ma %>% filter(!is.na(Longitude_))
  lns <- nekton_df_ma %>% filter(!is.na(RawLineStr))
  
  # add transects and points where available
  if(nrow(pts)>0){
    map <- map %>%
      addCircleMarkers(data = pts,
                       lat=~Latitude_D, lng=~Longitude_,
                       weight=1, color = "#000000", stroke = TRUE,
                       fillColor = ~color,
                       radius=calc_radius_nekton(pts$n_data), 
                       fillOpacity=~alpha)
  }
  
  if(nrow(lns)>0){
    # set ln-size weighting
    ln_weight_setting <- 3
    map <- map %>%
      addPolylines(data = lns,
                   weight = calc_radius_nekton(lns$n_data)*ln_weight_setting,
                   color = ~color, smoothFactor = 0.5,
                   stroke = TRUE, opacity = ~alpha)
  }
  
  # map output filepath
  map_output <- "output/maps/"
  file_name <- paste0("Nekton_", ma_abrev, "_map.png")
  map_out <- paste0(map_output, file_name)
  
  # save file as png
  mapview::mapshot(map, file = map_out, remove_controls = NULL)
  print(paste0("Map created for Nekton - ", ma))
}

#Gets list of all image files in output/Figures and creates zip directory
map_list <- list.files("output/maps/", pattern=".png", full=FALSE)
setwd("output/maps/")
zip("NektonMaps", files=map_list)
setwd(wd)
