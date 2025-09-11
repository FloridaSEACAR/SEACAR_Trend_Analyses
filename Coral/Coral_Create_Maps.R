# Creates Coral Reef sample location maps (SEACAR Atlas)
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
exportDate <- max(format(unique(coral_sr_data$ExportVersion), "%m/%d/%Y"),
                  format(unique(coral_pc_data$ExportVersion), "%m/%d/%Y"))
# Function to set radius / circle size by # of samples (for legend)
calc_radius_coral <- function(n){sqrt(n)}

# Load in RCP shapefiles
rcp <- SEACAR::GeoData$`RCP Boundaries`
locs_pts <- SEACAR::GeoData$pointLocations
locs_lns <- SEACAR::GeoData$lineLocations
locs_pts_rcp <- locs_pts[rcp, , op = st_intersects]
locs_lns_rcp <- locs_lns[rcp, , op = st_intersects]

# Grab all unique Coral SpeciesRichness sample locations
coral_sr_programs <- coral_sr_data %>% 
  group_by(ManagedAreaName, ProgramID, ProgramName, ProgramLocationID, 
           LocationID, IndicatorName, ParameterName, OriginalLatitude, OriginalLongitude) %>%
  reframe(n_data = n())

coral_pc_programs <- coral_pc_data %>% 
  group_by(ManagedAreaName, ProgramID, ProgramName, ProgramLocationID, 
           LocationID, IndicatorName, ParameterName, OriginalLatitude, OriginalLongitude) %>%
  reframe(n_data = n())

# grab sample coordinates from those programs
pt_coord_df <- locs_pts %>% filter(ProgramID %in% unique(coral_pc_programs$ProgramID))
ln_coord_df <- locs_lns %>% filter(ProgramID %in% unique(coral_pc_programs$ProgramID))
# Bind points and lines together
pt_ln_df <- bind_rows(pt_coord_df, ln_coord_df)
# Combine coral sample location info with shapefiles (points and lines)
coral_pc_df <- merge(coral_pc_programs, pt_ln_df)
coral_sr_df <- merge(coral_sr_programs, pt_ln_df)

# seacar color palette #2
seacar_palette <- SEACAR::seacar_palette2

# Abbreviated MA names where "focus point" of map is shifted right (for legend display)
right_shift <- c("KJCAP")
# Legend display alteration
top_left <- c("BBAP", "BBCFMCAP", "CBAP", "FKNMS")
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
  
for(i in c("Percent Cover", "Species Richness")){
  # Ensure correct file subset is being used for map generation, filter for a given MA
  if(i=="Percent Cover"){
    coral_df <- coral_pc_df
    ma_include <- coral_pc_MA_All
  } else {
    coral_df <- coral_sr_df
    ma_include <- coral_sr_MA_Include
  }
  p <- ifelse(i=="Percent Cover", i, "Presence/Absence")
  ind <- ifelse(i=="Percent Cover", i, "Grazers and Reef Dependent Species")
  for(ma in ma_include){
    if(is.na(ma)) next
    # Get abbreviated MA name
    ma_abrev <- MA_All[ManagedAreaName==ma, Abbreviation]
    # locate shape file for a given MA
    ma_shape <- SEACAR::find_shape(rcp, ma)
    # get coordinates to set zoom level
    shape_coordinates <- SEACAR::get_shape_coordinates(ma_shape)
    if(ma_abrev %in% right_shift){
      shape_coordinates$xmin <- shape_coordinates$xmin + 1
    }
    # Set legend location variable
    legend_loc <- ifelse(ma_abrev %in% top_left, "topleft", "topright")
    # Subset data for indicator, parameter, ma
    coral_df_ma <- coral_df %>% 
      filter(IndicatorName==ind, ParameterName==p, ManagedAreaName==ma)
    if(nrow(coral_df_ma)==0) next
    ma_coral_programs <- unique(coral_df_ma$ProgramName)
    # Palette
    coral_palette <- seacar_palette[seq(1, length(seacar_palette), by = length(seacar_palette) / length(ma_coral_programs))]
    names(coral_palette) <- ma_coral_programs
    coral_pal <- function(x){coral_palette[as.character(x)]}
    
    # Create radius from N_data column
    coral_df_ma$rad <- calc_radius_coral(coral_df_ma$n_data)
    # Apply palette to each program
    coral_df_ma$color <- coral_pal(coral_df_ma$ProgramName)
    # Define alpha (transparency) - make exception for programs with low amounts of data (make them less transparent)
    coral_df_ma$alpha <- ifelse(coral_df_ma$n_data<=10, 1, 0.6) # 0.6 is default
    
    # Apply linebreaks to long program names
    labs <- shorten_program_names(ma_coral_programs, cutoff=60)
    
    # Set up watermark text display
    fig_text <- tags$div(HTML(glue("{ma} - Coral Reef - {i} - {p} - Export Date: {exportDate}")),
                         style = "margin-bottom:10px;")
    
    # Create map (without pts or lines for now)
    map <- leaflet(coral_df_ma, options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addPolygons(data=ma_shape, color="black", weight = 1, smoothFactor = 0.5, 
                  opacity = 0.8, fillOpacity = 0.1) %>%
      addLegend(title = "Program", 
                colors=coral_pal(unique(coral_df_ma$ProgramName)), 
                labels=labs, position=legend_loc) %>%
      addLegendSymbol(title = "ORCP Boundary", 
                      color = "#000000", fillColor = "#000000", fillOpacity = 0.1,
                      values = ma, shape = "rect", position = "topright") %>%
      fitBounds(lng1=shape_coordinates$xmin,
                lat1=shape_coordinates$ymin,
                lng2=shape_coordinates$xmax,
                lat2=shape_coordinates$ymax) %>% 
      SEACAR::addCircleLegend(title = "Number of samples",
                              range = coral_df_ma$n_data,
                              scaling_fun = calc_radius_coral,
                              fillColor = "#b3b3b3",
                              fillOpacity = 0.8,
                              weight = 1,
                              color = "#000000",
                              position = "topright",
                              type = "coral") %>%
      addControl(fig_text, position="bottomleft", className="map-title") %>%
      addScaleBar(position = "bottomright",
                  options = scaleBarOptions(metric=TRUE)) %>%
      addControl(
        html = '<div><img src="https://upload.wikimedia.org/wikipedia/commons/8/84/North_Pointer.svg" style="width:25px; opacity:0.6;"></div>',
        position = "bottomright",
        className = "map-title"
      )
    
    # set coral_df_ma as SF geo-object
    coral_df_ma <- st_as_sf(coral_df_ma)
    # subsetting for lines vs points (coordinate vs transect)
    pts <- coral_df_ma %>% filter(!is.na(Longitude_))
    lns <- coral_df_ma %>% filter(!is.na(RawLineStr))
    
    # add transects and points where available
    if(nrow(pts)>0){
      map <- map %>%
        addCircleMarkers(data = pts,
                         lat=~Latitude_D, lng=~Longitude_,
                         weight=1, color = "#000000", stroke = TRUE,
                         fillColor = ~color,
                         radius=calc_radius_coral(pts$n_data), 
                         fillOpacity=~alpha)
    }
    
    if(nrow(lns)>0){
      map <- map %>%
        addPolylines(data = lns,
                     weight = calc_radius_coral(lns$n_data),
                     color = "#000000", stroke = TRUE,
                     fillColor = ~color, smoothFactor = 0.5, opacity = ~alpha)
    }
    
    # map output filepath
    map_output <- "output/maps/"
    short_i <- ifelse(i=="Percent Cover", "PC", "SpeciesRichness")
    file_name <- paste0("Coral_", short_i, "_", ma_abrev, "_map.png")
    map_out <- paste0(map_output, file_name)
    
    # save file as png
    mapview::mapshot(map, file = map_out, remove_controls = NULL)
    print(paste0("Map created for Coral Reef - ", ma, " - ", i, " - ", p))    
  }
}

#Gets list of all image files in output/Figures and creates zip directory
map_list <- list.files("output/maps/", pattern=".png", full=FALSE)
setwd("output/maps/")
zip("CoralMaps", files=map_list)
setwd(wd)
