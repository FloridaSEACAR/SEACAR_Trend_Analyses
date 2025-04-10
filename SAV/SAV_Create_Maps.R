##### This script generates localized maps of SAV sample locations for each MA
library(leaflet)
library(leaflegend)
library(mapview)
library(htmlwidgets)
library(htmltools)
library(glue)
library(rstudioapi)

wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

# Import pre-processed SAV data
SAV4 <- fread("output/SAV_DataUsed.txt", sep='|')

# Load shape files from load_shape_files.R
source("load_shape_files.R", echo = T)

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
# Export date
exportDate <- max(format(unique(SAV4$ExportVersion), "%m/%d/%Y"))

# Setting color palette
seacar_palette <- c("#964059", "#E05E7B", "#E98C86", "#F1B8AB", "#F8CAAA",
                    "#F8E6B9", "#FEEEE1", "#DAE9DA", "#8BE4C2", "#7EE7E8",
                    "#8FD0EC", "#6FA1DD", "#889BD1", "#8F83D3", "#6B59AB")

# The most commonly-occurring programs (if in more than half of MAs) will get the same color across all MAs
freq_programs <- SAV4 %>% 
  group_by(ProgramID, ProgramName) %>% 
  reframe(count=length(unique(ManagedAreaName))) %>% 
  filter(count >= max(count) * 0.5) %>%
  pull(ProgramName) %>% sort()

# Select colors to manually assign, selecting every other color
manual_colors <- seacar_palette[seq(1, length(seacar_palette), by = length(seacar_palette) / length(freq_programs))]
# Name them according to the color values
names(manual_colors) <- freq_programs

sav_maps <- function(ma, ma_abrev){
  # Grab a list of programs within SAV data for each MA
  sav_programs <- SAV4 %>% filter(ManagedAreaName == ma) %>% distinct(ProgramID, ProgramName)
  sav_programs$ProgramID <- as.numeric(sav_programs$ProgramID)
  
  # Identify the values that are not manually assigned a palette color
  remaining_values <- setdiff(sav_programs$ProgramName, freq_programs)
  
  # Use remaining colors for these values
  base_remaining <- seacar_palette[!seacar_palette %in% unname(manual_colors)]
  if(length(remaining_values) > length(base_remaining)){
    remaining_palette <- colorRampPalette(base_remaining)(length(remaining_values))
  } else {
    remaining_palette <- base_remaining[seq(from=1, to=length(base_remaining), length.out = length(remaining_values))]
  }
  # Name remaining palette
  names(remaining_palette) <- remaining_values
  # Combine all values into a single palette
  full_mapping <- c(manual_colors, remaining_palette)
  # Create color-lookup palette function
  sav_pal <- function(x){full_mapping[as.character(x)]}
  
  # grab sample coordinates from those programs
  pt_coord_df <- locs_pts_rcp %>% filter(ProgramID %in% sav_programs$ProgramID)
  ln_coord_df <- locs_lns_rcp %>% filter(ProgramID %in% sav_programs$ProgramID)
  
  # frame to plot coordinates, allows for bubble size display of n_samples
  # grouping by LocationID yields better results, PLID doesn't always match (BBAP)
  sav_df <- SAV4 %>% filter(ManagedAreaName == ma, ProgramID %in% sav_programs$ProgramID) %>%
    group_by(LocationID) %>%
    reframe(n_data = n())
  
  # Combine point and transect spatial df
  pt_ln_df <- bind_rows(pt_coord_df, ln_coord_df)
  
  # merge sav data with spatial data
  sav_df <- merge(sav_df, pt_ln_df)
  sav_df <- sav_df[order(sav_df$n_data, decreasing=TRUE), ]
  
  # locate shape file for a given MA
  ma_shape <- find_shape(rcp, ma)
  
  # get coordinates to set zoom level
  shape_coordinates <- get_shape_coordinates(ma_shape)
  
  # Function to set radius / circle size by # of samples (for legend)
  # LRLWCAP has bi-monthly monitoring, apply more severe transformation
  if(ma_abrev %in% c("LRLWCAP", "IRVBFPAP", "IRMVBAP", "BRAP", "JBJIAP")){
    calc_radius_sav <- function(n){sqrt(n)/3}
  } else {
    calc_radius_sav <- function(n){sqrt(n)}
  }

  # Add color column to apply palette
  sav_df$color <- sav_pal(sav_df$ProgramName)
  sav_df <- sav_df %>% arrange(match(color, seacar_palette))
  # Define alpha (transparency) - make exception for programs with low amounts of data (make them less transparent)
  sav_df$alpha <- ifelse(sav_df$n_data<=10, 1, 0.8) # 0.8 is default
  
  # Set up watermark text display
  fig_text <- tags$div(HTML(glue("{ma} - Submerged Aquatic Vegetation - Percent Cover (by species) - Percent Cover - Export Date: {exportDate}")),
                       style = "margin-bottom:10px;")
  
  # create empty map template with shape file
  # previous shape col - #4E809C
  map <- leaflet(sav_df, options = leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
    addPolygons(data=ma_shape, color="black", weight = 1, smoothFactor = 0.5, 
                opacity = 0.8, fillOpacity = 0.1) %>%
    addLegend(title = "Program", 
              colors=sav_pal(unique(sav_df$ProgramName)), 
              labels=unique(sav_df$ProgramName),
              position = "topright") %>%
    addLegendSymbol(title = "ORCP Boundary", 
                    color = "#000000", fillColor = "#000000", fillOpacity = 0.1,
                    values = ma, shape = "rect", position = "topright") %>%
    fitBounds(lng1=shape_coordinates$xmin,
              lat1=shape_coordinates$ymin,
              lng2=shape_coordinates$xmax,
              lat2=shape_coordinates$ymax) %>%
    addControl(fig_text, position="bottomleft", className = "map-title") %>%
    addScaleBar(position = "bottomright",
                options = scaleBarOptions(metric=TRUE)) %>%
    addControl(
      html = '<div><img src="https://upload.wikimedia.org/wikipedia/commons/8/84/North_Pointer.svg" style="width:25px; opacity:0.6;"></div>',
      position = "bottomright",
      className = "map-title"
    )
  
  # set sav_df as SF geo-object
  sav_df <- st_as_sf(sav_df)
  
  # subsetting for lines vs points (coordinate vs transect)
  pts <- sav_df %>% filter(!is.na(Longitude_))
  lns <- sav_df %>% filter(!is.na(RawLineStr))
  
  # add points where available
  if(nrow(pts)>0){
    map <- map %>%
      addCircleMarkers(data = pts,
                       lat=~Latitude_D, lng=~Longitude_,
                       weight=1, color = "#000000", stroke = TRUE,
                       fillColor = ~color,
                       radius=calc_radius_sav(pts$n_data), 
                       fillOpacity=~alpha)
  }
  # add transects where available
  if(nrow(lns)>0){
    map <- map %>%
      addPolylines(data = lns,
                   weight = calc_radius_sav(lns$n_data)*2,
                   color = ~color, smoothFactor = 0.5,
                   stroke = TRUE, opacity = ~alpha)
  }
  # Add circle legends, using combined range (points + transects)
  map <- map %>%
    addCircleLegend(title = "Number of samples",
                    range = c(lns$n_data, pts$n_data),
                    scaling_fun = calc_radius_sav,
                    fillColor = "#b3b3b3",
                    fillOpacity = 0.8,
                    weight = 1,
                    color = "#000000",
                    position = "topright",
                    type = "sav")
  
  # set map output location
  map_output <- "output/maps/"
  # map output filepath
  map_out <- paste0(map_output, "SAV_", ma_abrev, "_map.png")
  
  # save file as png
  mapshot(map, file = map_out, remove_controls = NULL)
}

for(ma in unique(SAV4$ManagedAreaName)){
  ma_abrev <- MA_All[ManagedAreaName==ma, Abbreviation]
  sav_maps(ma = ma, ma_abrev = ma_abrev)
  print(paste0("Map saved for ", ma))
}

#Gets list of all image files in output/Figures and creates zip directory
map_list <- list.files("output/maps/", pattern=".png", full=FALSE)
setwd("output/maps/")
zip("SAVMaps", files=map_list)
setwd(wd)