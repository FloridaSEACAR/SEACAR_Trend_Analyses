##### This script generates localized maps of SAV sample locations for each MA
library(leaflet)
library(leaflegend)
library(mapview)
library(htmlwidgets)
library(htmltools)
library(glue)
library(rstudioapi)
library(SEACAR)

wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

# Import pre-processed SAV data
SAV4 <- fread("output/SAV_DataUsed.txt", sep='|')

# Load shape files from load_shape_files.R
source("load_shape_files.R", echo = T)

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
  ma_shape <- SEACAR::find_shape(rcp, ma)
  
  # get coordinates to set zoom level
  shape_coordinates <- SEACAR::get_shape_coordinates(ma_shape)
  
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
    SEACAR::addCircleLegend(title = "Number of samples",
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