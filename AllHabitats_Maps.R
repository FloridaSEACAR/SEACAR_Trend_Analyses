# Create sample location maps for all habitats(SEACAR Atlas)
library(data.table)
library(dplyr)
library(sf)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(glue)
library(leaflegend)
library(rstudioapi)
library(SEACAR)
library(stringr)

# analysis <- "ma"

# hab <- "Coastal Wetlands"
# hab <- "Coral Reef"
# hab <- "Nekton"
# hab <- "Oyster Reef"
# hab <- "Submerged Aquatic Vegetation"

# Add ma boundaries, optionally buffer for CW
add_buff_boundary <- TRUE

addCircleLegend_temp <- function(
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
  if(range_size=="small"){
    n_range<-max_n
  } else if(range_size=="medium"){
    n_range<-c(med_n, max_n)
  } else {
    n_range<-c(min_n, med_n, max_n)
  }
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

# Set working directory to SEACAR_Trend_Analyses root folder
setwd(this.path::here())

# Create data frame to store habitat crosswalk
habs <- data.table(
  "Habitat" = c("Coastal Wetlands", "Coral Reef", "Nekton", "Oyster Reef", "Submerged Aquatic Vegetation"),
  "HabitatShort" = c("CW", "Coral", "Nekton", "Oyster", "SAV"),
  "HabitatFile" = c("CW", "CORAL", "NEKTON", "OYSTER", "SAV"),
  "HabitatFolder" = c("Coastal_Wetlands", "Coral", "Nekton", "Oyster", "SAV"),
  "HabitatZip" = c("CoastalWetlandsMaps", "CoralMaps", "NektonMaps", "OysterMaps", "SAVMaps")
)
# Declare output location for map exports
output_loc <- paste0(habs[Habitat==hab, HabitatFolder], "/output/maps/")

# Remove all previous maps from this location before rendering new maps
map_files <- list.files(output_loc, full=T)
file.remove(map_files)

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
# Add a border around legend boxes
border_style <- tags$style(HTML("
  .leaflet .legend i {
    border: 1px solid #000000;
  }
"))

# Read in correct file 
all_files <- list.files(seacar_data_location, full=T)
file <- str_subset(all_files, habs[Habitat==hab, HabitatFile])
# remove concatenated MA names, determine which col to target
if(hab=="Coastal Wetlands"){
  data <- cw
} else if(hab=="Nekton"){
  data <- nekton
} else if(hab=="Submerged Aquatic Vegetation"){
  data <- fread("SAV/output/SAV_DataUsed.txt", sep='|')
} else {
  data <- fread(file, sep = "|", na.strings = "NULL") %>% SEACAR::clean_managed_areas(type = "ma")
}

# Export date
exportDate <- max(format(unique(data$ExportVersion), "%m/%d/%Y"))

# Load in RCP shapefiles, make valid, apply transform
rcp <- SEACAR::GeoData$`RCP Boundaries`
# Load in OIMMP shapefiles
oimmp_boundaries <- SEACAR::GeoData$OIMMP
# Load in CW-buffered boundaries
ma_buff_boundaries <- ma_buff_boundaries <- st_read("C:/SEACAR Data/SEACARshapes/BoundaryUpdate2025oct3/ORCP_MA_Coral_MAbuff_CHIMMP_OIMMP_2025oct3.shp") %>%
  filter(MA_NameBuf!="9999") %>% 
  group_by(MA_NameBuf, MA_AreaIDB) %>% 
  summarise(geometry = st_union(geometry)) %>%
  st_make_valid() %>% st_transform(4326)

# Declare column name for proper retrieval by analysis type
analysis_col <- ifelse(analysis=="ma", "ManagedAreaName", "OIMMP")
# List of columns to group by
if(hab=="Submerged Aquatic Vegetation"){
  cols_to_group <- c(analysis_col, "ProgramID", "ProgramName", "ProgramLocationID", 
                     "LocationID", "IndicatorName", "OriginalLatitude", "OriginalLongitude")
} else {
  cols_to_group <- c(analysis_col, "ProgramID", "ProgramName", "ProgramLocationID", 
                     "LocationID", "IndicatorName", "ParameterName", "OriginalLatitude", "OriginalLongitude")
}

# Grab all unique sample locations
all_programs <- data %>% 
  group_by(across(all_of(cols_to_group))) %>%
  reframe(n_data = n())

# Convert to SF
df <- all_programs %>% sf::st_as_sf(coords = c("OriginalLongitude", "OriginalLatitude"), crs = 4326)

# # grab sample coordinates from those programs
# pt_coord_df <- locs_pts %>% filter(ProgramID %in% unique(all_programs$ProgramID))
# ln_coord_df <- locs_lns %>% filter(ProgramID %in% unique(all_programs$ProgramID))
# # Bind points and lines together
# pt_ln_df <- bind_rows(pt_coord_df, ln_coord_df)
# # Combine all sample location info with shapefiles (points and lines)
# df <- merge(all_programs, pt_ln_df)

##### Color palette set up -----
# seacar color palette
seacar_palette <- c("#964059", "#E05E7B", "#E98C86", "#F1B8AB", "#F8CAAA",
                    "#F8E6B9", "#FEEEE1", "#DAE9DA", "#8BE4C2", "#7EE7E8",
                    "#8FD0EC", "#6FA1DD", "#889BD1", "#8F83D3", "#6B59AB")
# Determine programs which occur most frequently
all_programs <- df %>%
  group_by(ProgramID, ProgramName) %>%
  reframe(count = length(unique(ManagedAreaName)))

if(nrow(all_programs)<=15){
  freq_programs <- all_programs %>% pull(ProgramName) %>% sort()
} else {
  freq_programs <- all_programs %>% filter(count >= (max(count)*0.5)) %>% pull(ProgramName) %>% sort()
}

# Select colors to manually assign, selecting roughly every other color
manual_colors <- seacar_palette[seq(1, length(seacar_palette), by = length(seacar_palette) / length(freq_programs))]
# Name them according to the color values
names(manual_colors) <- freq_programs
# Function to select n colors that are farthest from fixed_colors
# After the most frequent programs are determined above, all other programs are assigned
# colors within their MA which are farthest from the pre-determined colors in "manual_colors"
pick_far_colors <- function(candidate_colors, fixed_colors, n){
  if(n==0) return(character(0))
  if(length(candidate_colors) == 0) stop("No candidate colors available.")
  
  # If more colors are needed than available, interpolate a larger candidate set
  if(n > length(candidate_colors)){
    candidate_colors <- colorRampPalette(candidate_colors)(n * 5)
  }
  # Remove any accidental overlap with fixed colors
  candidate_colors <- candidate_colors[!candidate_colors %in% fixed_colors]
  
  # Convert hex colors to LAB
  to_lab <- function(cols){
    rgb <- t(grDevices::col2rgb(cols) / 255)
    grDevices::convertColor(rgb, from = "sRGB", to = "Lab")
  }
  selected <- character(0)
  for(i in seq_len(n)){
    comparison_colors <- c(fixed_colors, selected)
    cand_lab <- to_lab(candidate_colors)
    comp_lab <- to_lab(comparison_colors)
    # For each candidate, calculate its closest distance to any already-used color
    dists <- as.matrix(stats::dist(rbind(cand_lab, comp_lab)))
    n_cand <- nrow(cand_lab)
    min_dist_to_used <- apply(
      dists[seq_len(n_cand), -(seq_len(n_cand)), drop = FALSE], 1, min
    )
    # Pick the candidate with the largest minimum distance
    next_color <- candidate_colors[which.max(min_dist_to_used)]
    selected <- c(selected, next_color)
    candidate_colors <- setdiff(candidate_colors, next_color)
  }
  return(selected)
}

# List of abbreviated MA names where focal point of map is shifted (for legend display)
# Positive numbers shift focal point to the left, negative to the right
legend_shift <- data.table(
  ma = c("KJCAP", "ABAP"),
  amount = c(1, 0.2)
)

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

for(ma in unique(data$ManagedAreaName)){
  if(is.na(ma)) next
  # Set up watermark text display
  if(hab=="Coastal Wetlands"){
    indicators <- c("Species Composition")
    parameters <- c("Species Richness")
    parameters_display  <- c("Total/Canopy Percent Cover")
    # ma_include_list <- list(unique(data$ManagedAreaName))
    data_subset <- list(df)
  } else if(hab=="Coral Reef"){
    indicators <- c("Percent Cover", "Grazers and Reef Dependent Species")
    parameters <- c("Percent Cover", "Presence/Absence")
    parameters_display <- parameters
    # ma_include_list <- list(coral_pc_MA_Include, coral_sr_MA_Include)
    coral_sr_programs <- coral_sr_data %>% 
      group_by(!!sym(analysis_col), ProgramID, ProgramName, ProgramLocationID, 
               LocationID, IndicatorName, ParameterName, OriginalLatitude, OriginalLongitude) %>%
      reframe(n_data = n())
    coral_pc_programs <- coral_pc_data %>% 
      group_by(!!sym(analysis_col), ProgramID, ProgramName, ProgramLocationID, 
               LocationID, IndicatorName, ParameterName, OriginalLatitude, OriginalLongitude) %>%
      reframe(n_data = n())
    data_subset <- list(merge(coral_pc_programs, pt_ln_df), merge(coral_sr_programs, pt_ln_df))
  } else if(hab=="Nekton"){
    indicators <- c("Nekton")
    parameters <- c("Presence/Absence")
    parameters_display <- parameters
    data_subset <- list(df)
  } else if(hab=="Oyster Reef"){
    indicators <- c("Density", "Percent Live", "Size Class")
    parameters <- c("Density", "Percent Live", "Shell Height")
    parameters_display <- parameters
    data_subset <- list(df)
  } else if(hab=="Submerged Aquatic Vegetation"){
    indicators <- c("Percent Cover")
    parameters <- c("Percent Cover")
    parameters_display <- parameters
    data_subset <- list(df)
  }
  # Get abbreviated MA name
  ma_abrev <- ifelse(analysis=="ma", MA_All[ManagedAreaName==ma, Abbreviation], gsub(" ", "_", ma))
  # locate shape file for a given MA
  if(analysis=="ma"){
    ma_shape <- SEACAR::find_shape(rcp, ma)
  } else {
    ma_shape <- oimmp_boundaries %>% filter(OIMMP==ma)
  }
  
  # Set legend location variable
  legend_loc <- ifelse(ma_abrev %in% top_left, "topleft", "topright")
  
  for(i in 1:length(indicators)){
    ind <- indicators[i]
    param <- parameters[i]
    param_display <- parameters_display[i]
    # ma_include <- ma_include_list[[i]]
    # if(!ma %in% ma_include) next
    # Filter data for a given MA
    if(hab=="Submerged Aquatic Vegetation"){
      df_ma <- data_subset[[1]] %>% filter(ManagedAreaName==ma & IndicatorName==ind)
    } else {
      df_ma <- data_subset[[1]] %>% filter(ManagedAreaName==ma & IndicatorName==ind & ParameterName==param)
    }
    if(nrow(df_ma)==0) next
    # TMAP OYSTER SHELL HEIGHT EXCEPTION HERE?
    if(ma_abrev=="TMAP" & param=="Shell Height") next
    
    # Shuffle ordering of dataframe to prevent a single program being favored simply because it is plotted last
    set.seed(42)
    df_ma <- df_ma[sample(nrow(df_ma)), ]
    
    ##### Set up palette
    # Grab a list of programs with data for each MA
    progs <- unique(df_ma$ProgramName)
    # Identify the values that are not manually assigned a palette color
    remaining_values <- setdiff(progs, freq_programs)
    
    # Use remaining colors for these values
    base_remaining <- seacar_palette[!seacar_palette %in% unname(manual_colors)]
    
    remaining_palette <- pick_far_colors(
      candidate_colors = base_remaining,
      fixed_colors = unname(manual_colors),
      n = length(remaining_values)
    )
    
    # if(length(remaining_values) > length(base_remaining)){
    #   remaining_palette <- colorRampPalette(base_remaining)(length(remaining_values))
    # } else {
    #   remaining_palette <- base_remaining[seq(from=1, to=length(base_remaining), length.out = length(remaining_values))]
    # }
    
    # Name remaining palette
    names(remaining_palette) <- remaining_values
    # Combine all values into a single palette
    full_mapping <- c(manual_colors, remaining_palette)
    # Create color-lookup palette function
    pal <- function(x){full_mapping[as.character(x)]}
    
    # # Palette set up, if too many overall then create palette within MA / Indicator
    # if(length(unique(all_programs$ProgramName))>length(seacar_palette)){
    #   progs <- unique(df_ma$ProgramName)
    # } else {
    #   progs <- unique(all_programs$ProgramName)
    # }
    # palette <- seacar_palette[seq(1, length(seacar_palette), by = length(seacar_palette) / length(progs))]
    # names(palette) <- unique(progs)
    # pal <- function(x){palette[as.character(x)]}
    
    # Apply palette to each program
    df_ma$color <- pal(df_ma$ProgramName)
    # Define alpha (transparency) - make exception for programs with low amounts of data (make them less transparent)
    df_ma$alpha <- ifelse(df_ma$n_data<=10, 1, 0.6) # 0.6 is default
    # Habitat to display in metadata tag
    hab_display <- ifelse(hab=="Nekton", "Water Column", hab)
    # Indicator display in metadata tag
    ind_display <- ifelse(hab=="Submerged Aquatic Vegetation", "Percent Cover (by species)", ind)
    # metadata tag
    if(analysis=="ma"){
      fig_text <- tags$div(HTML(glue("{ma} - {hab_display} - {ind_display} - {param_display} - Export Date: {exportDate}")),
                           style = "margin-bottom:10px;")      
    } else {
      fig_text <- tags$div(HTML(glue("OIMMP: {ma} - {hab_display} - {ind_display} - {param_display} - Export Date: {exportDate}")),
                           style = "margin-bottom:10px;")    
    }
    
    # get coordinates to set zoom level
    # OIMMP: center around points, MA: center around MA boundary
    if(analysis=="ma"){
      shape_coordinates <- SEACAR::get_shape_coordinates(ma_shape)
    } else {
      shape_coordinates <- data.frame(xmin = min(df_ma$OriginalLongitude),
                                      ymin = min(df_ma$OriginalLatitude),
                                      xmax = max(df_ma$OriginalLongitude), 
                                      ymax = max(df_ma$OriginalLatitude))
    }
    
    # Apply shift where necessary
    if(ma_abrev %in% legend_shift$ma){
      shape_coordinates$xmin <- shape_coordinates$xmin + legend_shift[ma==ma_abrev, amount]
    }
    # # Apply shift to CW if adding buffer boundaries
    # if(hab=="Coastal Wetlands" && add_buff_boundary && !ma_abrev %in% legend_shift$ma){
    #   shape_coordinates$xmin <- shape_coordinates$xmin + 0.1
    # }

    # Apply linebreaks to long program names
    labs <- shorten_program_names(unique(df_ma$ProgramName), cutoff=60)
    # Title for boundary based on analysis type
    boundary_title <- ifelse(analysis=="ma", "ORCP Boundary", "OIMMP Boundary")
    
    # Function to set radius / circle size by # of samples (for legend)
    if(ma_abrev=="BBSAP" & hab=="Coastal Wetlands"){
      calc_radius <- function(n){log(n)*10}
    } else if(hab=="Nekton"){
      calc_radius <- function(n){sqrt(n)/2}
    } else if(hab=="Oyster Reef"){
      if(ma_abrev %in% c("ABAP", "ANERR") & param=="Shell Height"){
        calc_radius <- function(n){log(n)*2}
      } else {
        calc_radius <- function(n){log(n)*5}
      }
    } else if(hab=="Submerged Aquatic Vegetation"){
      if(ma_abrev %in% c("LRLWCAP", "IRVBFPAP", "IRMVBAP", "BRAP", "JBJIAP")){
        calc_radius <- function(n){sqrt(n)/3}
      } else {
        calc_radius <- function(n){sqrt(n)}
      }
    } else if(hab=="Coral Reef"){
      if(ma_abrev %in% c("KJCAP", "FKNMS") & ind=="Grazers and Reef Dependent Species"){
        calc_radius <- function(n){log(n)}
      } else {
        calc_radius <- function(n){log(n)*5}
      }
    } else {
      calc_radius <- function(n){log(n)*5}
    }
    
    # Create radius from N_data column
    df_ma$rad <- calc_radius(df_ma$n_data)
    
    # set df_ma as SF geo-object
    df_ma <- st_as_sf(df_ma)
    
    # # subsetting for lines vs points (coordinate vs transect)
    # pts <- df_ma %>% filter(!is.na(Longitude_))
    # lns <- df_ma %>% filter(!is.na(RawLineStr))
    
    # Create map (without pts or lines for now)
    map <- leaflet(df_ma, options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addLegend(title = "Program", 
                colors = pal(unique(df_ma$ProgramName)), 
                labels = labs, 
                position = legend_loc) %>%
      fitBounds(lng1=shape_coordinates$xmin,
                lat1=shape_coordinates$ymin,
                lng2=shape_coordinates$xmax,
                lat2=shape_coordinates$ymax) %>%
      addCircleLegend_temp(title = "Number of samples",
                           range = df_ma$n_data,
                           scaling_fun = calc_radius,
                           fillColor = "#b3b3b3",
                           fillOpacity = 0.8,
                           weight = 1,
                           color = "#000000",
                           position = "topright",
                           type = tolower(habs[Habitat==hab, HabitatShort])) %>%
      addControl(fig_text, position="bottomleft", className="map-title") %>%
      addScaleBar(position = "bottomright",
                  options = scaleBarOptions(metric=TRUE)) %>%
      addControl(
        html = '<div><img src="https://upload.wikimedia.org/wikipedia/commons/8/84/North_Pointer.svg" style="width:25px; opacity:0.6;"></div>',
        position = "bottomright",
        className = "map-title"
      ) %>% 
      prependContent(border_style)
    
    # Add inset map to OIMMP maps
    if(analysis=="oimmp"){
      map <- map %>%
        addMiniMap(centerFixed = c(mean(df_ma$OriginalLatitude),mean(df_ma$OriginalLongitude)), 
                   zoomLevelOffset = -5, 
                   position = 'topleft', 
                   tiles = providers$Esri.WorldGrayCanvas)
    }
    
    # add transects and points where available
    if(nrow(df_ma)>0){
      map <- map %>%
        addCircleMarkers(data = df_ma,
                         # lat=~OriginalLatitude, lng=~OriginalLongitude,
                         weight=1, color = "#000000", stroke = TRUE,
                         fillColor = unname(df_ma$color),
                         radius=~rad, #radius=calc_radius(pts$n_data),
                         fillOpacity = unname(df_ma$alpha))
    }
    
    # Add ma_buff boundary for coastal wetlands
    if(hab=="Coastal Wetlands" && add_buff_boundary){
      map <- map %>%
        addPolygons(data=ma_shape, color="black", weight = 1, smoothFactor = 0.5, 
                    opacity = 0.8, fillOpacity = 0.1) %>%
        addPolygons(data=ma_buff_boundaries %>% 
                      filter(MA_AreaIDB == MA_All[ManagedAreaName==ma, AreaID]), 
                    color="black", weight = 1, smoothFactor = 0.5, 
                    opacity = 0.8, fillOpacity = 0) %>%
        addLegendSymbol(title = paste0(boundary_title, " (+ 1 mile buffer)"), 
                        color = c("black"),
                        fillOpacity = 0.1,
                        values = ma,
                        shape = c("rect"), 
                        position = "topright", strokeWidth = 0.5)
    } else {
      map <- map %>% 
        addPolygons(data=ma_shape, color="black", weight = 1, smoothFactor = 0.5, 
                    opacity = 0.8, fillOpacity = 0.1) %>%
        addLegendSymbol(title = boundary_title, 
                        color = "#000000", fillColor = "#000000", fillOpacity = 0.1,
                        values = ma, shape = "rect", position = "topright", strokeWidth = 0.5)
    }
    
    # map output filepath
    hab_filepath <- gsub(" ", "", habs[Habitat==hab, Habitat])
    if(hab %in% c("Coastal Wetlands", "Nekton")){
      file_name <- paste0(hab_filepath, "_", ma_abrev, "_map.png")
    } else if(hab=="Coral Reef"){
      file_name <- paste0("Coral_", ifelse(ind=="Percent Cover", "PC", "SpeciesRichness"), "_", ma_abrev, "_map.png")
    } else if(hab=="Oyster Reef"){
      ind_short <- ifelse(ind=="Density", "Dens", ifelse(ind=="Percent Live", "PrcLive", "SH"))
      file_name <- paste0("Oyster_", ind_short, "_", ma_abrev, "_map.png")
    } else if(hab=="Submerged Aquatic Vegetation"){
      file_name <- paste0("SAV_", ma_abrev, "_map.png")
    }
    map_out <- paste0(output_loc, file_name)
    
    # save file as png
    mapview::mapshot(map, file = map_out, remove_controls = NULL)
    print(paste0("Map created for ", hab, " - ", ind, " - ", ma))
  }
}

#Gets list of all image files in output/Figures and creates zip directory
map_list <- list.files(output_loc, pattern=".png", full=FALSE)
setwd(output_loc)
zip(habs[Habitat==hab, HabitatZip], files=map_list)
setwd(wd)