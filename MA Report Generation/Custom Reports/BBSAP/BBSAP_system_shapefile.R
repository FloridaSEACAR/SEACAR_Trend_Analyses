library(sf)
library(leaflet)

# Import path variables
source("seacar_data_location.R")

# Read in original sample locations shapefile
pts <- st_read(paste0(shape_loc, "/SampleLocations12mar2024/vw_SampleLocation_Point.shp"))

# Filter for ProgramIDs and ProgramLocationIDs included in both WQ and SAV
pts <- pts %>% filter(ProgramLoc %in% bb_ploc,
                      ProgramID %in% bb_pid,
                      Latitude_D > 25)

# Write BBSAP sample locations to shapefile bb_points.shp
st_write(pts, paste0(shape_loc, "/bb_points.shp"))

# Modified bb_points.shp within ArcGIS Pro to select and label each system
# Re-import new modified shapefile with system information
bb_points <- st_read(bb_shape_location)

# Define palette to view results
pal <- colorFactor(palette = "viridis", domain = bb_points$System)

# Create leaflet map to display grouping results
map <- leaflet(bb_points) %>%
  addTiles() %>%
  addCircleMarkers(~Longitude_, ~Latitude_D, color = ~pal(System), 
                   popup = paste(sep = "<br/>",
                                 "lat: ",bb_points$Latitude_D,
                                 "lon: ",bb_points$Longitude_,
                                 "system: ",bb_points$System)) %>%
  addLegend("topright", colors = pal(unique(bb_points$System)), labels = unique(bb_points$System))