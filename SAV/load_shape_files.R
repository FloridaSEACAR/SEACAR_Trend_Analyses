library(sf)
# AP and NERR shapefiles
source("../SEACAR_data_location.R")
shape_files <- list.files(seacar_shape_location, full=TRUE)

shape_locate <- function(location){return(paste0(seacar_shape_location, location))}

GeoDBdate <- "5Mar2025"
locs_pts <- st_read(shape_locate(paste0("/SampleLocations", GeoDBdate, "/seacar_dbo_vw_SampleLocation_Point.shp")))
locs_lns <- st_read(shape_locate(paste0("/SampleLocations", GeoDBdate, "/seacar_dbo_vw_SampleLocation_Line.shp")))
rcp <- st_read(shape_locate("/orcp_all_sites/ORCP_Managed_Areas.shp"))
counties <- st_read(shape_locate("/FLCounties/Counties_-_Detailed_Shoreline.shp"))
corners <- fread(shape_locate("/MApolygons_corners.csv"))

corners[, `:=` (xmax = xmax + (xmax-xmin)*0.25, ymax = ymax + (ymax-ymin)*0.1)]

locs_pts <- st_make_valid(locs_pts)
locs_lns <- st_make_valid(locs_lns)
rcp <- st_make_valid(rcp)
counties <- st_make_valid(counties)

locs_pts <- st_transform(locs_pts, crs = 4326)
locs_lns <- st_transform(locs_lns, crs = 4326)
rcp <- st_transform(rcp, crs = 4326)
counties <- st_transform(counties, crs = 4326)

locs_pts_rcp <- locs_pts[rcp, , op = st_intersects]
locs_lns_rcp <- locs_lns[rcp, , op = st_intersects]

pnames <- distinct(SAV4[, .(ProgramID, ProgramName)])
locs_pts_rcp <- merge(locs_pts_rcp, pnames, by = "ProgramID", all.x = TRUE)
locs_lns_rcp <- merge(locs_lns_rcp, pnames, by = "ProgramID", all.x = TRUE)

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
