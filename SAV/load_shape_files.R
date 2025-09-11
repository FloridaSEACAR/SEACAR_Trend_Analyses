library(sf)
library(SEACAR)
library(tidyverse)

source("../SEACAR_data_location.R")
shape_files <- list.files(seacar_shape_location, full=TRUE)
shape_locate <- function(location){return(paste0(seacar_shape_location, location))}

locs_pts <- SEACAR::GeoData$pointLocations
locs_lns <- SEACAR::GeoData$lineLocations
rcp <- SEACAR::GeoData$`RCP Boundaries`
corners <- setDT(SEACAR::GeoData$corners)

# Bring in county-line shapefile for SAV scope plot creation
counties <- st_read(shape_locate("/FLCounties/Counties_-_Detailed_Shoreline.shp")) %>%
  st_make_valid() %>% st_transform(crs = 4326)

locs_pts_rcp <- locs_pts[rcp, , op = st_intersects]
locs_lns_rcp <- locs_lns[rcp, , op = st_intersects]

pnames <- distinct(SAV4[, .(ProgramID, ProgramName)])
locs_pts_rcp <- merge(locs_pts_rcp, pnames, by = "ProgramID", all.x = TRUE)
locs_lns_rcp <- merge(locs_lns_rcp, pnames, by = "ProgramID", all.x = TRUE)
