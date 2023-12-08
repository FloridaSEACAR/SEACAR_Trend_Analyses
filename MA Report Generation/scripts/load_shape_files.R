# AP and NERR shapefiles

AP_shp <- st_read(here::here("data/shapes/APs/Florida_Aquatic_Preserves.shp"))
NERR_shp <- st_read(here::here("data/shapes/NERRs/Florida_National_Estuarine_Resarch_Reserves__NERR__Boundaries.shp"))

###############
## FUNCTIONS ##
###############

# Allows location of shapefile for each MA
find_shape <- function(ma){
  if (grepl("National Estuarine", ma, fixed = TRUE)){
    shape_file <- NERR_shp %>% filter(SITE_NAME==ma)
  } else if (grepl("Aquatic Preserve", ma, fixed = TRUE)) {
    shape_file <- AP_shp %>% filter(LONG_NAME==ma)
  }
  return(shape_file)
}

# Gets coordinate min and max from shapefile
# This allows for accurately setting view on the map
get_shape_coordinates <- function(ma_shape){
  bbox_list <- lapply(st_geometry(ma_shape), st_bbox)
  maxmin <- as.data.frame(matrix(unlist(bbox_list),nrow=nrow(ma_shape)))
  names(maxmin) <- names(bbox_list[[1]])
  return(maxmin)
}