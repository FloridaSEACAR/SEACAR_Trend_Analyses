library(knitr)
library(readr)
library(tidyverse)
library(data.table)
library(dplyr)
library(purrr)
library(rstudioapi)
library(stringr)
library(utils)
library(leaflet)
library(mapview)
library(magick)
library(mgcv)
library(cowplot)
library(sf)
library(this.path)

# Set working directory to SEACAR_Trend_Analyses root folder
setwd(this.path::here())

# # Determine whether to run oyster or SAV scope plots
# analyze_hab <- "oyster"
# analyze_hab <- "sav"

# # Determine whether to create plots by OIMMP region or by MA
# analyze_type <- "ma"
# analyze_type <- "oimmp"

# # Determine whether to include county "backdrop" for each year (to see shoreline/rivers)
county_backdrop <- ifelse(analyze_type=="oimmp", TRUE, FALSE)

if(analyze_hab=="oyster"){
  # Bring in oyster data
  file_in <- str_subset(list.files("C:/SEACAR Data/SEACARdata/", full.names = TRUE), "OYSTER")
} else if(analyze_hab=="sav"){
  # Bring in SAV data
  file_in <- str_subset(list.files("C:/SEACAR Data/SEACARdata/", full.names = TRUE), "SAV")
}
# Bring in available habitat data
hab_data <- fread(file_in, sep = "|", na.strings = "NULL")

# Smaller subset of available MAs to de-concatenate
if(analyze_type=="ma"){
  ma_subset <- hab_data %>% group_by(AreaID, ManagedAreaName) %>% reframe() %>%
    SEACAR::clean_managed_areas(type = "ma") %>% distinct() %>% arrange(AreaID)
} else {
  ma_subset <- hab_data %>% group_by(OIMMP) %>% reframe()
}

locs_pts <- SEACAR::GeoData$pointLocations
locs_lns <- SEACAR::GeoData$lineLocations
rcp <- SEACAR::GeoData$`RCP Boundaries`
corners <- setDT(SEACAR::GeoData$corners)
oimmp_boundaries <- SEACAR::GeoData$OIMMP

if(analyze_type=="ma"){
  locs_pts_rcp <- locs_pts[rcp, , op = st_intersects]
  locs_lns_rcp <- locs_lns[rcp, , op = st_intersects]
} else {
  locs_pts_rcp <- locs_pts[oimmp_boundaries, , op = st_intersects]
  locs_lns_rcp <- locs_lns[oimmp_boundaries, , op = st_intersects]
}

pnames <- distinct(hab_data[, .(ProgramID, ProgramName)])
locs_pts_rcp <- merge(locs_pts_rcp, pnames, by = "ProgramID", all.x = TRUE)
locs_lns_rcp <- merge(locs_lns_rcp, pnames, by = "ProgramID", all.x = TRUE)  
  
# Bring in county-line shapefile for scope plot creation
counties <- st_read("C:/SEACAR Data/SEACARshapes/FLCounties/Counties_-_Detailed_Shoreline.shp") %>%
  st_make_valid() %>% st_transform(crs = 4326)

# Create directory to store .rds objects for each plot
if(analyze_hab=="oyster"){
  file_dir <- paste0("Oyster/output/Oyster_temporal_scope_plots/")
} else if(analyze_hab=="sav"){
  file_dir <- paste0("SAV/output/SAV_temporal_scope_plots/")
}
# Add OIMMP subfolder for OIMMP plots
if(analyze_type=="oimmp"){
  file_dir <- paste0(file_dir, "OIMMP/")
}
file_dirs <- c(file_dir, paste0(file_dir, "rds/")) # rds subdirectory
lapply(file_dirs, function(x){if(!file.exists(x)) dir.create(x)})

# set size of plot points
pt_size <- 3

# Code source for original rotate sf function: https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/geospatial-data/
#' Rotate simple features for 3D layers
#' Rotates a simple features layer using a shear matrix transformation on the 
#' \code{geometry} column. This can get nice for visualisation and works with
#' points, lines and polygons.
#'
#' @param data an object of class \code{sf}
#' @param x_add integer; x value to move geometry in space
#' @param y_add integer; x value to move geometry in space
#'
#' #' @importFrom magrittr %>%

rotate_sf <- function(data, x_add = 0, y_add = 0, ma, coast = "Atlantic"){
  if(coast == "Atlantic"){
    if(unique(ma) %in% c("Banana River Aquatic Preserve", "Indian River-Malabar to Vero Beach Aquatic Preserve",
                         "Indian River-Vero Beach to Ft. Pierce Aquatic Preserve", "Jensen Beach to Jupiter Inlet Aquatic Preserve",
                         "Mosquito Lagoon Aquatic Preserve")){
      shear_matrix <- function (x) {
        #matrix(c(2, 1.2, 0, 1), 2, 2)
        # matrix(c(0.2, -0.3, 0.5, 0.7), 2, 2)
        # matrix(c(0.2, -0.3, 0, 0.7), 2, 2)
        matrix(c(1, 1.2, 0, 1), 2, 2)
      }
      
      rotate_matrix <- function(x) {
        matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2)
      }
      
      data %>%
        dplyr::mutate(
          geometry =
            # .$geometry * shear_matrix() * rotate_matrix(pi*0.6) + c(x_add, y_add)
            .$geometry * shear_matrix() * rotate_matrix(pi*0.2) + c(x_add, y_add)
        )
    } else{
      shear_matrix <- function (x) {
        #matrix(c(2, 1.2, 0, 1), 2, 2)
        matrix(c(2, 1.2, 0, 1), 2, 2)
      }
      
      rotate_matrix <- function(x) {
        matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2)
      }
      
      data %>%
        dplyr::mutate(
          geometry =
            .$geometry * shear_matrix() * rotate_matrix(pi/20) + c(x_add, y_add)
        )
    }
    
  } else{
    shear_matrix <- function (x) {
      #matrix(c(2, 1.2, 0, 1), 2, 2)
      matrix(c(2, -1.2, 0, 1), 2, 2)
    }
    
    rotate_matrix <- function(x) {
      matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2)
    }
    
    data %>%
      dplyr::mutate(
        geometry =
          .$geometry * shear_matrix() * rotate_matrix(pi*1.98) + c(x_add, y_add)
      )
  }
}

MA_All <- SEACAR::ManagedAreas

set.seed(4691)

if(analyze_type=="ma"){
  managed_areas <- unique(ma_subset$ManagedAreaName)
} else {
  managed_areas <- unique(ma_subset$OIMMP)
}

# Use str_detect when filtering by ManagedAreaName within hab_data
# This avoids de-concatenating the entire dataset by MA

for(i in managed_areas){
  if(is.na(i)) next
  ma_abrev <- ifelse(analyze_type=="ma", MA_All[ManagedAreaName==i, unique(Abbreviation)], gsub(" ", "_", i))
  
  if(analyze_type=="ma"){
    rcp_i <- subset(rcp, rcp$LONG_NAME == i)
    hab_data_subset <- hab_data[str_detect(ManagedAreaName, i), ]
  } else {
    rcp_i <- oimmp_boundaries %>% filter(OIMMP==i)
    hab_data_subset <- hab_data[OIMMP==i, ]
  }
  
  fl_i <- st_crop(counties, 
                  xmin = corners[LONG_NAME == i, xmin], 
                  xmax = corners[LONG_NAME == i, xmax], 
                  ymin = corners[LONG_NAME == i, ymin], 
                  ymax = corners[LONG_NAME == i, ymax])
  
  # Set color palette independently for each MA
  progs <- sample(sort(unique(hab_data_subset$ProgramName)))
  seacar_palette <- colorRampPalette(SEACAR::seacar_palette2)(length(progs))
  color_pal <- setNames(seacar_palette, progs)
  
  #create scalebar and north arrow (https://stackoverflow.com/questions/34183049/plot-circle-with-a-certain-radius-around-point-on-a-map-in-ggplot2)
  if(corners[LONG_NAME == i, Coast[1]] == "Atlantic"){
    wkm <- (abs(st_bbox(fl_i)[[4]]) - abs(st_bbox(fl_i)[[2]])) * (40075 / 360) #* cos((st_bbox(fl_i)[[2]] + (abs(st_bbox(fl_i)[[4]]) - abs(st_bbox(fl_i)[[2]]))/2)/57.3)
    lonkm <- ifelse(wkm < 20, 3 / (40075 / 360), ifelse(wkm < 50, 5 / (40075 / 360),
                                                        10 / (40075 / 360)))
  } else{
    wkm <- (abs(st_bbox(fl_i)[[1]]) - abs(st_bbox(fl_i)[[3]])) * (40075 / 360) * cos((st_bbox(fl_i)[[2]] + (abs(st_bbox(fl_i)[[4]]) - abs(st_bbox(fl_i)[[2]]))/2)/57.3)
    lonkm <- ifelse(wkm < 20, 3 / ((40075 / 360) * cos((st_bbox(fl_i)[[2]] + (abs(st_bbox(fl_i)[[4]]) - abs(st_bbox(fl_i)[[2]]))/2)/57.3)), ifelse(wkm < 50, 5 / ((40075 / 360) * cos((st_bbox(fl_i)[[2]] + (abs(st_bbox(fl_i)[[4]]) - abs(st_bbox(fl_i)[[2]]))/2)/57.3)),
                                                                                                                                                   10 / ((40075 / 360) * cos((st_bbox(fl_i)[[2]] + (abs(st_bbox(fl_i)[[4]]) - abs(st_bbox(fl_i)[[2]]))/2)/57.3))))
  }
  
  fl_i_bbox <- st_bbox(fl_i)
  rcp_i_bbox <- st_bbox(rcp_i)
  min_x <- min(fl_i_bbox$xmin, rcp_i_bbox$xmin)
  max_x <- max(fl_i_bbox$xmax, rcp_i_bbox$xmax)
  min_y <- min(fl_i_bbox$ymin, rcp_i_bbox$ymin)
  max_y <- max(fl_i_bbox$ymax, rcp_i_bbox$ymax)
  
  if(corners[LONG_NAME == i, Coast[1]] == "Atlantic"){
    sbar <- data.table(x = max_x,
                       y = c(min_y, min_y + lonkm))#c(min_y - (abs(max_y) - abs(min_y)) * 0.15, min_y - (abs(max_y) - abs(min_y)) * 0.15))
    
    x_sbarpos1 <- (abs(min_x) - abs(max_x)) * 0.6 
    x_sbarpos2 <- (abs(min_x) - abs(max_x)) * 0.2 
    y_sbarpos1 <- ifelse(((abs(max_y) - abs(min_y)) * 0.6) >= lonkm, 
                         (abs(max_y) - abs(min_y)) * 0.6,
                         (lonkm) + ((abs(max_y) - abs(min_y)) * 0.1)) 
    y_sbarpos2 <- ifelse(((abs(max_y) - abs(min_y)) * 0.1) >= lonkm, 
                         (abs(max_y) - abs(min_y)) * 0.1,
                         (lonkm) + ((abs(max_y) - abs(min_y)) * 0.1)) 
    
  } else {
    sbar <- data.table(x = c(min_x, min_x + lonkm),
                       y = min_y)#c(min_y - (abs(max_y) - abs(min_y)) * 0.15, min_y - (abs(max_y) - abs(min_y)) * 0.15))
    
    x_sbarpos1 <- ifelse(((abs(min_x) - abs(max_x)) * 0.6) >= lonkm, 
                         ((abs(min_x) - abs(max_x)) * 0.6),
                         (lonkm) + ((abs(min_x) - abs(max_x)) * 0.1))
    x_sbarpos2 <- ifelse(((abs(min_x) - abs(max_x)) * 0.2) >= lonkm, 
                         ((abs(min_x) - abs(max_x)) * 0.2),
                         (lonkm) + ((abs(min_x) - abs(max_x)) * 0.1))
    y_sbarpos1 <- (abs(max_y) - abs(min_y)) * 0.6
    y_sbarpos2 <- (abs(max_y) - abs(min_y)) * 0.1
    
  }
  
  sbar[, `:=` (x = fcase(corners[LONG_NAME == i, Coast[1]] == "Gulf", x + x_sbarpos2, #(abs(min_x) - abs(max_x)) * ((lonkm/(max_x - min_x)) * x_sbarpos2),
                         corners[LONG_NAME == i, Coast[1]] == "Panhandle", x + x_sbarpos1, #(abs(min_x) - abs(max_x)) * ((lonkm/(max_x - min_x)) * x_sbarpos1),
                         i %in% c("Banana River Aquatic Preserve", "Indian River-Malabar to Vero Beach Aquatic Preserve", "Indian River-Vero Beach to Ft. Pierce Aquatic Preserve", "Jensen Beach to Jupiter Inlet Aquatic Preserve", 
                                  "Mosquito Lagoon"), x + x_sbarpos2, #(abs(min_x) - abs(max_x)) * ((lonkm/(max_x - min_x)) * x_sbarpos2),
                         corners[LONG_NAME == i, Coast[1]] == "Atlantic", x + x_sbarpos2), #(abs(min_x) - abs(max_x)) * ((lonkm/(max_x - min_x)) * x_sbarpos2)),
               y = fcase(corners[LONG_NAME == i, Coast[1]] == "Gulf", y - y_sbarpos2, #(abs(max_y) - abs(min_y)) * ((lonkm/(max_y - min_y)) * y_sbarpos2),
                         corners[LONG_NAME == i, Coast[1]] == "Panhandle", y + y_sbarpos2, #(abs(max_y) - abs(min_y)) * ((lonkm/(max_y - min_y)) * y_sbarpos2),
                         i %in% c("Banana River Aquatic Preserve", "Indian River-Malabar to Vero Beach Aquatic Preserve", "Indian River-Vero Beach to Ft. Pierce Aquatic Preserve", "Jensen Beach to Jupiter Inlet Aquatic Preserve", 
                                  "Mosquito Lagoon"), y + y_sbarpos1, #(abs(max_y) - abs(min_y)) * ((lonkm/(max_y - min_y)) * y_sbarpos1),
                         corners[LONG_NAME == i, Coast[1]] == "Atlantic", y + y_sbarpos1)),#(abs(max_y) - abs(min_y)) * ((lonkm/(max_y - min_y)) * y_sbarpos1))), 
       by = list(row.names(sbar))]
  
  sbar <- st_as_sf(sbar, coords = c("x", "y"), crs = 4326)
  sbar <- st_combine(sbar)
  sbar <- st_cast(sbar, "LINESTRING")
  sbar <- st_sf(sbar)
  st_geometry(sbar) <- "geometry"
  
  if(corners[LONG_NAME == i, Coast[1]] == "Atlantic"){
    sbarlab <- data.table(x = st_bbox(sbar)$xmin + (abs(min_x) - abs(max_x)) * 0.15,
                          y = st_bbox(sbar)$ymin + lonkm/3) #min_y)
  } else {
    sbarlab <- data.table(x = st_bbox(sbar)$xmin + lonkm/2,
                          y = st_bbox(sbar)$ymin - (abs(max_y) - abs(min_y)) * 0.1) #min_y)
  }
  
  sbarlab <- st_as_sf(sbarlab, coords = c("x", "y"), crs = 4326)
  
  if(corners[LONG_NAME == i, Coast[1]] == "Atlantic"){
    narrow <- data.table(x = c(st_bbox(sbar)$xmin,
                               st_bbox(sbar)$xmin,
                               st_bbox(sbar)$xmin,
                               st_bbox(sbar)$xmin + (abs(min_x) - abs(max_x)) * 0.055,
                               st_bbox(sbar)$xmin,
                               st_bbox(sbar)$xmin - (abs(min_x) - abs(max_x)) * 0.055),
                         y = c((st_bbox(sbar)$ymin - ((abs(max_y) - abs(min_y)) * 0.15)),
                               (st_bbox(sbar)$ymin - ((abs(max_y) - abs(min_y)) * 0.075)),
                               (st_bbox(sbar)$ymin - ((abs(max_y) - abs(min_y)) * 0.075)),
                               (st_bbox(sbar)$ymin - ((abs(max_y) - abs(min_y)) * 0.075)) - (abs(min_x) - abs(max_x)) * 0.065,
                               (st_bbox(sbar)$ymin - ((abs(max_y) - abs(min_y)) * 0.075)),
                               (st_bbox(sbar)$ymin - ((abs(max_y) - abs(min_y)) * 0.075)) - (abs(min_x) - abs(max_x)) * 0.065))
    
  } else{
    narrow <- data.table(x = c((st_bbox(sbar)$xmin - ((abs(min_x) - abs(max_x)) * 0.075)), 
                               (st_bbox(sbar)$xmin - ((abs(min_x) - abs(max_x)) * 0.075)), 
                               (st_bbox(sbar)$xmin - ((abs(min_x) - abs(max_x)) * 0.075)), 
                               (st_bbox(sbar)$xmin - ((abs(min_x) - abs(max_x)) * 0.075)) - (abs(max_y) - abs(min_y)) * 0.05, 
                               (st_bbox(sbar)$xmin - ((abs(min_x) - abs(max_x)) * 0.075)), 
                               (st_bbox(sbar)$xmin - ((abs(min_x) - abs(max_x)) * 0.075)) + (abs(max_y) - abs(min_y)) * 0.05),
                         y = c(st_bbox(sbar)$ymin, 
                               # min_y - (abs(max_y) - abs(min_y)) * 0.15, 
                               st_bbox(sbar)$ymin + (abs(max_y) - abs(min_y)) * 0.15,
                               st_bbox(sbar)$ymin + (abs(max_y) - abs(min_y)) * 0.15,
                               # min_y,
                               (st_bbox(sbar)$ymin + (abs(max_y) - abs(min_y)) * 0.15) - (abs(max_y) - abs(min_y)) * 0.065,
                               # min_y - (abs(max_y) - abs(min_y)) * 0.065, 
                               st_bbox(sbar)$ymin + (abs(max_y) - abs(min_y)) * 0.15,
                               # min_y, 
                               # min_y - (abs(max_y) - abs(min_y)) * 0.065,
                               (st_bbox(sbar)$ymin + (abs(max_y) - abs(min_y)) * 0.15) - (abs(max_y) - abs(min_y)) * 0.065))
    
  }
  
  narrow <- st_as_sf(narrow, coords = c("x", "y"), crs = 4326)
  narrow <- st_combine(narrow)
  narrow <- st_cast(narrow, "LINESTRING")
  narrow <- st_sf(narrow)
  st_geometry(narrow) <- "geometry"
  
  if(corners[LONG_NAME == i, Coast[1]] == "Atlantic"){
    narlab <- data.table(x = st_bbox(sbarlab)$xmin,
                         y = st_bbox(narrow)$ymin) #+ (abs(st_bbox(narrow)$ymax) - abs(st_bbox(narrow)$ymin)) / 5)
    
  } else{
    narlab <- data.table(x = st_bbox(narrow)$xmin + (abs(st_bbox(narrow)$xmin) - abs(st_bbox(narrow)$xmax)) / 2,
                         y = st_bbox(sbarlab)$ymin)
    
  }
  
  narlab <- st_as_sf(narlab, coords = c("x", "y"), crs = 4326)
  
  
  locs_pts_rcp_i <- locs_pts_rcp[rcp_i, , op = st_intersects]
  locs_lns_rcp_i <- locs_lns_rcp[rcp_i, , op = st_intersects]
  
  yadd <- 0
  xadd <- 0
  startyear <- min(hab_data_subset$Year)
  
  hab_display <- ifelse(analyze_hab=="oyster", str_to_title(analyze_hab), toupper(analyze_hab))
  
  base <- ggplot() +
    geom_sf(data = rotate_sf(fl_i, ma = i, coast = corners[LONG_NAME == i, Coast[1]]), fill = "beige", color = "navajowhite3", lwd = 0.5, inherit.aes = FALSE) +
    geom_sf(data = rotate_sf(rcp_i, ma = i, coast = corners[LONG_NAME == i, Coast[1]]), color = "grey50", fill = "powderblue", alpha = 0.35, lwd = 0.5, inherit.aes = FALSE) +
    geom_sf(data = rotate_sf(sbar, ma = i, coast = corners[LONG_NAME == i, Coast[1]]), color = "grey50", linewidth = 1.25, inherit.aes = FALSE) +
    geom_sf(data = rotate_sf(narrow, ma = i, coast = corners[LONG_NAME == i, Coast[1]]), color = "grey50", linewidth = 1, inherit.aes = FALSE) +
    geom_sf_text(data = rotate_sf(sbarlab, ma = i, coast = corners[LONG_NAME == i, Coast[1]]), label = ifelse(wkm < 20, "3 km", ifelse(wkm < 50, "5 km", "10 km")), hjust = 0.5, angle = 4, color = "grey50", size = 3.5, inherit.aes = FALSE) +
    geom_sf_text(data = rotate_sf(narlab, ma = i, coast = corners[LONG_NAME == i, Coast[1]]), label = "N", hjust = 0.7, angle = 4, color = "grey50", size = 3.5, inherit.aes = FALSE) +
    scale_color_manual(values = color_pal, 
                       aesthetics = c("color", "fill")) +
    labs(title = i,
         subtitle = paste0("Sample Locations - ", hab_display),
         fill = "Program name", color = "Program name") +
    theme(panel.grid.major = element_line(colour = NA),
          panel.grid.minor = element_line(colour = NA),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_rect(fill = NA),
          plot.background = element_rect(colour = NA),
          legend.position = "bottom",
          legend.direction = "vertical")
  ystart <- ifelse(corners[LONG_NAME == i, Coast[1]] == "Atlantic", attributes(base$layers[[2]]$data$geometry)$bbox$ymax[[1]], attributes(base$layers[[2]]$data$geometry)$bbox$ymin[[1]])
  xlab <- attributes(base$layers[[2]]$data$geometry)$bbox$xmax[[1]] + (attributes(base$layers[[2]]$data$geometry)$bbox$xmax[[1]] - attributes(base$layers[[2]]$data$geometry)$bbox$xmin[[1]])/50
  MAcoords <- setDT(as.data.frame(st_coordinates(rcp_i)))
  maxdist <- max(st_distance(st_as_sf(MAcoords[X == min(X), ], coords = c("X", "Y"), crs = 4326), st_as_sf(MAcoords[Y == max(Y), ], coords = c("X", "Y"), crs = 4326)),
                 st_distance(st_as_sf(MAcoords[X == max(X), ], coords = c("X", "Y"), crs = 4326), st_as_sf(MAcoords[Y == min(Y), ], coords = c("X", "Y"), crs = 4326)),
                 st_distance(st_as_sf(MAcoords[X == min(X), ], coords = c("X", "Y"), crs = 4326), st_as_sf(MAcoords[X == max(X), ], coords = c("X", "Y"), crs = 4326)),
                 st_distance(st_as_sf(MAcoords[Y == min(Y), ], coords = c("X", "Y"), crs = 4326), st_as_sf(MAcoords[Y == max(Y), ], coords = c("X", "Y"), crs = 4326)))
  area <- st_area(rcp_i)
  xyratio <- as.numeric((area/maxdist)/maxdist)
  
  MApolycoords <- setDT(as.data.frame(st_coordinates(base$layers[[2]]$data)))
  xmax_y <- MApolycoords[X == max(X), Y]
  base <- base + annotate("text", x = xlab, y = xmax_y, label = paste0(startyear), hjust = "left")
  
  MApolycoords[, Xrnd := round(X, 3)][, ydists := max(Y) - min(Y), by = Xrnd]
  MApolycoords[, Yrnd := round(Y, 3)][, xdists := max(X) - min(X), by = Yrnd]
  maxydist <- max(MApolycoords$ydists) + ((max(MApolycoords$ydists)/25) / xyratio)
  maxxdist <- 0
  
  if(analyze_type=="ma"){
    n_pt_data <- locs_pts_rcp_i %>% filter(LocationID %in% hab_data_subset[Year == startyear, unique(LocationID)])
    n_ln_data <- locs_lns_rcp_i %>% filter(LocationID %in% hab_data_subset[Year == startyear, unique(LocationID)])
  } else {
    n_pt_data <- locs_pts_rcp_i %>% filter(LocationID %in% hab_data_subset[Year == startyear, unique(LocationID)])
    n_ln_data <- locs_lns_rcp_i %>% filter(LocationID %in% hab_data_subset[Year == startyear, unique(LocationID)])
  }
  
  if(nrow(n_pt_data) > 0){
    base <- base +
      geom_sf(data = rotate_sf(n_pt_data,
                               ma = i, coast = corners[LONG_NAME == i, Coast[1]]),
              aes(fill = droplevels(as.factor(ProgramName))), shape = 21, color = "black", size=pt_size, alpha=0.7)
  }
  
  if(nrow(n_ln_data) > 0){
    base <- base +
      geom_sf(data = rotate_sf(n_ln_data,
                               ma = i, coast = corners[LONG_NAME == i, Coast[1]]),
              aes(color = droplevels(as.factor(ProgramName))), shape = 21, size=pt_size, alpha=0.7)
  }
  
  ###############
  
  years <- sort(unique(hab_data_subset[Year != startyear, Year]))
  total_years <- length(years)
  
  ## Determine number of columns to for each plot
  ## Determine level of "nudge" in x-axis when new column is made
  nudge_val <- 1.2 # default is +20%
  # Many years of oyster data in Apalachicola
  if(ma_abrev %in% c("ABAP", "ANERR", "Apalachicola_Bay") & analyze_hab=="oyster"){
    num_cols <- 4
    nudge_val <- 1.4 # +40%
  } else if(total_years > 30) {
    num_cols <- 4
  } else if(total_years > 20){
    num_cols <- 3
  } else {
    num_cols <- 2
  }
  
  # Individual exceptions
  if(str_detect(i, "Florida Keys National Marine Sanctuary")){
    maxydist <- maxydist * 3
    num_cols <- 2
  } 
  
  rows_per_column <- ceiling(total_years / num_cols)
  
  bbox <- st_bbox(rotate_sf(rcp_i, x_add = xadd, y_add = yadd+maxydist, ma = i, coast = corners[LONG_NAME == i, Coast[1]]))
  max_width <- abs(abs(bbox$xmax) - abs(bbox$xmin))
  x_increment <- max_width * nudge_val
  
  ###############  
  
  for(index in seq_along(years)){
    y <- years[index]
    
    n_pt_data_y <- locs_pts_rcp_i %>% filter(LocationID %in% hab_data_subset[Year == y, unique(LocationID)])
    n_ln_data_y <- locs_lns_rcp_i %>% filter(LocationID %in% hab_data_subset[Year == y, unique(LocationID)])
    
    # Rotated county and RCP/OIMMP shapefile
    rcp_rot <- rotate_sf(rcp_i, x_add = xadd + maxxdist, y_add = yadd + maxydist, ma = i, coast = corners[LONG_NAME == i, Coast[1]])
    county_rot <- rotate_sf(fl_i, x_add = xadd + maxxdist, y_add = yadd + maxydist, ma = i, coast = corners[LONG_NAME == i, Coast[1]])
    # County subset for RCP/OIMMP location (for county backdrop)
    county_subset <- sf::st_intersection(county_rot, rcp_rot)
    
    if(county_backdrop){
      base <- base + 
        geom_sf(data = county_subset, fill = "beige", color = "navajowhite3", lwd = 0.5, inherit.aes = FALSE)
    }
    
    base <- base +
      geom_sf(data = rcp_rot, 
              color = "grey50", fill = "powderblue", alpha = 0.65, lwd = 0.5, inherit.aes = FALSE) +
      annotate("text", x = xlab + xadd + maxxdist, y = xmax_y + yadd + maxydist, label = y, hjust = "left")
    
    if(nrow(n_pt_data_y) > 0){
      base <- base +
        geom_sf(data = rotate_sf(n_pt_data_y, 
                                 x_add = xadd + maxxdist, y_add = yadd + maxydist, ma = i, coast = corners[LONG_NAME == i, Coast[1]]), 
                aes(fill = droplevels(as.factor(ProgramName))), shape = 21, color = "black", size=pt_size, alpha=0.7)
    }
    
    if(nrow(n_ln_data_y) > 0){
      base <- base +
        geom_sf(data = rotate_sf(n_ln_data_y,
                                 x_add = xadd + maxxdist, y_add = yadd + maxydist, ma = i, coast = corners[LONG_NAME == i, Coast[1]]),
                aes(color = droplevels(as.factor(ProgramName))), shape = 21, size=pt_size, alpha=0.7)
    }
    
    yadd <- yadd + maxydist
    # xadd <- xadd + maxxdist
    
    if (index %% rows_per_column == 0) {
      yadd <- 0
      xadd <- xadd + x_increment
    }
  }
  
  base <- base +
    theme(legend.position='bottom', 
          legend.justification='left',
          legend.direction='vertical')
  
  a_stem <- ifelse(analyze_hab=="oyster", str_to_title(analyze_hab), str_to_upper(analyze_hab))
  file_stem <- paste0(a_stem, "_", ma_abrev, "_temporal_scope")
  
  saveRDS(base, paste0(file_dir, "rds/", file_stem, ".rds"))
  
  ggsave(filename = paste0(file_dir, file_stem, ".jpg"),
         plot = base,
         dpi = 300,
         width = 15,
         height = 18,
         units = "in",
         limitsize = FALSE)
  
  rm(base)
  print(paste0(i, " - ", hab_display, " scope plot object created"))
}
