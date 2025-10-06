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

# Load shape files from load_shape_files.R
source("load_shape_files.R", echo = T)

# Create directory to store .rds objects for each plot
if(!file.exists("output/SAV_temporal_scope_plots/rds/")) dir.create("output/SAV_temporal_scope_plots/rds/")

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
    if(unique(ma) %in% c("Banana River", "Indian River-Malabar to Vero Beach",
                         "Indian River-Vero Beach to Ft. Pierce", "Jensen Beach to Jupiter Inlet",
                         "Mosquito Lagoon")){
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

#Create map(s) for the managed area-------------------------------------------

for (i in unique(SAV4$ManagedAreaName)){
  # Determine unique programs for MA
  progs <- SAV4[ManagedAreaName == i & !is.na(BB_pct), unique(ProgramName)]
  # Set palette for these programs
  color_pal <- prcollist[round(seq(1, length(prcollist), length.out = length(progs)))]
  names(color_pal) <- progs
  
  ma_abrev <- MA_All %>% filter(ManagedAreaName==i) %>% pull(Abbreviation)

  fl_i <- st_crop(counties,
                  xmin = corners[LONG_NAME == i, xmin],
                  xmax = corners[LONG_NAME == i, xmax],
                  ymin = corners[LONG_NAME == i, ymin],
                  ymax = corners[LONG_NAME == i, ymax])

  rcp_i <- subset(rcp, rcp$LONG_NAME == i)

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

  # sbar <- st_linestring(x = matrix(c(st_bbox(fl_i)[[1]], st_bbox(fl_i)[[1]] + lon5km, st_bbox(fl_i)[[2]], st_bbox(fl_i)[[2]]), 2, 2), dim = "XYZ")
  # sbar <- sfheaders::sf_linestring(obj = matrix(c(st_bbox(fl_i)[[1]], st_bbox(fl_i)[[1]] + lon5km, st_bbox(fl_i)[[2]], st_bbox(fl_i)[[2]]), 2, 2), x = 1, y = 2)
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

  } else{
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
                         i %in% c("Banana River", "Indian River-Malabar to Vero Beach", "Indian River-Vero Beach to Ft. Pierce", "Jensen Beach to Jupiter Inlet",
                                  "Mosquito Lagoon"), x + x_sbarpos2, #(abs(min_x) - abs(max_x)) * ((lonkm/(max_x - min_x)) * x_sbarpos2),
                         corners[LONG_NAME == i, Coast[1]] == "Atlantic", x + x_sbarpos2), #(abs(min_x) - abs(max_x)) * ((lonkm/(max_x - min_x)) * x_sbarpos2)),
               y = fcase(corners[LONG_NAME == i, Coast[1]] == "Gulf", y - y_sbarpos2, #(abs(max_y) - abs(min_y)) * ((lonkm/(max_y - min_y)) * y_sbarpos2),
                         corners[LONG_NAME == i, Coast[1]] == "Panhandle", y + y_sbarpos2, #(abs(max_y) - abs(min_y)) * ((lonkm/(max_y - min_y)) * y_sbarpos2),
                         i %in% c("Banana River", "Indian River-Malabar to Vero Beach", "Indian River-Vero Beach to Ft. Pierce", "Jensen Beach to Jupiter Inlet",
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
  startyear <- min(SAV4[ManagedAreaName == i & !is.na(BB_pct), Year])

  base <- ggplot() +
    geom_sf(data = rotate_sf(fl_i, ma = i, coast = corners[LONG_NAME == i, Coast[1]]), fill = "beige", color = "navajowhite3", lwd = 0.5, inherit.aes = FALSE) +
    geom_sf(data = rotate_sf(rcp_i, ma = i, coast = corners[LONG_NAME == i, Coast[1]]), color = "grey50", fill = "grey70", alpha = 0.15, lwd = 0.5, inherit.aes = FALSE) +
    geom_sf(data = rotate_sf(sbar, ma = i, coast = corners[LONG_NAME == i, Coast[1]]), color = "grey50", linewidth = 1.25, inherit.aes = FALSE) +
    geom_sf(data = rotate_sf(narrow, ma = i, coast = corners[LONG_NAME == i, Coast[1]]), color = "grey50", linewidth = 1, inherit.aes = FALSE) +
    geom_sf_text(data = rotate_sf(sbarlab, ma = i, coast = corners[LONG_NAME == i, Coast[1]]), label = ifelse(wkm < 20, "3 km", ifelse(wkm < 50, "5 km", "10 km")), hjust = 0.5, angle = 4, color = "grey50", size = 3.5, inherit.aes = FALSE) +
    geom_sf_text(data = rotate_sf(narlab, ma = i, coast = corners[LONG_NAME == i, Coast[1]]), label = "N", hjust = 0.7, angle = 4, color = "grey50", size = 3.5, inherit.aes = FALSE) +
    scale_color_manual(values = color_pal, aesthetics = c("color", "fill")) +
    # scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(BB_pct), ProgramName])),
    #                    aesthetics = c("color", "fill")) +
    labs(title = paste0(i),
         subtitle = "SAV Percent Cover - Sample Locations",
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

  ###############

  bbox <- st_bbox(rotate_sf(rcp_i, x_add = xadd, y_add = yadd+maxydist, ma = i, coast = corners[LONG_NAME == i, Coast[1]]))
  max_width <- bbox$xmax - bbox$xmin
  x_increment <- max_width + 0.5

  ###############

  if(length(subset(locs_pts_rcp_i, locs_pts_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(BB_pct) & Year == startyear, LocationID]))$LocationID) > 0){
    base <- base +
      geom_sf(data = rotate_sf(subset(locs_pts_rcp_i, locs_pts_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(BB_pct) & Year == startyear, LocationID])),
                               ma = i, coast = corners[LONG_NAME == i, Coast[1]]),
              aes(fill = droplevels(as.factor(ProgramName))), shape = 21, color = "black", size=pt_size, alpha=0.8)
  }

  if(length(subset(locs_lns_rcp_i, locs_lns_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(BB_pct) & Year == startyear, LocationID]))$LocationID) > 0){
    base <- base +
      geom_sf(data = rotate_sf(subset(locs_lns_rcp_i, locs_lns_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(BB_pct) & Year == startyear, LocationID])),
                               ma = i, coast = corners[LONG_NAME == i, Coast[1]]),
              aes(color = droplevels(as.factor(ProgramName))), size=0.1, alpha=0.8, linewidth=3, lineend = "round")
  }

  years <- sort(unique(SAV4[ManagedAreaName == i & !is.na(BB_pct) & Year != startyear, Year]))
  total_years <- length(years)
  # num_col <- ifelse(i=="Biscayne Bay Aquatic Preserve", 2,
  #                   ifelse(total_years<=12, 2, ifelse(total_years<=24, 3, 4)))

  # 2 cols
  two_cols <- c("BBAP","BBSAP","BCBAP","BRAP","CHAP","CKRBAP","EBAP","FKNMS","LBAP","MLAP","PCAP","PISAP","TCAP","MPAP")
  # 3 cols
  three_cols <- c("GSCHAP","SMMAP","LRLWCAP","NCAP","PCAP")
  # 5 cols
  five_cols <- c("IRVBFPAP","IRMVBAP","JBJIAP")

  if(ma_abrev %in% two_cols){num_col<-2} else if(ma_abrev %in% three_cols){num_col<-3} else if(ma_abrev %in% five_cols){num_col<-5} else {num_col<-1}

  rows_per_column <- ceiling(total_years / num_col)

  for(index in seq_along(years)){
    y <- years[index]

    base <- base +
      geom_sf(data = rotate_sf(rcp_i, x_add = xadd + maxxdist, y_add = yadd + maxydist, ma = i, coast = corners[LONG_NAME == i, Coast[1]]),
              color = "grey50", fill = "grey70", alpha = 0.15, lwd = 0.5, inherit.aes = FALSE) +
      annotate("text", x = xlab + xadd + maxxdist, y = xmax_y + yadd + maxydist, label = y, hjust = "left")

    if(length(subset(locs_pts_rcp_i, locs_pts_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(BB_pct) & Year == y, LocationID]))$LocationID) > 0){
      base <- base +
        geom_sf(data = rotate_sf(subset(locs_pts_rcp_i, locs_pts_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(BB_pct) & Year == y, LocationID])),
                                 x_add = xadd + maxxdist, y_add = yadd + maxydist, ma = i, coast = corners[LONG_NAME == i, Coast[1]]),
                aes(fill = droplevels(as.factor(ProgramName))), shape = 21, color = "black", size=pt_size, alpha=0.8)
    }

    if(length(subset(locs_lns_rcp_i, locs_lns_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(BB_pct) & Year == startyear, LocationID]))$LocationID) > 0){
      base <- base +
        geom_sf(data = rotate_sf(subset(locs_lns_rcp_i, locs_lns_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(BB_pct) & Year == startyear, LocationID])),
                                 x_add = xadd + maxxdist, y_add = yadd + maxydist, ma = i, coast = corners[LONG_NAME == i, Coast[1]]),
                aes(color = droplevels(as.factor(ProgramName))), size=0.1, alpha=0.8, linewidth=3, lineend = "round")
    }

    yadd <- yadd + maxydist
    xadd <- xadd + maxxdist

    if (index %% rows_per_column == 0) {
      yadd <- 0
      xadd <- xadd + x_increment
    }
  }

  base <- base +
    theme(legend.position='bottom',
          legend.justification='left',
          legend.direction='vertical')

  saveRDS(base, paste0("output/SAV_temporal_scope_plots/rds/SAV_",
                                  ma_abrev,
                                  "_map_bypr.rds"))

  ggsave(filename = paste0("output/SAV_temporal_scope_plots/SAV_",
                                      ma_abrev,
                                      "_map_bypr.jpg"),
         plot = base,
         dpi = 300,
         limitsize = FALSE,
         width = 15,
         height = 18)
  rm(base)
  print(paste0(i, " - SAV Scope plot object created"))
}

# plotbuild <- ggplot_build(base)
# hwratio <- (plotbuild$layout$panel_scales_y[[1]]$range$range[2] - plotbuild$layout$panel_scales_y[[1]]$range$range[1]) / (plotbuild$layout$panel_scales_x[[1]]$range$range[2] - plotbuild$layout$panel_scales_x[[1]]$range$range[1])
# pwidth <- 6