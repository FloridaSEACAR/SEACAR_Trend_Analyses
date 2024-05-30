#Starting over converting all percent cover metrics to BB
library(tidyverse)
library(data.table)
library(scales)
library(brms)
library(broom.mixed)
library(tidybayes)
library(bayesplot)
library(sf)
library(gtable)
library(grid)
library(gridExtra)
library(tictoc)
library(nlme)
library(colorspace)
library(here)
library(patchwork)
#library(future)
library(extrafont)
library(magick)
library(rstudioapi)
# library(ggspatial)
# font_import()
# loadfonts()

wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

source("../seacar_data_location.R")

#Create folder paths if not already created
folder_paths <- c("output/models", "output/Figures", "output/Figures/BB/", 
                  "output/tables", "output/tables/SAV", 
                  "output/website/images/", "output/website/images/multiplots",
                  "output/website/images/trendplots","output/website/images/barplots")
for (path in folder_paths){if(!dir.exists(path)){dir.create(path)}}

# Initialize data directory to store model results
data_directory <- list()

#Load and wrangle data------------------------------------------------------------
file_in <- list.files(seacar_data_location, pattern="All_SAV", full=TRUE)
SAV <- fread(file_in, sep = "|", header = TRUE, stringsAsFactors = FALSE,
             na.strings=c("NULL","","NA"))

SAV <- SAV[!is.na(ResultValue), ]

# Create data columns based on old parameter results to make script run
SAV$BB <- NA
SAV$mBB <- NA
SAV$PC <- NA
SAV$PO <- NA
SAV$PA <- NA

# Fill created columns with values based on parameter names
SAV$BB[SAV$ParameterName=="Braun Blanquet Score"] <-
  SAV$ResultValue[SAV$ParameterName=="Braun Blanquet Score"]

SAV$mBB[SAV$ParameterName=="Modified Braun Blanquet Score"] <-
  SAV$ResultValue[SAV$ParameterName=="Modified Braun Blanquet Score"]

SAV$PC[SAV$ParameterName=="Percent Cover"] <-
  SAV$ResultValue[SAV$ParameterName=="Percent Cover"]

SAV$PO[SAV$ParameterName=="Percent Occurrence"] <-
  SAV$ResultValue[SAV$ParameterName=="Percent Occurrence"]

SAV$PA[SAV$ParameterName=="Presence/Absence"] <-
  SAV$ResultValue[SAV$ParameterName=="Presence/Absence"]

SAV2 <- subset(SAV, !is.na(SAV$BB) | !is.na(SAV$mBB) | !is.na(SAV$PC) | !is.na(SAV$PO))
SAV2 <- SAV2 %>% filter(BB >= 0 & BB <= 5 | is.na(BB))
SAV2 <- SAV2 %>% filter(mBB >= 0 & mBB <= 5 | is.na(mBB))
SAV2 <- SAV2 %>% filter(PC >= 0 & PC <= 100 | is.na(PC))
SAV2 <- SAV2 %>% filter(PO >= 0 & PO <= 100 | is.na(PO))
SAV2 <- SAV2 %>% filter(Month %in% c(4:10))
setDT(SAV2)

SAV2[!is.na(BB), BB_all := fcase(BB == 0, 0, 
                                 BB > 0 & BB <= 1, 1,
                                 BB > 1, round(BB))]
SAV2[!is.na(mBB), BB_all := fcase(mBB == 0, 0,
                                  mBB > 0 & mBB <= 1, 1, 
                                  mBB > 1, round(mBB))]
SAV2[!is.na(PC), BB_all := fcase(PC == 0, 0,
                                 PC > 0 & PC <= (2.5 + (15-2.5)/2), 1,
                                 PC <= (2.5 + (15-2.5) + (37.5-15)/2), 2,
                                 PC <= (2.5 + (15-2.5) + (37.5-15) + (62.5 - 37.5)/2), 3,
                                 PC <= (2.5 + (15-2.5) + (37.5-15) + (62.5 - 37.5) + (87.5 - 62.5)/2), 4, 
                                 PC > (2.5 + (15-2.5) + (37.5-15) + (62.5 - 37.5) + (87.5 - 62.5)/2), 5)]


#Replaces two blocks of code above by using the BB_all variable to create all estimates at once.
SAV2[!is.na(BB_all), BB_pct := fcase(BB_all == 0, 0, 
                                     BB_all > 0 & BB_all <= 0.1, rescale(BB_all, from=c(0, 0.1), to=c(0,0.02)), #Added by SRD 8/31/2021
                                     BB_all > 0.1 & BB_all <= 0.5, rescale(BB_all, from=c(0.1, 0.5), to=c(0.02,0.1)),
                                     BB_all > 0.5 & BB_all <= 1, rescale(BB_all, from=c(0.5,1), to=c(0.1,2.5)),
                                     BB_all > 1 & BB_all <= 2, rescale(BB_all, from=c(1,2), to=c(2.5,15)),
                                     BB_all > 2 & BB_all <= 3, rescale(BB_all, from=c(2,3), to=c(15,37.5)),
                                     BB_all > 3 & BB_all <= 4, rescale(BB_all, from=c(3,4), to=c(37.5,62.5)),
                                     BB_all > 4 & BB_all <= 5, rescale(BB_all, from=c(4,5), to=c(62.5,87.5)))]

SAV2[, BB_pct := as.numeric(BB_pct)]
SAV2[, BB_all := as.ordered(BB_all)]
SAV2[!is.na(PO), method := "Percent Occurrence"]
SAV2[!is.na(BB), method := "Braun Blanquet"]
SAV2[!is.na(mBB), method := "Modified Braun Blanquet"]
SAV2[!is.na(PC), method := "Percent Cover"]

SAV2[!is.na(BB_all), PA := ifelse(BB_all == 0, 0, 1)]
SAV2[!is.na(PO), PA := ifelse(PO == 0, 0, 1)]

SAV2[, relyear := Year - min(Year)]

SAV3 <- SAV2[SpeciesGroup1 %in% c("Seagrass", "Macroalgae", "Total SAV"), ]

species_reject <- c("All", "NA",
                    "Vallisneria americana", "Najas guadalupensis",
                    "Hydrilla verticillata", "Potamogeton pusillus",
                    "Zannichellia palustris")

SAV3[, `:=` (analysisunit_halid = ifelse(CommonIdentifier %in% species_reject, NA, 
                                         ifelse(str_detect(CommonIdentifier, "Halophila") & is.na(SpeciesName), "Unidentified Halophila", 
                                                ifelse(SpeciesGroup1 %in% c("Seagrass","Total SAV"), CommonIdentifier, Drift_Attached))),
             analysisunit = ifelse(CommonIdentifier %in% species_reject, NA, 
                                   ifelse(str_detect(CommonIdentifier, "Halophila"), "Halophila spp.", 
                                          ifelse(SpeciesGroup1 %in% c("Seagrass","Total SAV"), CommonIdentifier, Drift_Attached))))]

SAV3[str_detect(analysisunit, "Drift|Attached"), `:=` (analysisunit = paste0(analysisunit, " algae"))]
SAV3[str_detect(analysisunit_halid, "Drift|Attached"), `:=` (analysisunit_halid = paste0(analysisunit_halid, " algae"))]

SAV4 <- subset(SAV3, !is.na(SAV3$analysisunit))

saveRDS(SAV4, "output/SAV_DataUsed.rds")
fwrite(SAV4, "output/SAV_DataUsed.txt", sep = "|")

rm(SAV, SAV2, SAV3)

MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE, na.strings = "")

# modified version of previous addfits_blacktrendlines function to create multi-plots
addfits_multiplots <- function(models, plot_i, param, aucol){
  for(n in 1:length(models)){
    model_name <- names(models[n])
    model <- models[[model_name]]
    
    sp <- unique(model$data[[aucol]])
    
    species_data <- SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(as.name(aucol)) == sp, ]
    species_data$predictions <- predict(model, level = 0)
    
    plot_i <- plot_i +
      geom_line(data = species_data,
                aes(x = relyear, y = predictions), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)
  }
  
  # order_match <- ifelse(usenames=="common", "order(match(spp_common))", "order(match(spp))")
  
  if(usenames=="common"){
    plot_i <- plot_i +
      facet_wrap(~factor(modify_species_labels(eval(aucol), usenames),
                         levels = c("Total SAV",
                                    "Total seagrass",
                                    "Halophila spp.",
                                    "Halophila, unk.",
                                    "Johnson's seagrass",
                                    "Manatee grass",
                                    "Paddle grass",
                                    "Shoal grass",
                                    "Star grass",
                                    "Turtle grass",
                                    "Widgeon grass",
                                    "Attached algae",
                                    "Drift algae",
                                    "No grass In Quadrat")),
                 ncol = 3, strip.position = "top")
  } else if(usenames=="scientific") {
    plot_i <- plot_i +
      facet_wrap(~factor(modify_species_labels(eval(aucol), usenames),
                         levels = c("Total SAV",
                                    "Total seagrass",
                                    "Halophila spp.",
                                    "Halophila, unk.",
                                    "Halophila johnsonii",
                                    "Syringodium filiforme",
                                    "Halophila decipiens",
                                    "Halodule wrightii",
                                    "Halophila engelmannii",
                                    "Thalassia testudinum",
                                    "Ruppia maritima",
                                    "Attached algae",
                                    "Drift algae",
                                    "No grass In Quadrat")),
                 ncol = 3, strip.position = "top")
  }
  
  return(plot_i)
}

# declaring addfits function which plots Percent Cover models on a single plot
addfits <- function(models, plot_i, param) {
  # aucol determines whether analysisunit or analysisunit_halid is used
  aucol <- as.name(names(plot_i$data)[1])
  # empty data frame to fill with regression data
  regression_data <- data.frame()
  plot_data <- data.frame()
  
  for (i in seq_along(models)) {
    # finding model name, calling previously created model variable
    model_name <- names(models[i])
    model <- models[[i]]
    
    # selecting for Total SAV and Total Seagrass to apply aesthetic conditions later
    is_ToSa <- grepl("ToSa", model_name)
    is_ToSe <- grepl("ToSe", model_name)
    exclude <- c("DrAl","AtAl")
    
    # declaring species & managed area of each model
    species <- unique(model$data[[aucol]])
    managed_area <- unique(model$data$ManagedAreaName)
    
    #extract p-value
    p_val <- summary(model)$tTab[2,5]
    
    # exclude Drift algae from model plots
    if(!grepl(paste(exclude, collapse='|'), model_name)) {
      
      linetypes <- "solid"
      size <- 1
      alpha <- 1
      #alpha <- if (p_val <= 0.05) 1 else 0.8
      
      # filter dataframe for managed_area & species
      species_data <- SAV4 %>%
        filter(ManagedAreaName == managed_area,
               !is.na({{p}}),
               {{ aucol }} == species)
      
      # create predicted values variable for each model
      predicted_values <- predict(model, level = 0, newdata = species_data)
      
      # separate significant values
      significant <- if (p_val <=0.05) TRUE else FALSE
      
      # Add predicted values to the regression_data dataframe, with species & relyear
      regression_data <- rbind(regression_data, data.frame(
        relyear = species_data$relyear,
        fit = predicted_values,
        species = unique(species_data[[aucol]]),
        significance = significant))
      
      # in case we separate Total SAV and Total seagrass and treat them differently
      #if (is_ToSa || is_ToSe) {} else {}
      # regression_data <- regression_data %>%
      #   filter(!species %in% c("Total SAV", "Total seagrass"))
      
      # Plot all other species
      plot_i <- plot_i +
        geom_line(data = regression_data,
                  aes(x = relyear, y = fit, color=species, linetype=factor(significance)),
                  size=size, alpha=alpha, inherit.aes = FALSE) +
        # geom_bar(data = plot_dat, aes(x=relyear, y=npt), stat = "identity") +
        scale_linetype_manual(name="Trend significance (alpha = 0.05)",
                              values=c("TRUE" = "solid", "FALSE" = "dotdash"),
                              labels=c("TRUE" = "Significant", "FALSE" = "Not significant")) +
        # setting order of the legends, color first
        guides(color = guide_legend(order=1),
               linetype = guide_legend(order=2))
    }
  }
  
  # creating color scale so names line up correctly in legend
  species_list <- c("")
  
  for (l in plot_i[["layers"]]) {
    new_species <- unique(l$data$species[!l$data$species %in% species_list])
    if (length(new_species) > 0) {
      species_list <- append(species_list, new_species)
    }
  }
  
  # ordering species list to match spcols, with Total SAV & Total seagrass at bottom, otherwise alphabetical (Hal spp. at top)
  species_list <- species_list[order(match(species_list, names(spcols)))]
  
  # determining if scientific or common names
  species_labels <- modify_species_labels(species_list, usenames)
  
  plot_i <- plot_i + scale_color_manual(values = subset(spcols, names(spcols) %in% species_list),
                                        breaks = species_list,
                                        labels = species_labels)
  
  return(plot_i)
}

# function to modify species labels prior to plotting (sci vs common names)
# also replaces "Unidentified Halophila" with "Halophila, unk."
modify_species_labels <- function(species_list, usenames) {
  
  if(usenames == "scientific") {
    lab <- species_list
  } else {
    lab <- sapply(species_list, function(name) {
      match_idx <- match(name, spp)
      if (!is.na(match_idx)) {
        return(spp_common[match_idx])
      }
      return(name)
    })
  }
  lab <- str_replace_all(lab, "Unidentified Halophila", "Halophila, unk.")
  return(lab)
}

# Specify what to produce --------------
EDA <- "no" #Create and export Exploratory Data Analysis plots ("maps and plots" = create all EDA output, 
            #                                                   "maps" = create geographic scope maps only,
            #                                                   "plots" = create data exploration plots only,
            #                                                   "no" (or anything else) = skip all EDA output)

Analyses <- c("BB_pct", "PC", "PA") #Which analyses to run? c("BB_all," "BB_pct", "PC", "PO", and/or "PA") or c("none") for just EDA plotting

if(str_detect(EDA, "maps")){
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
  
  
  #Create model objects, tables and plots for all MAs w/ >5 yrs of data-------------------------------------------------
  #Load geospatial data
  GeoDBdate <- "6june2023"
  locs_pts <- st_read(here::here(paste0("SAV/mapping/SampleLocations", GeoDBdate, "/seacar_dbo_vw_SampleLocation_Point.shp")))
  locs_lns <- st_read(here::here(paste0("SAV/mapping/SampleLocations", GeoDBdate, "/seacar_dbo_vw_SampleLocation_Line.shp")))
  rcp <- st_read(here::here("SAV/mapping/orcp_all_sites/orcp_all_sites/ORCP_Managed_Areas.shp"))
  counties <- st_read(here::here("SAV/mapping/FLCounties/Counties_-_Detailed_Shoreline.shp"))
  corners <- fread(here::here("SAV/mapping/MApolygons_corners.csv"))
  #add 20% of difference (xmax-xmin) to xmax to help prevent year labels from getting cut off map images and 10% to ymax
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
}

#Empty data.table to house names of any failed models generated below.
failedmods <- data.table(model = character(),
                         error = character())

#Create a table of the proportion of present SAV types by managed area and year
props_halid <- SAV4 %>% 
  filter(str_detect(analysisunit_halid, "Total|Drift|spp\\.", negate = TRUE), !is.na(PA)) %>% 
  group_by(ManagedAreaName, analysisunit_halid, relyear) %>% 
  summarize(n_P = sum(PA), ntot_PA = n(), prop_P = n_P/ntot_PA)

props <- SAV4 %>% 
  filter(str_detect(analysisunit, "Total|Drift|decipiens|engelmannii|johnsonii|Unidentified", negate = TRUE), !is.na(PA)) %>% 
  group_by(ManagedAreaName, analysisunit, relyear) %>% summarize(n_P = sum(PA), ntot_PA = n(), prop_P = n_P/ntot_PA)

setDT(props_halid)
setDT(props)

for(m in unique(props_halid$ManagedAreaName)){
  props_halid[ManagedAreaName == m, `:=` (n_allsp_P = sum(n_P), sp_prop = n_P/sum(n_P), sp_pct = (n_P/sum(n_P)) * 100), by = c("relyear")]
}
for(m in unique(props$ManagedAreaName)){
  props[ManagedAreaName == m, `:=` (n_allsp_P = sum(n_P), sp_prop = n_P/sum(n_P), sp_pct = (n_P/sum(n_P)) * 100), by = c("relyear")]
}

setnames(props_halid, "analysisunit_halid", "analysisunit")
props2 <- distinct(rbind(props_halid, props))
setorder(props2, ManagedAreaName, relyear, analysisunit)
props <- props2

spcollist <- c("#005396","#005396",
               "#0088B1",
               "#00ADAE",
               "#65CCB3",
               "#AEE4C1",
               "#FDEBA8",
               "#F8CD6D",
               "#F5A800",
               "#F17B00",
               "#900667",
               "#000099")

spp <- c("Halophila spp.","Unidentified Halophila","Halophila johnsonii","Syringodium filiforme","Halophila decipiens","Halodule wrightii",
         "Halophila engelmannii","Thalassia testudinum","Ruppia maritima","Attached algae", "Total SAV", "Total seagrass")

spp_common <- c("Halophila spp.", "Unidentified Halophila", "Johnson's seagrass", "Manatee grass", "Paddle grass", 
                "Shoal grass", "Star grass", "Turtle grass", "Widgeon grass", "Attached algae", "Total SAV", "Total seagrass")

# Script now defaults to scientific throughout, will change labels
# in final steps when plots are created if "common" is chosen
usenames <- "common" #alternative is "scientific"

spcols <- setNames(spcollist, spp)

props <- props[, analysisunit := factor(analysisunit, levels = c("Unidentified Halophila",
                                                                 "Halophila spp.",
                                                                 "Halophila johnsonii",
                                                                 "Syringodium filiforme",
                                                                 "Halophila decipiens",
                                                                 "Halodule wrightii",
                                                                 "Halophila engelmannii",
                                                                 "Thalassia testudinum",
                                                                 "Ruppia maritima",
                                                                 "Attached algae"))]

# prcollist <- hcl.colors(n = length(unique(SAV4$ProgramID)), palette = "viridis")
prcollist_a <- sequential_hcl(length(unique(SAV4$ProgramName)), palette = "YlOrRd")
prcollist_b <- sequential_hcl(length(unique(SAV4$ProgramName)), palette = "YlGnBu", rev = TRUE)
prcollist <- append(prcollist_a[which(seq(1, length(prcollist_a)) %% 2 == 0)], 
                    prcollist_b[which(seq(1, length(prcollist_b)) %% 2 != 0)])
prcollist <- rev(prcollist)
set.seed(4691)
progs <- sample(sort(unique(SAV4$ProgramName)))
prcols <- setNames(prcollist, progs)

# parameters <- data.table(column = c(as.name("BB_all"), as.name("BB_pct"), as.name("PC"), as.name("PO"), as.name("PA")),
#                          name = c("Braun Blanquet score", "Median percent cover", "Visual percent cover", "Percent occurrence", "Frequency of occurrence"),
#                          type = c("BBall", "BBpct", "PC", "PO", "PA"))

parameters <- data.table(column = c(as.name("BB_pct"), as.name("PC"), as.name("PA")),
                         name = c("Median percent cover", "Visual percent cover", "Frequency of occurrence"),
                         type = c("BBpct", "PC", "PA"))

plot_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Arial"),
        # title = element_text(face="bold"),
        plot.title = element_text(hjust = 0.5, size = 12, color = "#314963"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "#314963"),
        legend.title = element_text(size = 10),
        legend.text.align = 0,
        axis.title.x = element_text(size = 10, margin = margin(t = 5, r = 0,
                                                               b = 10, l = 0)),
        axis.title.y = element_text(size = 10, margin = margin(t = 0, r = 10,
                                                               b = 0, l = 0)),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = -45, hjust = 0))

#Managed areas that should have Halophila species combined:
ma_halspp <- c("Banana River Aquatic Preserve", "Indian River-Malabar to Vero Beach Aquatic Preserve", 
               "Indian River-Vero Beach to Ft. Pierce Aquatic Preserve", "Jensen Beach to Jupiter Inlet Aquatic Preserve",
               "Loxahatchee River-Lake Worth Creek Aquatic Preserve", "Mosquito Lagoon Aquatic Preserve", 
               "Biscayne Bay Aquatic Preserve", "Florida Keys National Marine Sanctuary")

#save summary stats file
stats_pct <- SAV4[ManagedAreaName %in% ma_halspp, ] %>%
  group_by(ManagedAreaName, analysisunit) %>%
  summarize(ParameterName="Median percent cover (from BB scores)",
            N_Programs=length(unique(ProgramID)),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '),
            N_Data=length(BB_pct[!is.na(BB_pct)]),
            N_Years=length(unique(Year[!is.na(Year) & !is.na(BB_pct)])),
            EarliestYear=min(Year[!is.na(BB_pct)]),
            LatestYear=max(Year[!is.na(BB_pct)]),
            SufficientData=ifelse(N_Data>0 & N_Years>=5, TRUE, FALSE))
stats_pct2 <- SAV4[ManagedAreaName %in% setdiff(unique(SAV4$ManagedAreaName), ma_halspp), ] %>%
  group_by(ManagedAreaName, analysisunit_halid) %>%
  summarize(ParameterName="Median percent cover (from BB scores)",
            N_Programs=length(unique(ProgramID)),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '),
            N_Data=length(BB_pct[!is.na(BB_pct)]),
            N_Years=length(unique(Year[!is.na(Year) & !is.na(BB_pct)])),
            EarliestYear=min(Year[!is.na(BB_pct)]),
            LatestYear=max(Year[!is.na(BB_pct)]),
            SufficientData=ifelse(N_Data>0 & N_Years>=5, TRUE, FALSE))
setDT(stats_pct2)
setnames(stats_pct2, "analysisunit_halid", "analysisunit")
stats_pct <- distinct(rbind(stats_pct, stats_pct2))
setcolorder(stats_pct, c("ManagedAreaName", "analysisunit"))
setDT(stats_pct)
stats_pct[N_Years == 0, `:=` (EarliestYear = NA, LatestYear = NA)]

data.table::fwrite(stats_pct, "output/SAV_BBpct_Stats.txt", sep = "|")

stats_pa <- SAV4[ManagedAreaName %in% ma_halspp, ] %>%
  group_by(ManagedAreaName, analysisunit) %>%
  summarize(ParameterName="Frequency of occurrence",
            N_Programs=length(unique(ProgramID)),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '),
            N_Data=length(PA[!is.na(PA)]),
            N_Years=length(unique(Year[!is.na(Year) & !is.na(PA)])),
            EarliestYear=min(Year[!is.na(PA)]),
            LatestYear=max(Year[!is.na(PA)]),
            SufficientData=ifelse(N_Data>0 & N_Years>=5, TRUE, FALSE))
stats_pa2 <- SAV4[ManagedAreaName %in% setdiff(unique(SAV4$ManagedAreaName), ma_halspp), ] %>%
  group_by(ManagedAreaName, analysisunit_halid) %>%
  summarize(ParameterName="Frequency of occurrence",
            N_Programs=length(unique(ProgramID)),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '),
            N_Data=length(PA[!is.na(PA)]),
            N_Years=length(unique(Year[!is.na(Year) & !is.na(PA)])),
            EarliestYear=min(Year[!is.na(PA)]),
            LatestYear=max(Year[!is.na(PA)]),
            SufficientData=ifelse(N_Data>0 & N_Years>=5, TRUE, FALSE))
setDT(stats_pa2)
setnames(stats_pa2, "analysisunit_halid", "analysisunit")
stats_pa <- distinct(rbind(stats_pa, stats_pa2))
setcolorder(stats_pa, c("ManagedAreaName", "analysisunit"))
setDT(stats_pa)
stats_pa[N_Years == 0, `:=` (EarliestYear = NA, LatestYear = NA)]

# fwrite(stats2, here::here(paste0("output/data/SAV_BBpct_PA_Stats", Sys.Date(), ".txt")), sep = "|")
statpardat <- list("BB_pct" = stats_pct, "PA" = stats_pa)
openxlsx::write.xlsx(statpardat, here::here("output/SAV_BBpct_PA_Stats.xlsx"), colNames = c(TRUE, TRUE), colWidths = c("auto", "auto"), firstRow = c(TRUE, TRUE))
openxlsx::write.xlsx(statpardat, here::here(paste0("output/SAV_BBpct_PA_Stats_", Sys.Date(), ".xlsx")), colNames = c(TRUE, TRUE), colWidths = c("auto", "auto"), firstRow = c(TRUE, TRUE))

# #subset to run only part of the script------------------------------------------------------
# parameters <- parameters[column == "PA", ]

#Save session info-----------------------------------------------------
session <- sessionInfo()
saveRDS(session, here::here(paste0("SAV/output/SessionInfo_", Sys.Date())))

#start script----------------------------------------------------------------------
tic()
n <- 0
seed <- 352
set.seed(seed)

for(p in parameters$column){
  
  cat(paste0("\nStarting indicator: ", p, "\n"))
  
  #List managed areas with at least 5 years of data
  nyears <- SAV4[!is.na(eval(p)) & !is.na(analysisunit), ] %>% group_by(ManagedAreaName, analysisunit) %>% summarize(type = paste0(p), nyr = length(unique(Year)))
  nyears2 <- SAV4[!is.na(eval(p)) & !is.na(analysisunit_halid), ] %>% group_by(ManagedAreaName, analysisunit_halid) %>% summarize(type = paste0(p), nyr = length(unique(Year)))
  setDT(nyears2)
  setnames(nyears2, "analysisunit_halid", "analysisunit")
  nyears <- distinct(rbind(nyears, nyears2))
  ma_include <- unique(subset(nyears, nyears$nyr >= 5)$ManagedAreaName)
  
  #For each managed area, make sure there are multiple levels of BB scores per species; remove ones that don't from further consideration.
  for(i in ma_include){
    
    ma_abrev <- MA_All %>% filter(ManagedAreaName==i) %>% pull(Abbreviation)
    
    cat(paste0("\nStarting MA: ", i, "\n"))
    
    #create data exploration plots-----------------------------------------------------
    if(EDA %in% c("plots", "maps", "plots and maps")){
      if(str_detect(EDA, "plots")){
        parvYear_bysp <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ],
                                aes(x = Year, y = eval(p), color = analysisunit)) +
          geom_jitter() +
          theme_bw() +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
               y = parameters[column == p, name],
               color = "Species") +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                             aesthetics = c("color", "fill"))
        
        saveRDS(parvYear_bysp, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                 gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                 ifelse(stringr::str_detect(i, "NERR"), paste0("ERR_EDA001_", str_replace(p, "_", ""), "vYear_bysp.rds"), 
                                                        ifelse(stringr::str_detect(i, "NMS"), paste0("MS_EDA001_", str_replace(p, "_", ""), "vYear_bysp.rds"), paste0("AP_EDA001_", str_replace(p, "_", ""), "vYear_bysp.rds"))))))
        
        parvYear_bypr <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = eval(p), color = as.factor(ProgramID))) +
          geom_jitter() +
          theme_bw() +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
               y = parameters[column == p, name],
               color = "Program ID") +
          scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), as.factor(ProgramID)])), 
                             aesthetics = c("color", "fill"))
        
        saveRDS(parvYear_bypr, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                 gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                 ifelse(stringr::str_detect(i, "NERR"), paste0("ERR_EDA002_", str_replace(p, "_", ""), "vYear_bypr.rds"), 
                                                        ifelse(stringr::str_detect(i, "NMS"), paste0("MS_EDA002_", str_replace(p, "_", ""), "vYear_bypr.rds"), paste0("AP_EDA002_", str_replace(p, "_", ""), "vYear_bypr.rds"))))))
        
        spvYear_bypr <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = analysisunit, color = as.factor(ProgramID))) +
          geom_jitter() +
          theme_bw() +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
               y = "Species",
               color = "Program ID") +
          scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), as.factor(ProgramID)])), 
                             aesthetics = c("color", "fill"))
        
        saveRDS(spvYear_bypr, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA003_spvYear_bypr.rds", 
                                                       ifelse(stringr::str_detect(i, "NMS"), "MS_EDA003_spvYear_bypr.rds", "AP_EDA003_spvYear_bypr.rds")))))
        
        qsvYear_bysp <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = QuadSize_m2, color = analysisunit)) +
          geom_jitter() +
          theme_bw() +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
               y = "Quadrat size (m^2)",
               color = "Species") +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                             aesthetics = c("color", "fill"))
        
        saveRDS(qsvYear_bysp, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA004_qsvYear_bysp.rds", 
                                                       ifelse(stringr::str_detect(i, "NMS"), "MS_EDA004_qsvYear_bysp.rds", "AP_EDA004_qsvYear_bysp.rds")))))
        
        qsvYear_bypr <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = QuadSize_m2, color = as.factor(ProgramID))) +
          geom_jitter() +
          theme_bw() +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
               y = "Quadrat size (m^2)",
               color = "Program ID") +
          scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), as.factor(ProgramID)])), 
                             aesthetics = c("color", "fill"))
        
        saveRDS(qsvYear_bypr, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA005_qsvYear_bypr.rds", 
                                                       ifelse(stringr::str_detect(i, "NMS"), "MS_EDA005_qsvYear_bypr.rds", "AP_EDA005_qsvYear_bypr.rds")))))
        
        metvYear_bysp <- ggplot(data = SAV4[ManagedAreaName == i, ], aes(x = Year, y = method, color = analysisunit)) +
          geom_jitter() +
          theme_bw() +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
               y = "Method",
               color = "Species") +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                             aesthetics = c("color", "fill"))
        
        saveRDS(metvYear_bysp, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                 gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                 ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA006_metvYear_bysp.rds", 
                                                        ifelse(stringr::str_detect(i, "NMS"), "MS_EDA006_metvYear_bysp.rds", "AP_EDA006_metvYear_bysp.rds")))))
        
        metvYear_bypr <- ggplot(data = SAV4[ManagedAreaName == i, ], aes(x = Year, y = method, color = as.factor(ProgramID))) +
          geom_jitter() +
          theme_bw() +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
               y = "Method",
               color = "Program ID") +
          scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), as.factor(ProgramID)])), 
                             aesthetics = c("color", "fill"))
        
        saveRDS(metvYear_bypr, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                 gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                 ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA007_metvYear_bypr.rds", 
                                                        ifelse(stringr::str_detect(i, "NMS"), "MS_EDA007_metvYear_bypr.rds", "AP_EDA007_metvYear_bypr.rds")))))
        
        metvqs_bysp <- ggplot(data = SAV4[ManagedAreaName == i, ], aes(x = QuadSize_m2, y = method, color = analysisunit)) +
          geom_jitter() +
          theme_bw() +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
               x = "Quadrat size (m^2)",
               y = "Method",
               color = "Species") +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                             aesthetics = c("color", "fill"))
        
        saveRDS(metvqs_bysp, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                               gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                               ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA008_metvqs_bysp.rds", 
                                                      ifelse(stringr::str_detect(i, "NMS"), "MS_EDA008_metvqs_bysp.rds", "AP_EDA008_metvqs_bysp.rds")))))
        
        metvqs_bypr <- ggplot(data = SAV4[ManagedAreaName == i, ], aes(x = QuadSize_m2, y = method, color = as.factor(ProgramID))) +
          geom_jitter() +
          theme_bw() +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
               x = "Quadrat size (m^2)",
               y = "Method",
               color = "Program ID") +
          scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), as.factor(ProgramID)])), 
                             aesthetics = c("color", "fill"))
        
        saveRDS(metvqs_bypr, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                               gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                               ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA009_metvqs_bypr.rds", 
                                                      ifelse(stringr::str_detect(i, "NMS"), "MS_EDA009_metvqs_bypr.rds", "AP_EDA009_metvqs_bypr.rds")))))
        
        if(length(SAV4[ManagedAreaName == i & !is.na(eval(p)) & !is.na(Grid), Grid]) > 0){              
          grvYear_bysp <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = Grid, color = analysisunit)) +
            geom_jitter() +
            theme_bw() +
            labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                                ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
                 y = "Grid number",
                 color = "Species") +
            scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                               aesthetics = c("color", "fill"))
          
          saveRDS(grvYear_bysp, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                  gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                  ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA010_grvYear_bysp.rds", 
                                                         ifelse(stringr::str_detect(i, "NMS"), "MS_EDA010_grvYear_bysp.rds", "AP_EDA010_grvYear_bysp.rds")))))
          
          grvYear_bypr <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = Grid, color = as.factor(ProgramID))) +
            geom_jitter() +
            theme_bw() +
            labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                                ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
                 y = "Grid number",
                 color = "Program ID") +
            scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), as.factor(ProgramID)])), 
                               aesthetics = c("color", "fill"))
          
          saveRDS(grvYear_bypr, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                  gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                  ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA011_grvYear_bypr.rds", 
                                                         ifelse(stringr::str_detect(i, "NMS"), "MS__EDA011_grvYear_bypr.rds", "AP_EDA011_grvYear_bypr.rds")))))
        }
        
        if(length(SAV4[ManagedAreaName == i & !is.na(eval(p)) & !is.na(Depth_M), Depth_M]) > 0){                
          dpvYear_bysp <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = Depth_M, color = analysisunit)) +
            geom_jitter() +
            theme_bw() +
            labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                                ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), "National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
                 y = "Depth (m)",
                 color = "Species") +
            scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                               aesthetics = c("color", "fill"))
          
          saveRDS(dpvYear_bysp, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                  gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                  ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA012_dpvYear_bysp.rds", 
                                                         ifelse(stringr::str_detect(i, "NMS"), "MS_EDA012_dpvYear_bysp.rds", "AP_EDA012_dpvYear_bysp.rds")))))
          
          dpvYear_bypr <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = Depth_M, color = as.factor(ProgramID))) +
            geom_jitter() +
            theme_bw() +
            labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                                ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
                 y = "Depth (m)",
                 color = "Program ID") +
            scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), as.factor(ProgramID)])), 
                               aesthetics = c("color", "fill"))
          
          saveRDS(dpvYear_bypr, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                  gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                  ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA013_dpvYear_bypr.rds", 
                                                         ifelse(stringr::str_detect(i, "NMS"), "MS_EDA013_dpvYear_bypr.rds", "AP_EDA013_dpvYear_bypr.rds")))))
        }
        
        
        #Generate the legend
        plotall <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, fill = analysisunit)) +
          geom_bar()  +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                             aesthetics = c("color", "fill")) +
          labs(y="Frequency of data", x="Year") +
          theme(legend.title = element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1),
                axis.title = element_text(size = 7),
                axis.text = element_text(size = 7),
                legend.text = element_text(size = 7))
        
        legend = gtable::gtable_filter(ggplotGrob(plotall), "guide-box")
        
        saveRDS(legend, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                          gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                          ifelse(stringr::str_detect(i, "NERR"), "ERR_hist_specieslegend.rds", 
                                                 ifelse(stringr::str_detect(i, "NMS"), "MS_hist_specieslegend.rds", "AP_hist_specieslegend.rds")))))
        
        #Create and save the hist objects---------------------------------------------------
        for(a in setdiff(unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit]), c("Total seagrass", "Attached algae", "Drift algae"))){
          dat <- filter(SAV4[ManagedAreaName == i & !is.na(eval(p)), ], analysisunit == a)
          
          plot <- ggplot(data = dat, aes(x = Year, fill = analysisunit)) +
            geom_bar() +
            scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                               aesthetics = c("color", "fill")) +
            scale_x_continuous(limits = c(min(dat$Year - 1), max(dat$Year + 1))) +
            #scale_y_continuous(limits = c(0, 2600)) +
            labs(y="Frequency of data", x="Year") +
            theme(#legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1),
              #axis.title = element_text(size = 7),
              axis.title = element_blank(),
              axis.text = element_text(size = 7),
              #legend.text = element_text(size = 7),
              legend.position = "none")
          
          saveRDS(plot, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                          gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                          ifelse(stringr::str_detect(i, "NERR"), "ERR_hist_", 
                                                 ifelse(stringr::str_detect(i, "NMS"), "MS_hist_", "AP_hist_")), 
                                          gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', a, perl = TRUE), ".rds")))
        }
        
        dat <- filter(SAV4[ManagedAreaName == i & !is.na(eval(p)), ], analysisunit %in% c("Total seagrass", "Attached algae", "Drift algae"))
        
        plot <- ggplot(data = dat, aes(x = Year, fill = analysisunit)) +
          geom_bar() +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(dat$analysisunit)), 
                             aesthetics = c("color", "fill")) +
          scale_x_continuous(limits = c(min(dat$Year - 1), max(dat$Year + 1))) +
          #scale_y_continuous(limits = c(0, 2600)) +
          #paste0(as_label(BBAP_BB_EDAplots[[1]]$mapping$y))
          labs(y="Frequency of data", x="Year") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                #axis.title = element_text(size = 7),
                #axis.title = element_blank(),
                axis.text = element_text(size = 7),
                #legend.text = element_text(size = 7),
                legend.position = "none",
                legend.title = element_blank())
        
        saveRDS(plot, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                        gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                        ifelse(stringr::str_detect(i, "NERR"), "ERR_hist_SGvMA.rds", 
                                               ifelse(stringr::str_detect(i, "NMS"), "MS_hist_SGvMA.rds", "AP_hist_SGvMA.rds")))))  
        
        #Generate the legend
        plotall <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = eval(p), color = analysisunit)) +
          geom_boxplot() +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                             aesthetics = c("color", "fill")) +
          labs(y = parameters[column == p, name], x = "Year") +
          theme(legend.title = element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1),
                axis.title = element_text(size = 7),
                axis.text = element_text(size = 7),
                legend.text = element_text(size = 7))
        
        legend = gtable::gtable_filter(ggplotGrob(plotall), "guide-box")
        
        saveRDS(legend, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                          gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                          ifelse(stringr::str_detect(i, "NERR"), "ERR_boxplot_specieslegend.rds", 
                                                 ifelse(stringr::str_detect(i, "NMS"), "MS_boxplot_specieslegend.rds", "AP_boxplot_specieslegend.rds")))))
        
        #Create and save the boxplot objects--------------------------------------------------
        for(b in setdiff(unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit]), c("Total seagrass", "Attached algae", "Drift algae"))){
          dat <- filter(SAV4[ManagedAreaName == i & !is.na(eval(p)), ], analysisunit == b)
          
          plot <- ggplot(data = dat, aes(group=Year, x = Year, y = eval(p), color = analysisunit)) +
            geom_boxplot() +
            scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                               aesthetics = c("color", "fill")) +
            scale_x_continuous(limits = c(min(dat$Year - 1), max(dat$Year + 1))) +
            #scale_y_continuous(limits = c(0, 100)) +
            labs(y = parameters[column == p, name], x = "Year") +
            theme(#legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1),
              #axis.title = element_text(size = 7),
              axis.title = element_blank(),
              axis.text = element_text(size = 7),
              #legend.text = element_text(size = 7),
              legend.position = "none")
          
          saveRDS(plot, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                          gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                          ifelse(stringr::str_detect(i, "NERR"), "ERR_boxplot_", 
                                                 ifelse(stringr::str_detect(i, "NMS"), "MS_boxplot_", "AP_boxplot_")), 
                                          gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', b, perl = TRUE), ".rds")))
        }
        
        dat <- filter(SAV4[ManagedAreaName == i & !is.na(eval(p)), ], analysisunit %in% c("Total seagrass", "Attached algae", "Drift algae"))
        
        plot <- ggplot(data = dat, aes(x = as.factor(Year), y = eval(p), color = analysisunit)) +
          geom_boxplot() +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(dat$analysisunit)), 
                             aesthetics = c("color", "fill")) +
          #scale_x_continuous(limits = c(min(dat$Year - 1), max(dat$Year + 1))) +
          #scale_y_continuous(limits = c(0, 100)) +
          labs(y = parameters[column == p, name], x = "Year") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                #axis.title = element_text(size = 7),
                #axis.title = element_blank(),
                axis.text = element_text(size = 7),
                #legend.text = element_text(size = 7),
                legend.position = "none",
                legend.title = element_blank())
        
        saveRDS(plot, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                        gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                        ifelse(stringr::str_detect(i, "NERR"), "ERR_boxplot_SGvMA.rds", 
                                               ifelse(stringr::str_detect(i, "NMS"), "MS_boxplot_SGvMA.rds", "AP_boxplot_SGvMA.rds")))))  
      }
      
      if(str_detect(EDA, "maps")){
      
        #Create map(s) for the managed area-------------------------------------------
        
        fl_i <- st_crop(counties, xmin = corners[ManagedAreaName == i, xmin], xmax = corners[ManagedAreaName == i, xmax], ymin = corners[ManagedAreaName == i, ymin], ymax = corners[ManagedAreaName == i, ymax])
        # fl_i2 <- ggplot() +
        #   geom_sf(data = fl_i, fill = "beige", color = "navajowhite3", lwd = 0.5, inherit.aes = FALSE)
        # fl_i <- fl_i +
        #   annotation_scale(
        #     location = "tl",
        #     bar_cols = c("grey60", "white"),
        #     text_family = "Arial") #+
        #   annotation_north_arrow(
        #     location = "tr", 
        #     which_north = "true",
        #     pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
        #     style = north_arrow_nautical(
        #       fill = c("grey40", "white"),
        #       line_col = "grey20",
        #       text_family = "Arial"
        #     )
        #   )
        
        rcp_i <- subset(rcp, rcp$LONG_NAME == ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                                                     ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), 
                                                            ifelse(str_detect(i, "Fort Clinch|Fort Pickens|Rocky Bayou|St. Andrews"), paste0(i, " State Park Aquatic Preserve"), paste0(i, " Aquatic Preserve")))))
        
        
        #create scalebar and north arrow (https://stackoverflow.com/questions/34183049/plot-circle-with-a-certain-radius-around-point-on-a-map-in-ggplot2)
        if(corners[ManagedAreaName == i, Coast[1]] == "Atlantic"){
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
        
        if(corners[ManagedAreaName == i, Coast[1]] == "Atlantic"){
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
        
        
        # x_sbarpos1 <- ifelse(((abs(min_x) - abs(max_x)) * 0.6) >= lonkm, 
        #                      ((abs(min_x) - abs(max_x)) * 0.6),
        #                      (lonkm) + ((abs(min_x) - abs(max_x)) * 0.1)) #/(lonkm/(max_x - min_x))
        # x_sbarpos2 <- ifelse(((abs(min_x) - abs(max_x)) * 0.2) >= lonkm, 
        #                      ((abs(min_x) - abs(max_x)) * 0.2),
        #                      (lonkm) + ((abs(min_x) - abs(max_x)) * 0.1)) #/(lonkm/(max_x - min_x))
        # y_sbarpos1 <- ifelse(((abs(max_y) - abs(min_y)) * 0.6) >= lonkm, 
        #                      (abs(max_y) - abs(min_y)) * 0.6,
        #                      (lonkm) + ((abs(max_y) - abs(min_y)) * 0.1)) #/(lonkm/(max_y - min_y))
        # y_sbarpos2 <- ifelse(((abs(max_y) - abs(min_y)) * 0.1) >= lonkm, 
        #                      (abs(max_y) - abs(min_y)) * 0.1,
        #                      (lonkm) + ((abs(max_y) - abs(min_y)) * 0.1)) #/(lonkm/(max_y - min_y))
        
        sbar[, `:=` (x = fcase(corners[ManagedAreaName == i, Coast[1]] == "Gulf", x + x_sbarpos2, #(abs(min_x) - abs(max_x)) * ((lonkm/(max_x - min_x)) * x_sbarpos2),
                               corners[ManagedAreaName == i, Coast[1]] == "Panhandle", x + x_sbarpos1, #(abs(min_x) - abs(max_x)) * ((lonkm/(max_x - min_x)) * x_sbarpos1),
                               i %in% c("Banana River", "Indian River-Malabar to Vero Beach", "Indian River-Vero Beach to Ft. Pierce", "Jensen Beach to Jupiter Inlet", 
                                        "Mosquito Lagoon"), x + x_sbarpos2, #(abs(min_x) - abs(max_x)) * ((lonkm/(max_x - min_x)) * x_sbarpos2),
                               corners[ManagedAreaName == i, Coast[1]] == "Atlantic", x + x_sbarpos2), #(abs(min_x) - abs(max_x)) * ((lonkm/(max_x - min_x)) * x_sbarpos2)),
                     y = fcase(corners[ManagedAreaName == i, Coast[1]] == "Gulf", y - y_sbarpos2, #(abs(max_y) - abs(min_y)) * ((lonkm/(max_y - min_y)) * y_sbarpos2),
                               corners[ManagedAreaName == i, Coast[1]] == "Panhandle", y + y_sbarpos2, #(abs(max_y) - abs(min_y)) * ((lonkm/(max_y - min_y)) * y_sbarpos2),
                               i %in% c("Banana River", "Indian River-Malabar to Vero Beach", "Indian River-Vero Beach to Ft. Pierce", "Jensen Beach to Jupiter Inlet", 
                                        "Mosquito Lagoon"), y + y_sbarpos1, #(abs(max_y) - abs(min_y)) * ((lonkm/(max_y - min_y)) * y_sbarpos1),
                               corners[ManagedAreaName == i, Coast[1]] == "Atlantic", y + y_sbarpos1)),#(abs(max_y) - abs(min_y)) * ((lonkm/(max_y - min_y)) * y_sbarpos1))), 
             by = list(row.names(sbar))]

        sbar <- st_as_sf(sbar, coords = c("x", "y"), crs = 4326)
        sbar <- st_combine(sbar)
        sbar <- st_cast(sbar, "LINESTRING")
        sbar <- st_sf(sbar)
        st_geometry(sbar) <- "geometry"
        
        if(corners[ManagedAreaName == i, Coast[1]] == "Atlantic"){
          sbarlab <- data.table(x = st_bbox(sbar)$xmin + (abs(min_x) - abs(max_x)) * 0.15,
                                y = st_bbox(sbar)$ymin + lonkm/3) #min_y)
        } else {
          sbarlab <- data.table(x = st_bbox(sbar)$xmin + lonkm/2,
                                y = st_bbox(sbar)$ymin - (abs(max_y) - abs(min_y)) * 0.1) #min_y)
        }
        
        # sbarlab[, `:=` (x = fcase(corners[ManagedAreaName == i, Coast[1]] == "Gulf", x + x_sbarpos2, #(abs(min_x) - abs(max_x)) * ((lonkm/(max_x - min_x)) * x_sbarpos2),
        #                           corners[ManagedAreaName == i, Coast[1]] == "Panhandle", x + x_sbarpos1, #(abs(min_x) - abs(max_x)) * ((lonkm/(max_x - min_x)) * x_sbarpos1),
        #                           i %in% c("Banana River", "Indian River-Malabar to Vero Beach", "Indian River-Vero Beach to Ft. Pierce", "Jensen Beach to Jupiter Inlet", 
        #                                    "Mosquito Lagoon"), x + x_sbarpos2, #(abs(min_x) - abs(max_x)) * ((lonkm/(max_x - min_x)) * x_sbarpos2),
        #                           corners[ManagedAreaName == i, Coast[1]] == "Atlantic", x + x_sbarpos2), #(abs(min_x) - abs(max_x)) * ((lonkm/(max_x - min_x)) * x_sbarpos2)),
        #                 y = fcase(corners[ManagedAreaName == i, Coast[1]] == "Gulf", y - y_sbarpos2, #(abs(max_y) - abs(min_y)) * ((lonkm/(max_y - min_y)) * y_sbarpos2),
        #                           corners[ManagedAreaName == i, Coast[1]] == "Panhandle", y - y_sbarpos2, #(abs(max_y) - abs(min_y)) * ((lonkm/(max_y - min_y)) * y_sbarpos2),
        #                           i %in% c("Banana River", "Indian River-Malabar to Vero Beach", "Indian River-Vero Beach to Ft. Pierce", "Jensen Beach to Jupiter Inlet", 
        #                                    "Mosquito Lagoon"), y + y_sbarpos1, #(abs(max_y) - abs(min_y)) * ((lonkm/(max_y - min_y)) * y_sbarpos1),
        #                           corners[ManagedAreaName == i, Coast[1]] == "Atlantic", y + y_sbarpos1))] #(abs(max_y) - abs(min_y)) * ((lonkm/(max_y - min_y)) * y_sbarpos1)))]
        # 
        sbarlab <- st_as_sf(sbarlab, coords = c("x", "y"), crs = 4326)
        
        
        if(corners[ManagedAreaName == i, Coast[1]] == "Atlantic"){
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
        
        if(corners[ManagedAreaName == i, Coast[1]] == "Atlantic"){
          narlab <- data.table(x = st_bbox(sbarlab)$xmin,
                               y = st_bbox(narrow)$ymin) #+ (abs(st_bbox(narrow)$ymax) - abs(st_bbox(narrow)$ymin)) / 5)
        
        } else{
          narlab <- data.table(x = st_bbox(narrow)$xmin + (abs(st_bbox(narrow)$xmin) - abs(st_bbox(narrow)$xmax)) / 2,
                               y = st_bbox(sbarlab)$ymin)
          
        }
        # narlab[, `:=` (x = ifelse(corners[ManagedAreaName == i, Coast[1]] == "Gulf", x + (abs(min_x) - abs(max_x)) * 0.05,
        #                           x + (abs(min_x) - abs(max_x)) * 0.91),
        #                y = ifelse(corners[ManagedAreaName == i, Coast[1]] == "Gulf", y + (abs(max_y) - abs(min_y)) * 0.005,
        #                           y + (abs(max_y) - abs(min_y)) * 0.005))]
        narlab <- st_as_sf(narlab, coords = c("x", "y"), crs = 4326)
        
        
        locs_pts_rcp_i <- locs_pts_rcp[rcp_i, , op = st_intersects]
        locs_lns_rcp_i <- locs_lns_rcp[rcp_i, , op = st_intersects]
        
        yadd <- 0
        startyear <- min(SAV4[ManagedAreaName == i & !is.na(eval(p)), Year])
        base <- ggplot() +
          geom_sf(data = rotate_sf(fl_i, ma = i, coast = corners[ManagedAreaName == i, Coast[1]]), fill = "beige", color = "navajowhite3", lwd = 0.5, inherit.aes = FALSE) +
          geom_sf(data = rotate_sf(rcp_i, ma = i, coast = corners[ManagedAreaName == i, Coast[1]]), color = "grey50", fill = "powderblue", alpha = 0.35, lwd = 0.5, inherit.aes = FALSE) +
          geom_sf(data = rotate_sf(sbar, ma = i, coast = corners[ManagedAreaName == i, Coast[1]]), color = "grey50", linewidth = 1.25, inherit.aes = FALSE) +
          geom_sf(data = rotate_sf(narrow, ma = i, coast = corners[ManagedAreaName == i, Coast[1]]), color = "grey50", linewidth = 1, inherit.aes = FALSE) +
          geom_sf_text(data = rotate_sf(sbarlab, ma = i, coast = corners[ManagedAreaName == i, Coast[1]]), label = ifelse(wkm < 20, "3 km", ifelse(wkm < 50, "5 km", "10 km")), hjust = 0.5, angle = 4, color = "grey50", size = 3.5, inherit.aes = FALSE) +
          geom_sf_text(data = rotate_sf(narlab, ma = i, coast = corners[ManagedAreaName == i, Coast[1]]), label = "N", hjust = 0.7, angle = 4, color = "grey50", size = 3.5, inherit.aes = FALSE) +
          # geom_text(data = sbarlab, 
          #           aes(geometry = geometry, stat(X), stat(Y)),
          #           label = "5 km",
          #           stat = StatSfCoordinates,
          #           # fun.geometry = rotate_sf,
          #           size = 10) +
          # annotation_scale(
          #   location = "tl",
          #   bar_cols = c("grey60", "white"),
          #   text_family = "Arial") +
          # annotation_north_arrow(
          #   location = "tl", 
          #   which_north = "true",
          #   pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
          #   style = north_arrow_nautical(
          #     fill = c("grey40", "white"),
          #     line_col = "grey20",
          #     text_family = "Arial"
          #   )
          # ) +
          scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), ProgramName])), 
                             aesthetics = c("color", "fill")) +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))), 
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
        ystart <- ifelse(corners[ManagedAreaName == i, Coast[1]] == "Atlantic", attributes(base$layers[[2]]$data$geometry)$bbox$ymax[[1]], attributes(base$layers[[2]]$data$geometry)$bbox$ymin[[1]])
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
        maxydist <- max(MApolycoords$ydists) + ((max(MApolycoords$ydists)/25) / xyratio) 
        
        if(length(subset(locs_pts_rcp_i, locs_pts_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year == startyear, LocationID]))$LocationID) > 0){
          base <- base +
            geom_sf(data = rotate_sf(subset(locs_pts_rcp_i, locs_pts_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year == startyear, LocationID])),
                                     ma = i, coast = corners[ManagedAreaName == i, Coast[1]]),
                    aes(fill = droplevels(as.factor(ProgramName))), shape = 21, color = "black")
        }
        
        if(length(subset(locs_lns_rcp_i, locs_lns_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year == startyear, LocationID]))$LocationID) > 0){
          base <- base +
            geom_sf(data = rotate_sf(subset(locs_lns_rcp_i, locs_lns_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year == startyear, LocationID])),
                                     ma = i, coast = corners[ManagedAreaName == i, Coast[1]]),
                    aes(color = droplevels(as.factor(ProgramName))), shape = 21)
        }
        
        for(y in sort(unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year != startyear, Year]))){
          base <- base +
            geom_sf(data = rotate_sf(rcp_i, y_add = yadd + maxydist, ma = i, coast = corners[ManagedAreaName == i, Coast[1]]), 
                    color = "grey50", fill = "powderblue", alpha = 0.65, lwd = 0.5, inherit.aes = FALSE) +
            annotate("text", x = xlab, y = xmax_y + yadd + maxydist, label = y, hjust = "left")
          
          if(length(subset(locs_pts_rcp_i, locs_pts_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year == y, LocationID]))$LocationID) > 0){
            base <- base +
              geom_sf(data = rotate_sf(subset(locs_pts_rcp_i, locs_pts_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year == y, LocationID])),
                                       y_add = yadd + maxydist, ma = i, coast = corners[ManagedAreaName == i, Coast[1]]), 
                      aes(fill = droplevels(as.factor(ProgramName))), shape = 21, color = "black")
          }
          
          if(length(subset(locs_lns_rcp_i, locs_lns_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year == startyear, LocationID]))$LocationID) > 0){
            base <- base +
              geom_sf(data = rotate_sf(subset(locs_lns_rcp_i, locs_lns_rcp_i$LocationID %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)) & Year == startyear, LocationID])),
                                       y_add = yadd + maxydist, ma = i, coast = corners[ManagedAreaName == i, Coast[1]]),
                      aes(color = droplevels(as.factor(ProgramName))), shape = 21)
          }
          
          yadd <- yadd + maxydist
          startyear <- startyear + 1
          ystart <- ystart + maxydist
        }        
        
        saveRDS(base, here::here(paste0("SAV/output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                        gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE),
                                        ifelse(stringr::str_detect(i, "NERR"), "ERR_map_bypr.rds", 
                                               ifelse(stringr::str_detect(i, "NMS"), "MS_map_bypr.rds", "AP_map_bypr.rds")))))
        
        #Save image file versions of the maps
        # nlayers <- 0
        # for(k in seq_along(base$layers)){
        #   class_k <- class(base$layers[[k]])[2]
        #   if(class_k == 'Layer'){
        #     nlayers <- nlayers + 1
        #   } else{
        #     next
        #   }
        # }
        
        base <- base +
          theme(legend.position='bottom', 
                legend.justification='left',
                legend.direction='vertical')
        
        plotbuild <- ggplot_build(base)
        hwratio <- (plotbuild$layout$panel_scales_y[[1]]$range$range[2] - plotbuild$layout$panel_scales_y[[1]]$range$range[1]) / (plotbuild$layout$panel_scales_x[[1]]$range$range[2] - plotbuild$layout$panel_scales_x[[1]]$range$range[1])
        pwidth <- 6
        
        ggsave(filename = here::here(paste0("SAV/output/Figures/BB/img/SAV_", parameters[column == p, type], "_", 
                                            gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE),
                                            ifelse(stringr::str_detect(i, "NERR"), "ERR_map_bypr.jpg", 
                                                   ifelse(stringr::str_detect(i, "NMS"), "MS_map_bypr.jpg", "AP_map_bypr.jpg")))), 
               plot = base,
               width = pwidth, #6.1,
               #height = 8 + nlayers - 5,
               height = pwidth * hwratio, #yadd/maxydist,
               units = "in",
               dpi = 300,
               limitsize = FALSE)
      }  
    }
    
    if("none" %in% Analyses){
      if(p == parameters$column[length(parameters$column)] & i == ma_include[length(ma_include)]){
        toc()
      }
      next
    } 
    
    if(i %in% ma_halspp){
      species <- subset(nyears, nyears$ManagedAreaName == i & nyears$nyr >= 5 & analysisunit %in% c("Attached algae", "Drift algae", "Halophila spp.", "Syringodium filiforme", 
                                                                                                    "Halodule wrightii", "Total seagrass", "Total SAV", "Thalassia testudinum", 
                                                                                                    "Ruppia maritima"))$analysisunit
    } else{
      species <- subset(nyears, nyears$ManagedAreaName == i & nyears$nyr >= 5 & analysisunit %in% c("Attached algae", "Drift algae", "Unidentified Halophila", 
                                                                                                    "Halophila johnsonii", "Syringodium filiforme", "Halophila decipiens", 
                                                                                                    "Halodule wrightii", "Halophila engelmannii", "Total seagrass", "Total SAV", 
                                                                                                    "Thalassia testudinum", "Ruppia maritima"))$analysisunit
    }
    
    #Create data.tables to hold model results for managed area i----------------------------------------------------
    lmemodresults <- data.table(managed_area = character(),
                                species = character(),
                                filename = character(),
                                effect = character(),
                                group = character(),
                                term = character(),
                                estimate = numeric(),
                                std.error = numeric(),
                                df = numeric(),
                                statistic = numeric(),
                                p.value = numeric())
    
    
    #In case model doesn't converge on the first try, attempt each model up to 5 times before moving on
    for(j in species){
      
      cat(paste0("\n  Starting species: ", j, "\n"))
      
      if(paste0(p) %in% c("BB_pct", "PC") & ("BB_pct" %in% Analyses | "PC" %in% Analyses)){
        
        formula_j <- as.formula(paste0(p, " ~ relyear"))
        
        set.seed(seed + n)
        if(j %in% setdiff(unique(SAV4$analysisunit_halid), unique(SAV4$analysisunit))){
          model_j <- try(lme(formula_j,
                             random = list(SiteIdentifier = ~relyear),
                             control = list(msMaxIter = 1000, msMaxEval = 1000, sing.tol=1e-20),
                             na.action = na.omit, 
                             data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit_halid == j, ]), 
                         silent = TRUE)
          n <- n + 1
          x <- 0
          
          while(class(model_j) == "try-error" & x < 5){
            if(x %% 25 == 0) print(paste0("    Model failed, starting attempt ", x, " of 5"))
            
            set.seed(seed + n)
            model_j <- try(lme(formula_j,
                               random = list(SiteIdentifier = ~relyear),
                               control = list(msMaxIter = 1000, msMaxEval = 1000, sing.tol=1e-20),
                               na.action = na.omit, 
                               data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit_halid == j, ]),
                           silent = TRUE)
            n <- n + 1
            x <- x + 1
          }
        } else{
          model_j <- try(lme(formula_j,
                             random = list(SiteIdentifier = ~relyear),
                             control = list(msMaxIter = 1000, msMaxEval = 1000, sing.tol=1e-20),
                             na.action = na.omit, 
                             data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == j, ]), 
                         silent = TRUE)
          n <- n + 1
          x <- 0
          
          while(class(model_j) == "try-error" & x < 5){
            if(x %% 25 == 0) print(paste0("    Model failed, starting attempt ", x, " of 5"))
            
            set.seed(seed + n)
            model_j <- try(lme(formula_j,
                               random = list(SiteIdentifier = ~relyear),
                               control = list(msMaxIter = 1000, msMaxEval = 1000, sing.tol=1e-20),
                               na.action = na.omit, 
                               data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == j, ]),
                           silent = TRUE)
            n <- n + 1
            x <- x + 1
          }
        }
        
        
        #Individual model objects are needed for plotting all species together
        # eval(call("<-", as.name(paste0(gsub('\\b(\\pL)\\pL{2,}|.', '\\U\\1', i, perl = TRUE), 
        #                                "_", 
        #                                gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE))), 
        #           model_j))
        
        short_model_name <- gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE)
        
        #Save the model object as .rds
        saveRDS(model_j, paste0("output/models/SAV_", parameters[column == p, type], "_", 
                                           ma_abrev, "_",
                                           short_model_name, 
                                           ".rds"))
        
        print(paste0("  Model object saved: ", 
                     ma_abrev, 
                     "_", 
                     short_model_name))
        
        #record lme model results------------------------------------------------------
        if(class(try(eval(model_j), silent = TRUE)) != "try-error"){
          # append only the successful models to data_directory object
          data_directory[[ma_abrev]][[p]][[short_model_name]] <- model_j
          
          modj_i <- setDT(broom.mixed::tidy(eval(model_j)))
          modj_i[, `:=` (managed_area = i,
                         species = j,
                         filename = paste0("SAV_", parameters[column == p, type], "_", 
                                           ma_abrev, "_",
                                           short_model_name, ".rds"))]
          lmemodresults <- rbind(lmemodresults, modj_i)
          
        } else{
          failedmod <- data.table(model = paste0("SAV_", parameters[column == p, type], "_",
                                                 ma_abrev, "_",
                                                 short_model_name, ".rds"),
                                  error = model_j[1])
          
          failedmods <- rbind(failedmods, failedmod)
          
          modj_i <- data.table(managed_area = i,
                               species = j,
                               filename = paste0("SAV_", parameters[column == p, type], "_", 
                                                 ma_abrev, "_",
                                                 short_model_name, ".rds"),
                               effect = NA,
                               group = NA,
                               term = NA,
                               estimate = NA,
                               std.error = NA,
                               df = NA,
                               statistic = NA,
                               p.value = NA)
          lmemodresults <- rbind(lmemodresults, modj_i)
        }
      }
      
      #Indicator == "BB_all"------------------------------------------------------
      if(paste0(p) == "BB_all" & "BB_all" %in% Analyses) next
      
      #Indicator == "PO"--------------------------------------------------------
      if(paste0(p) == "PO" & "PO" %in% Analyses) next #Temporarily blocking the percent occurrence analyses because the binomial model doesn't seem to fit the data very well. Will probably have to figure something else out.
      
      
      #Indicator == "PA"------------------------------------------------------------
      if(paste0(p) == "PA" & "PA" %in% Analyses) next
    }
    
    #Final results tables and plots--------------------------------------------------------------------
    ## Trend plots
    if(paste0(p) %in% c("BB_pct", "PC") & ("BB_pct" %in% Analyses | "PC" %in% Analyses)){
      #Summarize # points per category
      
      if(i %in% ma_halspp){
        plotdat <- SAV4[ManagedAreaName == i & !is.na(eval(p)), ] %>% group_by(analysisunit, Year, relyear, eval(p)) %>% summarise(npt = n())
      } else{
        plotdat <- SAV4[ManagedAreaName == i & !is.na(eval(p)), ] %>% group_by(analysisunit_halid, Year, relyear, eval(p)) %>% summarise(npt = n())
      }
      setDT(plotdat)
      setnames(plotdat, "eval(p)", "data")
      aucol <- names(plotdat[,1])
      
      # declaring available models
      models <- data_directory[[ma_abrev]][[as.character(p)]]
      
      miny <- c()
      for(v in seq_along(models)){
        miny_v <- try(predict(eval(models[[v]]), level = 0), silent = TRUE)
        if(class(miny_v) == "try-error") next
        miny <- append(miny, min(miny_v))
      }
      miny <- ifelse(floor(min(miny)) < 0, floor(min(miny)), 0)
      
      # Scale x-axis data
      breaks_seq <- seq(from = min(plotdat$relyear),
                        to = max(plotdat$relyear),
                        by = 3)
      labels_seq <- seq(from = min(plotdat$Year),
                        to = max(plotdat$Year),
                        by = 3)
      
      #create base plot of seagrass percent cover data over time for managed area i
      plot_i <- ggplot(data = droplevels(plotdat),
                       aes(x = relyear, y = data)) +
        labs(title = parameters[column == p, name], 
             subtitle = i,
             x = "Year",
             y = parameters[column == p, name],
             color = "Species",
             linetype = "Trend significance (alpha = 0.05)") +
        plot_theme +
        ylim(miny, 100) +
        scale_x_continuous(breaks = breaks_seq, labels = labels_seq) +
        scale_colour_manual(values = spcols)
      
      #create second base plot to feature scatter points and make facetted multi-plots
      plot_i_2 <- ggplot(data = droplevels(plotdat),
                         aes(x = relyear, y = data, fill = npt)) +
        geom_point(shape = 21, alpha = 0.9, color = "grey50") +
        geom_hline(yintercept = 0, color = "grey10", lwd = 0.5) +
        labs(title = parameters[column == p, name], 
             subtitle = i,
             x = "Year",
             y = parameters[column == p, name],
             fill = "Number of\nobservations") +
        plot_theme +
        ylim(miny, 100) +
        scale_fill_continuous_sequential(palette = "YlGnBu") +
        scale_x_continuous(breaks = breaks_seq, labels = labels_seq)
      
      if(length(models) > 0){
        #make sure that no failed models slipped through
        classes <- lapply(models, function(x) class(eval(x)))
        models <- models[classes != "try-error"]
        
        # trendlines single plot (addfits function)
        plot_i <- addfits(models, plot_i, p)
        
        # trendlines multi-plot (addfits_blacktrendlines function)
        aucol <- as.name(names(plot_i_2$data)[1])
        
        # modify_species_labels changes scientific into common if needed
        # also replaces "Unidentified Halophila" with "Halophila, unk."
        plot_i_2 <- addfits_multiplots(models, plot_i_2, p, aucol)
      }
      
      #Save the single plot object as .rds
      saveRDS(plot_i, paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_",
                             paste0(str_sub(ma_abrev, 1, -1), "_trendplot.rds")))
      
      #Save the multi plot object as .rds
      saveRDS(plot_i_2, paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_",
                               paste0(str_sub(ma_abrev, 1, -1), "_multiplot.rds")))
      
      #Save the results table objects as .rds
      saveRDS(lmemodresults, paste0("output/tables/SAV/SAV_", parameters[column == p, type], "_",
                                    paste0(str_sub(ma_abrev, 1, -1), "_lmeresults.rds")))
      
    }
    
    ### Bar Plots ###
    if(paste0(p) == "PA" & "PA" %in% Analyses){
      #Bar chart of proportions by analysisunit
      breaks <- c(seq(min(SAV4[ManagedAreaName == i & !is.na(PA), relyear]),
                      max(SAV4[ManagedAreaName == i & !is.na(PA), relyear]),
                      by = 2))
      yrlist <- sort(unique(SAV4$Year))
      
      labels <- c()
      for(b in breaks){
        labels <- append(labels, yrlist[b + 1])
      }
      
      if(i %in% ma_halspp){
        
        bpdat <- props[ManagedAreaName == i & !is.na(analysisunit) & str_detect(analysisunit, "decipiens|engelmannii|johnsonii|Unidentified|Star|Paddle|Johnson", negate = TRUE), ]
        
        sp_list <- unique(bpdat$analysisunit)
        sp_list <- sp_list[order(match(sp_list, names(spcols)))]
        
        # add color scale, determining if scientific or common names
        sp_labels <- modify_species_labels(sp_list, usenames)
        
        barplot_sp <- ggplot(data = bpdat, aes(x = relyear, y = sp_pct, fill = analysisunit)) +
          geom_col(color = "grey20") +
          scale_x_continuous(breaks = breaks, labels = labels) +
          plot_theme +
          labs(title = parameters[column == p, name], subtitle = i,
               fill = "Species",
               x = "Year",
               y = "Occurrence frequency (%)") +
          scale_color_manual(values = subset(spcols, names(spcols) %in% sp_list),
                             labels = sp_labels,
                             aesthetics = c("color", "fill"))
      } else{
        
        bpdat <- props[ManagedAreaName == i & !is.na(analysisunit) & analysisunit != "Halophila spp.", ]
        
        sp_list <- unique(bpdat$analysisunit)
        sp_list <- sp_list[order(match(sp_list, names(spcols)))]
        
        # add color scale, determining if scientific or common names
        sp_labels <- modify_species_labels(sp_list, usenames)
        
        barplot_sp <- ggplot(data = bpdat, aes(x = relyear, y = sp_pct, fill = analysisunit)) +
          geom_col(color = "grey20") +
          scale_x_continuous(breaks = breaks, labels = labels) +
          plot_theme +
          labs(title = parameters[column == p, name], subtitle = i,
               fill = "Species",
               x = "Year",
               y = "Occurrence frequency (%)") +
          scale_color_manual(values = subset(spcols, names(spcols) %in% sp_list),
                             labels = sp_labels,
                             aesthetics = c("color", "fill"))
      }
      
      saveRDS(barplot_sp, paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_",
                                 ma_abrev, "_barplot_sp.rds"))
    }
    
    print(paste0("  Plot objects and results tables saved: ",
                 ma_abrev,
                 "_",
                 short_model_name))
  }
}

#Save failedmodslist-----------------------------------------------------
saveRDS(failedmods, "output/models/failedmodslist.rds")

#Get rid of eval(p)'s from plot file mappings---------------------------------------
files <- list.files("output/Figures/BB/") #get file list
files <- str_subset(files, ".rds") #exclude non-.RDS files

filesupdated <- list()
for(f in seq_along(files)){
  file_f <- readRDS(paste0("output/Figures/BB/", files[f]))
  if(paste0(as_label(file_f$mapping$y)) == "eval(p)"){
    file_f$mapping$y <- parameters[name %in% file_f$labels$y, column][[1]]
    saveRDS(file_f, paste0("output/Figures/BB/", files[f]))
    rm(file_f)
    filesupdated <- append(filesupdated, files[f])
  } else {
    rm(file_f)
  }
  if(round((f/length(files))*100, 1) %% 10 == 0){
    print(paste0(round((f/length(files))*100), "% done!"))
  }
}

# Export .png plots for each plot type below
plot_types <- c("multiplot","trendplot","barplot")

for(plot_type in plot_types){
  file_subset <- str_subset(files, plot_type)
  
  if(plot_type %in% c("trendplot","multiplot")){
    file_subset <- str_subset(file_subset, "_BBpct_")
  }
  
  w <- 8
  h <- 8
  r <- 200
  
  for(file in file_subset){
    plot <- readRDS(paste0("output/Figures/BB/",file))
    plot <- plot + plot_theme
    
    png(paste0("output/website/images/",plot_type,"s/", str_sub(file, 1, -5),".png"),
        width = w,
        height = h,
        units = "in",
        res = r)
    print(plot)
    print(paste0(str_sub(file, 1, -5), ".png exported"))
    
    dev.off()
  }
}

toc()

wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

#Gets list of all image files in output/Figures and creates zip directory
fig_list <- list.files("output/website/images/", full = FALSE, recursive = TRUE)
setwd("output/website/images/")
zip(zipfile=paste0("../SAVFigures_",usenames), 
    files=fig_list)
setwd(wd)
