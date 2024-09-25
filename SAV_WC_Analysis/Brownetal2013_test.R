# Test plotting method from Brown et al., 2013 (Fig. 7)
# Stephen R. Durham, PhD
# Office of Resilience and Coastal Protection, FDEP
# May 2023
# Last modified: 12/7/2023 (this heading added 9/24/2024)

library(tidyverse)
library(data.table)
library(colorspace)
library(here)
library(patchwork)
#library(future)
library(extrafont)
library(magick)
library(sf)
library(mapview)
mapviewOptions(fgb = FALSE)
library(ggtext)
library(glue)
library(rstudioapi)

wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

SAV4 <- fread("../SAV/output/SAV_DataUsed.txt")
SAV4[, monthdate := floor_date(SampleDate, unit = "months")]

SAV4_sum <- distinct(SAV4[, .(BB_allmn = mean(BB_all), BB_allsd = sd(BB_all)), by = list(ManagedAreaName, SiteIdentifier, monthdate, analysisunit)])

samplocs <- st_read("C:/SEACAR Data/SEACARshapes/SampleLocations9sep2024/vw_SampleLocation_Point.shp")
samplocs <- st_make_valid(samplocs)
samplocs <- st_transform(samplocs, crs = 4326)
samplocs_sav <- subset(samplocs, samplocs$LocationID %in% unique(SAV4$LocationID))

linelocs <- st_read("C:/SEACAR Data/SEACARshapes/SampleLocations9sep2024/vw_SampleLocation_Line.shp")
linelocs <- st_make_valid(linelocs)
linelocs <- st_transform(linelocs, crs = 4326)
linelocs_sav <- subset(linelocs, linelocs$LocationID %in% unique(SAV4$LocationID))

linelocs_sav$RawLineStr <- NULL
samplocs2 <- bind_rows(samplocs_sav, linelocs_sav)
samplocs2$len <- st_length(samplocs2$geometry)
samplocs3 <- st_centroid(samplocs2)
minbuff <- 500
units(minbuff) <- "m"
sampbuffs <- st_buffer(samplocs3, ifelse(samplocs3$len > minbuff, samplocs3$len, minbuff))

mapview(linelocs_sav, col.regions = "dodgerblue") +
  mapview(sampbuffs, col.regions = "firebrick")

# Load DDI exports of water quality/clarity data
watdat_exportdate <- "2024-Sep-19"
watdat_files <- list.files("C:/SEACAR Data/SEACARdata", full.names = TRUE)
files_toload <- watdat_files[which(str_detect(watdat_files, "Combined_WQ_WC_NUT_") & str_detect(watdat_files, watdat_exportdate))]

# disc <- files_toload %>% 
#   map_df(~fread(., sep = "|"))

disc <- data.table()
for(file in files_toload){
  df <- fread(file, sep='|', na.strings = "NULL")
  df$OriginalLatitude <- as.double(df$OriginalLatitude)
  df$OriginalLongitude <- as.double(df$OriginalLongitude)
  disc <- bind_rows(disc, df)
}

disc_sf <- st_as_sf(disc[!is.na(OriginalLongitude) & !is.na(OriginalLatitude), ], coords = c("OriginalLongitude", "OriginalLatitude"), crs = 4326)
disc_sav <- st_join(disc_sf, sampbuffs[, c("LocationID", "geometry")], join = st_intersects, left = T)
st_geometry(disc_sav) <- NULL
setDT(disc_sav)
disc_sav <- unique(disc_sav)
saveRDS(disc_sav, paste0("data/disc_sav_", Sys.Date(), ".rds"))

# disc_sav <- readRDS(here::here("SAV/data/WaterData/disc_sav_2023-05-03.rds"))
disc_sav[ParameterUnits == "Degrees C", ParameterUnits := "C"]
disc_sav[, ParameterName := fcase(ParameterName == "Chlorophyll a, Corrected for Pheophytin", "Chl a corrected",
                                  ParameterName == "Chlorophyll a, Uncorrected for Pheophytin", "Chl a uncorrected",
                                  ParameterName == "Colored Dissolved Organic Matter", "CDOM",
                                  ParameterName == "Dissolved Oxygen", "Dissolved oxygen",
                                  ParameterName == "Light Extinction Coefficient", "Light extinction",
                                  ParameterName == "Salinity", "Salinity",
                                  ParameterName == "Secchi Depth", "Secchi depth",
                                  ParameterName == "Total Suspended Solids", "TSS",
                                  ParameterName == "Turbidity", "Turbidity",
                                  ParameterName == "Water Temperature", "Temperature"), by = ParameterName]
disc_sav <- disc_sav[!is.na(ParameterName)]

samplocs3$Latitude_D[which(is.na(samplocs3$Latitude_D))] <- lapply(samplocs3$geometry[which(is.na(samplocs3$Latitude_D))], function(x) x[2])
samplocs3$Longitude_[which(is.na(samplocs3$Longitude_))] <- lapply(samplocs3$geometry[which(is.na(samplocs3$Longitude_))], function(x) x[1])

SAV4 <- merge(SAV4, samplocs3[, c("LocationID", "Latitude_D", "Longitude_")], by = "LocationID", all.x = TRUE)
SAV4[, MA := ManagedAreaName]
saveRDS(SAV4, paste0("data/SAV4_", Sys.Date(), ".rds"))
# SAV4 <- readRDS("SAV/data/WaterData/SAV4_2024-09-25.rds")

for(m in unique(SAV4$MA)){
  savdat_m <- SAV4[MA == m & !is.na(BB_all), ]
  if(nrow(savdat_m) == 0) next
  
  for(i in unique(savdat_m$analysisunit)){
    savdat_mi <- savdat_m[analysisunit == i, .(mean = mean(BB_all), sd = sd(BB_all)), by = Year]
    if(nrow(savdat_mi) == 0) next
    if(i == "No grass in quadrat") next
    
    au_i <- ifelse(i == "Halophila spp.", "<i>Halophila</i> spp.", 
                   ifelse(i %in% c("Drift algae", "Total SAV", "Total seagrass", "Attached algae"), i, glue("<i>{i}</i>")))
    
    for(w in disc_sav[ManagedAreaName == m, unique(ParameterName)]){
      if(is.null(w)) next
      if(is.na(w)) next
      
      watdat_mw <- disc_sav[ManagedAreaName == m & ParameterName == w, .(mean = mean(ResultValue), sd = sd(ResultValue)), by = Year]
      axis1max <- ifelse(plyr::round_any(max(savdat_mi[, mean + sd]), 1, ceiling) > 5, plyr::round_any(max(savdat_mi[, mean + sd]), 1, ceiling), 5) 
      axis1min <- ifelse(plyr::round_any(min(savdat_mi[, mean - sd]), 1, floor) < 0, plyr::round_any(min(savdat_mi[, mean - sd]), 1, floor), 0)
      axis2scale <- max(watdat_mw$mean) / 5
      axis2col <- "dodgerblue3"
      axis2units <- disc_sav[ManagedAreaName == m & ParameterName == w, unique(ParameterUnits)]
      savdat_mi_overlap <- data.table(Year = setdiff(unique(watdat_mw$Year), unique(savdat_mi$Year)))
      savdat_mi <- bind_rows(savdat_mi, savdat_mi_overlap)
      
      min_year <- min(savdat_mi$Year)
      max_year <- max(savdat_mi$Year)
      nyr <- max_year - min_year
      nbrks <- ifelse(plyr::round_any(nyr, 5, f = ceiling) / 5 <= 3, 5, plyr::round_any(nyr, 5, f = ceiling) / 5)
      
      plot_miw <- ggplot() +
        geom_hline(yintercept = 0, color = "grey25", lwd = 0.5) +
        geom_col(data = savdat_mi, aes(x = Year, y = mean), fill = "grey80", color = "grey30") +
        geom_errorbar(data = savdat_mi, aes(x = Year, ymin = mean - sd, ymax = mean + sd), width = 0.25, lwd = 0.5, color = "grey25") +
        geom_line(data = watdat_mw, aes(x = Year, y = mean / axis2scale), lty = "solid", color = axis2col, lwd = 0.75) +
        geom_point(data = watdat_mw, aes(x = Year, y = mean / axis2scale), shape = 21, color = "dodgerblue4", fill = axis2col, size = 1.5) +
        scale_y_continuous(limits = c(axis1min, axis1max), breaks = c(0:5), sec.axis = sec_axis(name = glue("Mean {w} ({axis2units})"), #glue("<span class = color:{axis2col}>Mean {w}</span>") - https://github.com/wilkelab/ggtext/blob/master/man/figures/README-unnamed-chunk-4-1.png
                                                                                                trans = ~. * axis2scale)) +
        scale_x_continuous(n.breaks = nbrks, limits = c(min(savdat_mi$Year), max(savdat_mi$Year))) +
        theme_classic() +
        labs(x = "Year", y = paste0("Mean BB Score"), title = glue("{m}<br>{au_i}")) +
        theme(plot.title = element_markdown(),
              axis.text.y.right = element_markdown(color = axis2col),
              axis.title.y.right = element_markdown(color = axis2col))
      
      ggsave(filename = paste0("output/", str_replace_all(m, " ", ""), "_", str_replace_all(i, " ", ""), "_v_", str_replace_all(w, " ", ""), "_", Sys.Date(), ".png"),
             plot = plot_miw,
             height = 3,
             width = 5,
             units = "in",
             dpi = 200)
      
      print(paste0("Saving plot: ", str_replace_all(m, " ", ""), "_", str_replace_all(i, " ", ""), "_v_", str_replace_all(w, " ", "")))
    
    }
  }
}

