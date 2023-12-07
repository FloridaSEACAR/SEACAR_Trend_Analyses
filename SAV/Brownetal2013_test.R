
#Test plotting method from Brown et al. (Fig. 7)

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

SAV4 <- fread(here::here("SAV/output/data/SAV_Used.txt"))
SAV4[, monthdate := floor_date(SampleDate, unit = "months")]

SAV4_sum <- distinct(SAV4[, .(BB_allmn = mean(BB_all), BB_allsd = sd(BB_all)), by = list(ManagedAreaName, SiteIdentifier, monthdate, analysisunit)])

samplocs <- st_read(here::here("SAV/mapping/SampleLocations26jan2023/seacar_dbo_vw_SampleLocation_Point.shp"))
samplocs <- st_make_valid(samplocs)
samplocs <- st_transform(samplocs, crs = 4326)
samplocs_sav <- subset(samplocs, samplocs$LocationID %in% unique(SAV4$LocationID))

linelocs <- st_read(here::here("SAV/mapping/SampleLocations26jan2023/seacar_dbo_vw_SampleLocation_Line.shp"))
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
watdat_exportdate <- "2022-Dec-01"
watdat_files <- list.files(here::here("SAV/data/WaterData/"), full.names = TRUE)
files_toload <- watdat_files[which(str_detect(watdat_files, "Combined_WQ_WC_NUT_") & str_detect(watdat_files, watdat_exportdate))]

disc <- files_toload %>% 
  map_df(~fread(., sep = "|"))

disc_sf <- st_as_sf(disc[!is.na(OriginalLongitude) & !is.na(OriginalLatitude), ], coords = c("OriginalLongitude", "OriginalLatitude"), crs = 4326)
disc_sav <- st_join(disc_sf, sampbuffs[, c("LocationID", "geometry")], join = st_intersects, left = T)
st_geometry(disc_sav) <- NULL
setDT(disc_sav)
disc_sav <- unique(disc_sav)
saveRDS(disc_sav, here::here(paste0("SAV/data/WaterData/disc_sav_", Sys.Date(), ".rds")))

disc_sav <- readRDS(here::here("SAV/data/WaterData/disc_sav_2023-05-03.rds"))
disc_sav[ParameterUnits == "Degrees C", ParameterUnits := "C"]
disc_sav[, ParameterName := fcase(ParameterName == "Chlorophyll a corrected for pheophytin", "Chl a corrected",
                                  ParameterName == "Chlorophyll a uncorrected for pheophytin", "Chl a uncorrected",
                                  ParameterName == "Colored dissolved organic matter, CDOM", "CDOM",
                                  ParameterName == "Dissolved Oxygen", "Dissolved oxygen",
                                  ParameterName == "Light Extinction Coefficient", "Light extinction",
                                  ParameterName == "Salinity", "Salinity",
                                  ParameterName == "Secchi Depth", "Secchi depth",
                                  ParameterName == "Total Suspended Solids, TSS", "TSS",
                                  ParameterName == "Turbidity", "Turbidity",
                                  ParameterName == "Water Temperature", "Temperature"), by = ParameterName]

samplocs3$Latitude_D[which(is.na(samplocs3$Latitude_D))] <- lapply(samplocs3$geometry[which(is.na(samplocs3$Latitude_D))], function(x) x[2])
samplocs3$Longitude_[which(is.na(samplocs3$Longitude_))] <- lapply(samplocs3$geometry[which(is.na(samplocs3$Longitude_))], function(x) x[1])

SAV4 <- merge(SAV4, samplocs3[, c("LocationID", "Latitude_D", "Longitude_")], by = "LocationID", all.x = TRUE)
SAV4[str_detect(ManagedAreaName, "NMS|NERR", negate = TRUE), MA := paste0(ManagedAreaName, " Aquatic Preserve")]
SAV4[str_detect(ManagedAreaName, "NMS"), MA := str_replace(ManagedAreaName, "NMS", "National Marine Sanctuary")]
SAV4[str_detect(ManagedAreaName, "NERR"), MA := str_replace(ManagedAreaName, "NERR", "National Estuarine Research Reserve")]
SAV4[str_detect(ManagedAreaName, "St. Andrews"), MA := "St. Andrews State Park Aquatic Preserve"]
SAV4[str_detect(ManagedAreaName, "Fort Pickens"), MA := "Fort Pickens State Park Aquatic Preserve"]

for(m in unique(SAV4$MA)){
  savdat_m <- SAV4[MA == m & !is.na(BB_all), ]
  if(nrow(savdat_m) == 0) next
  
  for(i in unique(savdat_m$analysisunit)){
    savdat_mi <- savdat_m[analysisunit == i, .(mean = mean(BB_all), sd = sd(BB_all)), by = Year]
    if(nrow(savdat_mi) == 0) next
    
    au_i <- ifelse(i == "Halophila spp.", "<i>Halophila</i> spp.", 
                   ifelse(i %in% c("Drift algae", "Total SAV", "Total seagrass", "Attached algae"), i, glue("<i>{i}</i>")))
    
    for(w in disc_sav[ManagedAreaName == m, unique(ParameterName)]){
      if(is.null(w)) next
      
      watdat_mw <- disc_sav[ManagedAreaName == m & ParameterName == w, .(mean = mean(ResultValue), sd = sd(ResultValue)), by = Year]
      axis1max <- ifelse(plyr::round_any(max(savdat_mi[, mean + sd]), 1, ceiling) > 5, plyr::round_any(max(savdat_mi[, mean + sd]), 1, ceiling), 5) 
      axis1min <- ifelse(plyr::round_any(min(savdat_mi[, mean - sd]), 1, floor) < 0, plyr::round_any(min(savdat_mi[, mean - sd]), 1, floor), 0)
      axis2scale <- max(watdat_mw$mean) / 5
      axis2col <- "dodgerblue3"
      axis2units <- disc_sav[ManagedAreaName == m & ParameterName == w, unique(ParameterUnits)]
      nyr <- max(c(range(watdat_mw$Year), range(savdat_mi$Year))) - min(c(range(watdat_mw$Year), range(savdat_mi$Year)))
      nbrks <- ifelse(plyr::round_any(nyr, 5, f = ceiling) / 5 <= 3, 5, plyr::round_any(nyr, 5, f = ceiling) / 5)
      savdat_mi_overlap <- data.table(Year = setdiff(unique(watdat_mw$Year), unique(savdat_mi$Year)))
      savdat_mi <- bind_rows(savdat_mi, savdat_mi_overlap)
      
      plot_miw <- ggplot() +
        geom_hline(yintercept = 0, color = "grey25", lwd = 0.5) +
        geom_col(data = savdat_mi, aes(x = Year, y = mean), fill = "grey80", color = "grey30") +
        geom_errorbar(data = savdat_mi, aes(x = Year, ymin = mean - sd, ymax = mean + sd), width = 0.25, lwd = 0.5, color = "grey25") +
        geom_line(data = watdat_mw, aes(x = Year, y = mean / axis2scale), lty = "solid", color = axis2col, lwd = 0.75) +
        geom_point(data = watdat_mw, aes(x = Year, y = mean / axis2scale), shape = 21, color = "dodgerblue4", fill = axis2col, size = 1.5) +
        scale_y_continuous(limits = c(axis1min, axis1max), breaks = c(0:5), sec.axis = sec_axis(name = glue("Mean {w} ({axis2units})"), #glue("<span class = color:{axis2col}>Mean {w}</span>") - https://github.com/wilkelab/ggtext/blob/master/man/figures/README-unnamed-chunk-4-1.png
                                                                                                trans = ~. * axis2scale)) +
        scale_x_continuous(n.breaks = nbrks) +
        theme_classic() +
        labs(x = "Year", y = paste0("Mean BB Score"), title = glue("{m}<br>{au_i}")) +
        theme(plot.title = element_markdown(),
              axis.text.y.right = element_markdown(color = axis2col),
              axis.title.y.right = element_markdown(color = axis2col))
      
      ggsave(filename = here::here(paste0("SAV/output/Figures/Brownetal2013_test/", str_replace_all(m, " ", ""), "_", str_replace_all(i, " ", ""), "_v_", str_replace_all(w, " ", ""), "_", Sys.Date(), ".png")),
             plot = plot_miw,
             height = 3,
             width = 5,
             units = "in",
             dpi = 200)
    
    }
  }
}

