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
library(openxlsx)
library(kableExtra)

# Trim data to a minimum year?
minimumYear <- 2014

wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

# SEACAR plot standards
plot_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(family="Arial"),
        plot.title=element_text(hjust=0.5, size=12, color="#314963"),
        plot.subtitle=element_text(hjust=0.5, size=10, color="#314963"),
        legend.title=element_text(size=10),
        legend.text.align = 0,
        axis.title.x = element_text(size=10, margin = margin(t = 5, r = 0,
                                                             b = 10, l = 0)),
        axis.title.y = element_text(size=10, margin = margin(t = 0, r = 10,
                                                             b = 0, l = 0)),
        axis.text=element_text(size=10),
        axis.text.x=element_text(angle = -45, hjust = 0))

# Set up SAV Species palette
spcollist <- c(
  "#005396",
  "#005396",
  "#0088B1",
  "#00ADAE",
  "#65CCB3",
  "#AEE4C1",
  "#FDEBA8",
  "#F8CD6D",
  "#F5A800",
  "#F17B00",
  "#900667",
  "#000099"
)
spp <- c("Halophila spp.","Unidentified Halophila","Halophila johnsonii","Syringodium filiforme","Halophila decipiens","Halodule wrightii",
         "Halophila engelmannii","Thalassia testudinum","Ruppia maritima","Attached algae", "Total SAV", "Total seagrass")
spcols <- setNames(spcollist, spp)

# Bring in list of MAs (for abbreviations)
MA_All <- fread("../MA Report Generation/data/ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE, na.strings = "")

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

# mapview(linelocs_sav, col.regions = "dodgerblue") +
#   mapview(sampbuffs, col.regions = "firebrick")

# Load DDI exports of water quality/clarity data
watdat_exportdate <- "2024-Oct-03"
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

# disc_sav <- readRDS("data/disc_sav_2024-10-07.rds")
disc_sav[ParameterUnits == "Degrees C", ParameterUnits := "C"]
# Rename parameters and determine which to include
# Remove DO, add TN ; Combine Chla Corrected and Uncorrected - cpclark 10/01/2024
# TN and TP together on same plot, add DO & DOS, fix bar widths - SEACAR Team 10/10/2024
disc_sav[, ParameterName := fcase(ParameterName == "Chlorophyll a, Corrected for Pheophytin", "Chl a corr",
                                  ParameterName == "Chlorophyll a, Uncorrected for Pheophytin", "Chl a uncorr",
                                  ParameterName == "Colored Dissolved Organic Matter", "CDOM",
                                  ParameterName == "Dissolved Oxygen", "Dissolved Oxygen",
                                  ParameterName == "Dissolved Oxygen Saturation", "Dissolved Oxygen Saturation",
                                  ParameterName == "Total Nitrogen", "Total Nitrogen",
                                  ParameterName == "Total Phosphorus", "Total Phosphorus",
                                  ParameterName == "Light Extinction Coefficient", "Light extinction",
                                  ParameterName == "Salinity", "Salinity",
                                  ParameterName == "Secchi Depth", "Secchi depth",
                                  ParameterName == "Total Suspended Solids", "TSS",
                                  ParameterName == "Turbidity", "Turbidity",
                                  ParameterName == "Water Temperature", "Temperature"), by = ParameterName]
disc_sav <- disc_sav[!is.na(ParameterName)]
disc_sav <- disc_sav[Include==1, ]
disc_sav$YearMonthDec <- disc_sav$Year + ((disc_sav$Month-0.5) / 12)

# Trim disc_sav to growing months
disc_sav_trimmed <- disc_sav[Month >= 4 & Month <=10, ]

samplocs3$Latitude_D[which(is.na(samplocs3$Latitude_D))] <- lapply(samplocs3$geometry[which(is.na(samplocs3$Latitude_D))], function(x) x[2])
samplocs3$Longitude_[which(is.na(samplocs3$Longitude_))] <- lapply(samplocs3$geometry[which(is.na(samplocs3$Longitude_))], function(x) x[1])

SAV4 <- merge(SAV4, samplocs3[, c("LocationID", "Latitude_D", "Longitude_")], by = "LocationID", all.x = TRUE)
SAV4[, MA := ManagedAreaName]
saveRDS(SAV4, paste0("data/SAV4_", Sys.Date(), ".rds"))
# SAV4 <- readRDS("data/SAV4_2024-10-07.rds")

# Included parameters, chla is combined and treated separately
# TN and TP also combined
included_params <- c("Chl a",
                     "CDOM",
                     "Dissolved Oxygen",
                     "Dissolved Oxygen Saturation",
                     "Salinity",
                     "Secchi depth",
                     "Total Nitrogen",
                     "TSS",
                     "Turbidity",
                     "Temperature")

# Generate mean and sd for all ManagedArea and Species combinations
savdat <- SAV4[!is.na(BB_all), .(mean = mean(BB_all), sd = sd(BB_all)),
               by = c("ManagedAreaName", "Year", "analysisunit")]
# Exclude certain species
# excluded_sp <- c("No grass in quadrat","Total SAV","Total seagrass", "Drift algae")
excluded_sp <- c("No grass in quadrat", "Drift algae")
savdat <- savdat[!analysisunit %in% excluded_sp, ]

# Parameter colors
bar_color <- "#005396"
mean_col <- "black"
minmax_col <- "firebrick"
# Second parameter color
p2MeanCol <- "grey50" #8FD0EC

# Data directory to store results tables
data_directory <- list()

for(m in unique(SAV4$MA)){
# for(m in "Estero Bay Aquatic Preserve"){
  ma_abrev <- MA_All[ManagedAreaName==m, Abbreviation]
  savdat_m <- savdat[ManagedAreaName == m, ]
  if(nrow(savdat_m) == 0) next
  
  # Set height for plots if there is only 1 row of species
  if(length(unique(savdat_m$analysisunit))>3){
    h <- 7
  } else {
    h <- 4
  }
  
  for(p in included_params){
    # Combine chl a corrected and uncorrected
    if(p %in% c("Chl a","Total Nitrogen")){
      if(p=="Chl a"){
        params <- c("Chl a corr", "Chl a uncorr")
        axis2units <- disc_sav[ManagedAreaName == m & ParameterName == "Chl a corr", unique(ParameterUnits)]
        title <- "Chl a Corrected and Uncorrected"
        filename_param <- "Chla"
      } else if(p=="Total Nitrogen"){
        params <- c("Total Nitrogen", "Total Phosphorus")
        axis2units <- disc_sav[ManagedAreaName == m & ParameterName == "Total Nitrogen", unique(ParameterUnits)]
        title <- "Total Nitrogen and Total Phosphorus"
        filename_param <- "TN_TP"
      }
      
      # Set color associations
      vals <- c(mean_col, p2MeanCol)
      names(vals) <- params
      
      watdat_mw <- disc_sav_trimmed[
        ManagedAreaName == m & ParameterName %in% params, .(
          mean = mean(ResultValue),
          median = median(ResultValue),
          sd = sd(ResultValue),
          min = min(ResultValue),
          max = max(ResultValue)), 
        by = c("Year", "ParameterName")
      ]
      
      if(length(unique(watdat_mw$Year))<10) next
      
      axis2scale <- max(watdat_mw$mean) / 5
      
      min_year <- min(savdat_m$Year)
      max_year <- max(savdat_m$Year)
      nyr <- max_year - min_year
      nbrks <- ifelse(plyr::round_any(nyr, 5, f = ceiling) / 5 <= 3, 5, plyr::round_any(nyr, 5, f = ceiling) / 5)
      
      watdat_mw <- watdat_mw[Year >= min_year]
      
      # Store tables in data directory
      data_directory[["watdat_mw"]][[m]][[p]] <- watdat_mw
      data_directory[["savdat_m"]][[m]][[p]] <- savdat_m
      
      plot_miw <- ggplot() +
        geom_hline(yintercept = 0, color = "grey25", lwd = 0.5) +
        geom_bar(data = savdat_m, aes(x = Year, y = mean), stat = "identity", position = "dodge", fill=bar_color, alpha = 0.5) +
        geom_line(data = watdat_mw, aes(x = Year, y = mean / axis2scale, color = ParameterName), lty = "solid", lwd = 1) +
        # geom_point(data = watdat_mw, aes(x = Year, y = mean / axis2scale, color = ParameterName), shape = 21, fill = mean_col, size = 1.5) +
        scale_y_continuous(sec.axis = sec_axis(name = glue("Annual Mean {title} ({axis2units})"),
                                               trans = ~. * axis2scale)) +
        plot_theme +
        labs(x = "Year", y = "Annual Mean Braun Blanquet Score", 
             title = glue("{m}<br>{title} ({axis2units})"),
             subtitle = "Limited to Growing Season (Apr - Oct)",
             fill = "Species",
             color = "Parameter") +
        theme(plot.title = element_markdown(),
              axis.text.y.right = element_markdown(color = mean_col),
              axis.title.y.right = element_markdown(color = mean_col)) +
        scale_x_continuous(n.breaks = nbrks, limits = c(min_year, max_year)) +
        scale_color_manual(
          name = "Parameter",
          values = vals) +
        scale_fill_manual(
          name = "Species",
          values = spcols
        ) +
        facet_wrap(~analysisunit)
      
      # Save plot
      ggsave(filename = paste0("output/", ma_abrev, "_", filename_param, "_", Sys.Date(), ".png"),
             plot = plot_miw,
             height = h,
             width = 10,
             units = "in",
             dpi = 200)
      print(paste0("Saving plot: ", paste0("output/", ma_abrev, "_", filename_param, "_", Sys.Date(), ".png")))
    } else {
      
      watdat_mw <- disc_sav_trimmed[
        ManagedAreaName == m & ParameterName == p, .(
          mean = mean(ResultValue),
          median = median(ResultValue),
          sd = sd(ResultValue),
          min = min(ResultValue),
          max = max(ResultValue)
        ), by = c("Year", "ParameterName")]
      
      if(length(unique(watdat_mw$Year))<10) next
      
      axis2scale <- max(watdat_mw$mean) / 5
      axis2units <- disc_sav_trimmed[ManagedAreaName == m & ParameterName == p, unique(ParameterUnits)]
      
      min_year <- min(savdat_m$Year)
      max_year <- max(savdat_m$Year)
      nyr <- max_year - min_year
      nbrks <- ifelse(plyr::round_any(nyr, 5, f = ceiling) / 5 <= 3, 5, plyr::round_any(nyr, 5, f = ceiling) / 5)
      
      watdat_mw <- watdat_mw[Year >= min_year]
      
      # Store tables in data directory
      data_directory[["watdat_mw"]][[m]][[p]] <- watdat_mw
      data_directory[["savdat_m"]][[m]][[p]] <- savdat_m
      
      # Add min and max for Temp and Sal
      if(p %in% c("Temperature", "Salinity")){
        plot_miw <- ggplot() +
          geom_hline(yintercept = 0, color = "grey25", lwd = 0.5) +
          geom_bar(data = savdat_m, aes(x = Year, y = mean), stat = "identity", position = "dodge", fill=bar_color, alpha = 0.5) +
          geom_line(data = watdat_mw, aes(x = Year, y = mean / axis2scale, lty = "mean", color = "mean"), lwd = 1) +
          geom_line(data = watdat_mw, aes(x = Year, y = min / axis2scale, lty = "min/max", color = "min/max"), lwd = 0.75) +
          geom_line(data = watdat_mw, aes(x = Year, y = max / axis2scale, lty = "min/max", color = "min/max"), lwd = 0.75) +
          # geom_point(data = watdat_mw, aes(x = Year, y = mean / axis2scale), shape = 21, color = mean_col, fill = mean_col, size = 1.5) +
          scale_y_continuous(sec.axis = sec_axis(name = glue("Annual Mean {p} ({axis2units})"),
                                                 trans = ~. * axis2scale)) +
          plot_theme +
          theme(plot.title = element_markdown(),
                axis.text.y.right = element_markdown(color = mean_col),
                axis.title.y.right = element_markdown(color = mean_col)) +
          scale_x_continuous(n.breaks = nbrks, limits = c(min_year, max_year)) +
          scale_linetype_manual(values = c("mean" = "solid", "min/max" = "dashed")) +
          scale_color_manual(values = c("mean" = mean_col, "min/max" = minmax_col)) +
          labs(x = "Year", 
               y = "Annual Mean Braun Blanquet Score", 
               title = glue("{m}<br>{p} ({axis2units})"),
               subtitle = "Limited to Growing Season (Apr - Oct)",
               fill = "Species",
               linetype = "Value type",
               color = "Value type") +
          facet_wrap(~analysisunit)
      } else {
        plot_miw <- ggplot() +
          geom_hline(yintercept = 0, color = "grey25", lwd = 0.5) +
          geom_bar(data = savdat_m, aes(x = Year, y = mean), stat = "identity", position = "dodge", fill=bar_color, alpha = 0.5) +
          geom_line(data = watdat_mw, aes(x = Year, y = mean / axis2scale), lty = "solid", color = mean_col, lwd = 1) +
          # geom_point(data = watdat_mw, aes(x = Year, y = mean / axis2scale), shape = 21, color = mean_col, fill = mean_col, size = 1.5) +
          scale_y_continuous(sec.axis = sec_axis(name = glue("Annual Mean {p} ({axis2units})"),
                                                 trans = ~. * axis2scale)) +
          plot_theme +
          labs(x = "Year", 
               y = "Annual Mean Braun Blanquet Score", 
               title = glue("{m}<br>{p} ({axis2units})"),
               subtitle = "Limited to Growing Season (Apr - Oct)",
               fill = "Species",
               color = "Parameter") +
          theme(plot.title = element_markdown(),
                axis.text.y.right = element_markdown(color = mean_col),
                axis.title.y.right = element_markdown(color = mean_col)) +
          scale_x_continuous(n.breaks = nbrks, limits = c(min_year, max_year)) +
          facet_wrap(~analysisunit)        
      }
      # Save plot
      ggsave(filename = paste0("output/", ma_abrev, "_", str_replace_all(p, " ", ""), "_", Sys.Date(), ".png"),
             plot = plot_miw,
             height = h,
             width = 10,
             units = "in",
             dpi = 200)
      print(paste0("Saving plot: ", paste0("output/", ma_abrev, "_", str_replace_all(p, " ", ""), "_", Sys.Date(), ".png")))
    }
  }
}

##### Post-processing for data export -----
# Record program information for WQ and SAV
program_data_wq <- disc_sav_trimmed %>%
  group_by(ManagedAreaName, ParameterName, ProgramID, ProgramName) %>%
  summarise(YearMin = min(Year),
            YearMax = max(Year),
            N_Data = n()) %>% ungroup()
program_data_sav <- SAV4 %>%
  group_by(ManagedAreaName, ParameterName, ProgramID, ProgramName) %>%
  summarise(YearMin = min(Year),
            YearMax = max(Year),
            N_Data = n()) %>% ungroup()

wq_results <- data.table()
sav_results <- data.table()
for(ma in names(data_directory[["watdat_mw"]])){
  wq <- bind_rows(data_directory[["watdat_mw"]][[ma]])
  wq$ManagedAreaName <- ma
  wq_results <- bind_rows(wq_results, wq)
  
  sav <- bind_rows(data_directory[["savdat_m"]][[ma]])
  sav$ManagedAreaName <- ma
  sav_results <- bind_rows(sav_results, sav)
}
# Sort and arrange columns for both SAV and WQ
wq_results <- wq_results %>% 
  arrange(ManagedAreaName, ParameterName, Year) %>%
  select(ManagedAreaName, ParameterName, Year, mean, median, min, max, sd)

sav_results <- sav_results %>% distinct() %>%
  arrange(ManagedAreaName, Year, analysisunit) %>%
  select(ManagedAreaName, analysisunit, Year, mean, sd) %>%
  rename("Species" = analysisunit)
# create workbook variable to store data results in separate sheets
wb <- createWorkbook()
addWorksheet(wb, "WQ")
addWorksheet(wb, "SAV")
addWorksheet(wb, "WQ_ProgramInfo")
addWorksheet(wb, "SAV_ProgramInfo")
writeDataTable(wb, "WQ", wq_results, tableStyle = "TableStyleLight16")
writeDataTable(wb, "SAV", sav_results, tableStyle = "TableStyleLight18")
writeDataTable(wb, "WQ_ProgramInfo", program_data_wq, tableStyle = "TableStyleLight16")
writeDataTable(wb, "SAV_ProgramInfo", program_data_sav, tableStyle = "TableStyleLight18")
# Save excel file
saveWorkbook(wb, file = paste0("output/tables/SAV_WC_", gsub("_","-",Sys.Date()), ".xlsx"))

#Gets list of all image files and creates zip
fig_list <- list.files("output", pattern=".png", full=FALSE)
setwd("output")
zip("SAV_WC_Figures", files=fig_list)
setwd(wd)

#Renders SAV_WC_ReportTemplate.Rmd and writes the report to pdf
file_name <- paste0("SAV_WC_Report_", gsub("_","-",Sys.Date()))
rmarkdown::render(input = "SAV_WC_ReportTemplate.Rmd", 
                  output_format = "pdf_document",
                  output_file = paste0(file_name, ".pdf"),
                  clean=TRUE)

#Removes unwanted files created in the rendering process
unlink(paste0(file_name, ".md"))
unlink(paste0(file_name, ".log"))
unlink(paste0(file_name, "_files"), recursive=TRUE)
