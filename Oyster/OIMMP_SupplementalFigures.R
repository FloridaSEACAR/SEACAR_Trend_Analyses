# Workflow for plotting indicator uncertainties by OIMMP region
library(stringr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(leaflet)
library(htmltools)
library(sf)

output_path <- "output/OIMMP/"

oyster <- fread(list.files(seacar_data_location, full=T, pattern="OYSTER"))
exportDate <- max(format(unique(oyster$ExportVersion), "%m/%d/%Y"))

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
    font-family: Arial, Helvetica, sans-serif;
    clear: none;
  }"))

files <- list.files("C:/Users/Hill_T/Desktop/SEACAR GitHub/MonitoringPlanningTools/Oyster_Sample_Size_Site_Selection/Updated_Model_Workflow/data/OIMMP", pattern = "shiny_store", full=T)

loc_files <- function(oimmp, p){stringr::str_subset(files, paste0(gsub(" ", "_", oimmp), "_", gsub(" ", "_", p)))}

for(oimmp in unique(oyster$OIMMP)){
  cat("-", oimmp, "\n")
  for(p in c("Density", "Shell Height", "Percent Live")){
    cat("--", p, "\n")  
    shiny_store <- readRDS(loc_files(oimmp, p))
    # Ensure standardized naming convention ##
    names(shiny_store) <- gsub("oyster.oimmp", "oyster.managed_area", names(shiny_store))
    names(shiny_store) <- gsub("Oyster_Beds_in_OIMMP", "Oyster_Beds_in_ManagedArea", names(shiny_store))
    names(shiny_store) <- gsub("oimmp", "managed_area", names(shiny_store))
    ##########################################
    ###### Sample size + site number plot -----
    k = ifelse(length(unique(shiny_store$sd_by_sample_size$sample_size)) <= 7, 1, 5)
    samplesize_plot <- shiny_store$sd_by_sample_size %>%
      ggplot(aes(x = sample_size, y = sd)) + geom_point() +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = k), se = FALSE) +
      labs(y = "Standard deviation", 
           x = "Sample size", 
           title = "Sample size") + 
      SEACAR::SEACAR_plot_theme()
    
    samplenumber_plot <- shiny_store$sd_by_number_of_sites_no_posteriors %>%
      ggplot(aes(x = number_of_sites, y = sd)) +
      geom_point() + 
      geom_smooth(se = FALSE) + 
      labs(y = "Standard deviation", 
           x = "Number of sites", 
           title = "Number of sites") +
      SEACAR::SEACAR_plot_theme()
    
    plot <- samplesize_plot + samplenumber_plot + 
      plot_annotation(title = p,
                      subtitle = paste0(oimmp, " OIMMP Region"),
                      theme = SEACAR::SEACAR_plot_theme()) +
      plot_layout(axes = "collect")
    
    ggsave(plot = plot, filename = paste0(output_path, "ReportFigures/", gsub(" ", "_", oimmp), "_", gsub(" ", "_", p), "_sdplot.png"),
           width = 8, height = 4)
    
    ##### Parameter SD maps -----
    pal <- colorBin("YlOrRd", datawizard::normalize(shiny_store$oyster.managed_area_parameter_positions_sd$sd))
    
    map <- leaflet() %>% 
      addProviderTiles(providers$CartoDB.PositronNoLabels, 
                       options = providerTileOptions(opacity = 0.5))    %>%
      addMapPane("background", zIndex = 400) %>% 
      addMapPane("foreground", zIndex = 500) %>% 
      addScaleBar("bottomright") %>% 
      addControl(
        html = '<div><img src="https://upload.wikimedia.org/wikipedia/commons/8/84/North_Pointer.svg" style="width:25px; opacity:0.6;"></div>',
        position = "bottomright",
        className = "map-title"
      ) %>% 
      addPolygons(data = oimmp_boundaries %>% filter(OIMMP==oimmp),
                  color = "black", weight = 1, options = pathOptions(pane="background"), opacity = 0.5) %>%
      addCircleMarkers(lng = shiny_store$oyster.managed_area_parameter_positions_sd$X,
                       lat = shiny_store$oyster.managed_area_parameter_positions_sd$Y,
                       color = pal(datawizard::normalize(shiny_store$oyster.managed_area_parameter_positions_sd$sd)),
                       radius = 5,
                       options = pathOptions(pane = "foreground")) %>%
      addLegend(pal = pal, values = datawizard::normalize(shiny_store$oyster.managed_area_parameter_positions_sd$sd),
                title = paste("Standard deviation")) %>%
      leaflegend::addLegendSymbol(title = "OIMMP Boundary", 
                      color = "#000000", fillColor = "#000000", fillOpacity = 0.1,
                      values = oimmp, shape = "rect", position = "topright") %>%
      fitBounds(lng1 = max(shiny_store$oyster.managed_area_parameter_positions_sd$X),
                lat1 = max(shiny_store$oyster.managed_area_parameter_positions_sd$Y),
                lng2 = min(shiny_store$oyster.managed_area_parameter_positions_sd$X),
                lat2 = min(shiny_store$oyster.managed_area_parameter_positions_sd$Y)) %>%
      addControl(paste("Oyster", tolower(p), "standard deviation on reefs in", oimmp, "- ExportDate:", exportDate), position="bottomleft", className="map-title")
    
    mapview::mapshot(map, file = paste0(output_path, "ReportFigures/", 
                                        gsub(" ", "_", oimmp), "_", 
                                        gsub(" ", "_", p), "_sdmap.png"), remove_controls = NULL)
  }
}

##### Generate plots to show reef overview
# Load in OIMMP shapefiles
oimmp_boundaries <- sf::st_read("C:/SEACAR Data/SEACARshapes/BoundaryUpdate2025oct3/ORCP_MA_Coral_MAbuff_CHIMMP_OIMMP_2025oct3.shp", quiet = TRUE) %>% 
  rename("OIMMP" = "Region") %>% st_make_valid() %>% st_transform(crs = 4326) %>%
  group_by(OIMMP) %>% summarise() %>% filter(!OIMMP=="9999")
# Overview of all available reefs by OIMMP region
oyster_reefs_shapefile <- st_read("C:/SEACAR Data/SEACARshapes/Oyster_Beds_in_Florida/Oyster_Beds_in_Florida.shp", quiet = TRUE) %>% 
  st_make_valid() %>% 
  st_transform(crs = 4326) %>%
  st_join(oimmp_boundaries, join = st_within)
# Provide overview of all available reefs in each OIMMP region
reef_overview <- oyster_reefs_shapefile %>% group_by(OIMMP) %>% 
  reframe(n = n()) %>% filter(!is.na(OIMMP), OIMMP!="Biscayne Bay and Florida Keys")
# Save file for use within OIMMP_Template.Rmd
saveRDS(reef_overview, file = "output/OIMMP/ReportFigures/reef_overview.rds")

# Load in raw oyster data to compute metrics by OIMMP + UniversalReefID
file_in <- str_subset(list.files(seacar_data_location, full.names = TRUE),"OYSTER")
oy <- fread(file_in, sep="|", na.strings=c("NULL"))

# Overview of data collection by reef in each OIMMP region
reef_data_overview <- oy %>% 
  group_by(OIMMP) %>% 
  mutate(n_tot = length(unique(UniversalReefID))) %>%
  group_by(OIMMP, ProgramID, ProgramName) %>% 
  reframe(
    n_tot = unique(n_tot),
    n_reefs = length(unique(UniversalReefID))
  )
# Save file for use within OIMMP_Template.Rmd
saveRDS(reef_data_overview, file = "output/OIMMP/ReportFigures/reef_data_overview.rds")

# Determine color palette for all programs
oimmp_programs <- unique(reef_data_overview$ProgramID)
prog_palette <- colorRampPalette(seacar_palette)(length(oimmp_programs))
names(prog_palette) <- oimmp_programs

for(oimmp in unique(reef_data_overview$OIMMP)){
  cat("-", oimmp, "\n")
  # Subset reefs for each OIMMP region
  reef_subset <- reef_data_overview %>% filter(OIMMP==oimmp)
  if(nrow(reef_subset)==0) next
  # Gather total number of reefs in region
  n_reefs_tot <- reef_overview %>% filter(OIMMP==oimmp) %>% pull(n)
  # Gather total number of reefs sampled in region
  n_reefs_sampled <- unique(reef_subset$n_tot)
  # Create reef plot to show overview
  reef_plot <- ggplot(reef_subset, aes(x = as.factor(ProgramID), y = n_reefs, fill = as.factor(ProgramID))) +
    geom_bar(stat = "identity", position="stack") +
    geom_text(aes(label = n_reefs, vjust = -0.5)) +
    geom_hline(yintercept = n_reefs_sampled, color="#314963") +
    annotate("text", x = 0.5, y = n_reefs_sampled, 
             label = paste0("Total number of unique mapped reefs sampled in ", oimmp, ": ", n_reefs_sampled), 
             vjust = -1, hjust = 0, color="#314963") +
    labs(x = "SEACAR ProgramID",
         y = "Number of reefs sampled",
         title = paste("Number of mapped reefs sampled in", oimmp, "by SEACAR ProgramID"),
         subtitle = paste(oimmp)) +
    ylim(c(0, n_reefs_sampled*1.1)) +
    scale_fill_manual("SEACAR ProgramID", values=prog_palette) +
    SEACAR::SEACAR_plot_theme() +
    theme(axis.text = element_text(family = "Arial", size=14))
  
  ggsave(reef_plot, filename = paste0(output_path, "ReportFigures/", gsub(" ", "_", oimmp), "_reef_overview.png"), 
         height = 6, width = 10)
}

#### Maps to show proportion of reefs monitored in all OIMMP regions
oimmp_props <- oy %>% 
  group_by(OIMMP) %>%
  reframe(n_reefs = length(unique(UniversalReefID))) %>% 
  left_join(reef_overview) %>% rename("n_total" = "n") %>%
  mutate(prop = round(n_reefs / n_total, 3)*100)
# Combine with oimmp boundaries file
oimmp_props_sf <- oimmp_boundaries %>% left_join(oimmp_props)
# Set up color palette
prop_pal <- colorBin("YlOrRd", oimmp_props_sf$prop, bins = 3)

prop_map <- leaflet(data = oimmp_props_sf, 
                    options = leafletOptions(zoomControl = FALSE)) %>% 
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addPolygons(fillColor = ~prop_pal(prop),
              weight = 1, smoothFactor = 0.5, color = "black",
              fillOpacity = 0.6) %>%
  addLegend(title = "Percent of mapped reefs</br>sampled by OIMMP region",
            pal = prop_pal,
            values = ~prop,
            labFormat = labelFormat(suffix = " %")) %>%
  leafem::addStaticLabels(label = ~prop)

mapshot(prop_map, file = paste0(output_path, "ReportFigures/OIMMP_PctSampled_FL.png"))

## Trivia?
# Most-monitored reef in the state, how many times?
most_sampled <- oy %>% 
  group_by(UniversalReefID, Year, Month) %>% 
  reframe(n = n()) %>%
  group_by(UniversalReefID) %>%
  reframe(sampling_events = sum(n)) %>%
  arrange(desc(sampling_events)) %>% 
  filter(sampling_events == max(sampling_events))

print(paste0("UniversalReefID of most sampled reef: ", most_sampled$UniversalReefID))
print(paste0("Number of times sampled: ", most_sampled$sampling_events))
