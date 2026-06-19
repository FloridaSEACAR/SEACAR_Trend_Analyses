library(data.table)
library(tidyverse)
library(ggplot2)
library(glue)
library(plotrix)
library(patchwork)
library(gridExtra)
library(ggblend)
library(rstudioapi)
library(scales)
library(SEACAR)

source("../SEACAR_data_location.R")

wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

MA_All <- SEACAR::ManagedAreas

# Create necessary filepaths to store results
for(file_path in c("output/EDA/", "output/EDA/hist", "output/EDA/ma", "output/EDA/oimmp")){
  if(!file.exists(file_path)) dir.create(file_path)
}

# seacar color palette
seacar_palette <- SEACAR::seacar_palette2
oy_file_loc <- str_subset(list.files(seacar_data_location, full=T), "OYSTER")
oyster <- fread(oy_file_loc, sep='|', na.strings = "NULL")
# Apply Managed Area transformation - de-concatenate MA names
oyster <- setDT(SEACAR::clean_managed_areas(oyster, "ma"))
# Determine whether a data point is Inside or Outside of Managed Area Boundaries
oyster$in_MA <- ifelse(is.na(oyster$ManagedAreaName), "Outside MA", "Inside MA")
# Add abbreviated MA names for easier display in table
oyster <- merge(oyster, MA_All[, c("AreaID", "Abbreviation")], all.x=T)
oyster$ManagedAreaName[is.na(oyster$ManagedAreaName)] <- "NA"
# Record current export information & write to tracking file
export_info <- fread("output/EDA/ExportTracking.csv") %>%
  rbind(data.frame("Date" = as.IDate(Sys.Date()), 
                   "FileName" = tail(str_split_1(oy_file_loc, "/"), 1))) %>%
  distinct()
fwrite(export_info, "output/EDA/ExportTracking.csv")

# Create dated folder to store results previous plots to allow for comparison
for(folder in c("output/EDA/hist/", "output/EDA/ma/", "output/EDA/oimmp/")){
  new_filepath <- paste0(folder, Sys.Date())
  if(!dir.exists(new_filepath)) dir.create(new_filepath, recursive = T)
}

##### Histogram plots for each parameter
hist_output <- paste0("output/EDA/hist/", Sys.Date(), "/")
# Locate previous export and list previous plots to create side-by-side comparison
prev_folder <- export_info[which(export_info$Date==Sys.Date()) - 1, Date]
prev_imgs <- list.files(paste0("output/EDA/hist/", prev_folder, "/"), full=T)
# Create title variables from file names
old_file_name <- export_info[Date==prev_folder, FileName]
new_file_name <- export_info[Date==Sys.Date(), FileName]

for(param in c("Shell Height", "Density", "Percent Live")){
  for(pid in oyster[ParameterName==param, unique(ProgramID)]){
    subset <- oyster[ParameterName==param & ProgramID==pid, ]
    dataTable <- subset %>% group_by(Abbreviation, QuadSize_m2) %>% 
      reframe(N = n(),
                Mean = round(mean(ResultValue),2),
                Median = round(median(ResultValue),2),
                Min = min(ResultValue),
                Max = max(ResultValue),
                sd = round(sd(ResultValue),2),
                se = round(std.error(ResultValue),2))
    
    plot <- ggplot(subset, aes(x = ResultValue)) +
      geom_histogram(binwidth = 1, color = "black", fill = "skyblue") +
      facet_wrap(ParameterName ~ in_MA, scales = "free_y") +
      theme_minimal() +
      labs(
        title = glue("Histogram of {param} Values for ID_{pid}"),
        x = param,
        y = "Count"
      )
    
    allPlot <- (plot / wrap_table(dataTable))
    ggsave(plot = allPlot, filename = paste0(hist_output, pid, "_", gsub(" ", "_", param), ".png"),
           height = 6, width = 8)
    
    # Locate old plot
    old_plot_img <- magick::image_read(str_subset(prev_imgs, paste0(pid, "_", gsub(" ", "_", param))))
    old_plot <- cowplot::ggdraw() + cowplot::draw_image(old_plot_img) + 
      cowplot::draw_label(as.character(old_file_name), x = 0.5, y = 1, color = "cornflowerblue")
    # Read in new plot
    new_plot_img <- magick::image_read(paste0(hist_output, pid, "_", gsub(" ", "_", param), ".png"))
    new_plot <- cowplot::ggdraw() + cowplot::draw_image(new_plot_img) + 
      cowplot::draw_label(as.character(new_file_name), x = 0.5, y = 1, color = "cornflowerblue")
    
    # Combine & save
    combined_plot <- (new_plot + old_plot)
    ggsave(plot = combined_plot, filename = paste0("output/EDA/hist/", pid, "_", gsub(" ", "_", param), ".png"),
           height = 6, width = 12)
    
    print(paste0("Plot created for ID_", pid, ": ", param))
  }
}

##### Jittered plots by Parameter
plot_jitter <- position_jitter(width = 0.5, height = 0.5, seed=42)
# Generate plots by MA and by OIMMP region
plot_types <- c("ma", "oimmp")
for(plot_type in plot_types){
  col_name <- ifelse(plot_type=="ma", "ManagedAreaName", "OIMMP")
  subtitle <- ifelse(plot_type=="ma", "ManagedAreaName", "OIMMP Region")
  jitter_output <- paste0("output/EDA/", plot_type, "/", Sys.Date(), "/")
  
  # List previous plots to create side-by-side comparison
  prev_imgs2 <- list.files(paste0("output/EDA/", plot_type, "/", prev_folder, "/"), full=T)
  
  for(group in oyster[,unique(get(col_name))]){
    subset <- oyster[get(col_name)==group, ]
    
    progs <- unique(subset$ProgramID)
    pal <- seacar_palette[seq(from = 1, to = length(seacar_palette), by = (length(seacar_palette) / length(progs)))]
    names(pal) <- progs
    
    plot <- ggplot(subset, aes(x=as.factor(Year), y=ResultValue, color=as.factor(ProgramID))) +
      geom_point(position=plot_jitter, alpha=0.6) +
      labs(y = "Parameter units", 
           x = "Year",
           color = "ProgramID",
           title=paste0("Parameter values by ProgramID for ", ifelse(group=="NA", "No Managed Area", group)),
           subtitle = subtitle) +
      scale_color_manual(values = pal) +
      scale_x_discrete(breaks = pretty_breaks()) +
      facet_wrap(~ParameterName, scales = "free_y")
    
    file_name <- paste0(jitter_output, "Oyster_EDA_", group, ".png")
    ggsave(filename = file_name, plot = plot, height = 10, width = 10)
    
    # Locate old plot
    old_plot_img <- magick::image_read(str_subset(prev_imgs2, paste0(group)))
    old_plot <- try(cowplot::ggdraw() + cowplot::draw_image(old_plot_img) + cowplot::draw_label(as.character(old_file_name), x = 0.5, y = 1))
    
    if("try-error" %in% class(old_plot)){
      "No previous image available"
      ggsave(plot = plot, filename = paste0("output/EDA/", plot_type, "/", "Oyster_EDA_", group, ".png"),
             height = 6, width = 12)
    } else {
      # Read in new plot
      new_plot_img <- magick::image_read(file_name)
      new_plot <- cowplot::ggdraw() + cowplot::draw_image(new_plot_img) + cowplot::draw_label(as.character(new_file_name), x = 0.5, y = 1)
      
      # Combine & save
      combined_plot <- (new_plot + old_plot)
      ggsave(plot = combined_plot, filename = paste0("output/EDA/", plot_type, "/", "Oyster_EDA_", group, ".png"),
             height = 6, width = 12)      
    }
    
    print(paste0("Plot created for ", subtitle, ": ", group))
  }
}

# Zip all figures
out_dir <- paste0("output/EDA/")
hist_files <- paste0("hist/", list.files(paste0(out_dir, "hist"), recursive = F, pattern = ".png"))
ma_files <- paste0("ma/", list.files(paste0(out_dir, "ma"), recursive = F, pattern = ".png"))
oimmp_files <- paste0("oimmp/", list.files(paste0(out_dir, "oimmp"), recursive = F, pattern = ".png"))

fig_list <- c(hist_files, ma_files, oimmp_files)
filename <- paste0("OysterEDA_", Sys.Date())
setwd(out_dir)
zip(filename, files=fig_list)
setwd(wd)
