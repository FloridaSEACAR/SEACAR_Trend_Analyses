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

wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

# Create necessary filepaths to store results
for(file_path in c("output/EDA/", "output/EDA/hist", "output/EDA/ma", "output/EDA/oimmp")){
  if(!file.exists(file_path)) dir.create(file_path)
}

# seacar color palette
seacar_palette <- SEACAR::seacar_palette2

oyster <- fread(str_subset(list.files(seacar_data_location, full=T), "OYSTER"), sep='|', na.strings = "NULL")
# Determine whether a data point is Inside or Outside of Managed Area Boundaries
oyster$in_MA <- ifelse(is.na(oyster$ManagedAreaName), "Outside MA", "Inside MA")
# Add abbreviated MA names for easier display in table
oyster <- merge(oyster, MA_All[, c("AreaID", "Abbreviation")], all.x=T)
oyster$ManagedAreaName[is.na(oyster$ManagedAreaName)] <- "NA"

##### Histogram plots for each parameter
for(param in c("Shell Height", "Density", "Percent Live")){
  for(pid in oyster[ParameterName==param, unique(ProgramID)]){
    subset <- oyster[ParameterName==param & ProgramID==pid, ]
    
    # dataTable <- Rmisc::summarySE(subset, measurevar = "ResultValue", groupvars = "ManagedAreaName")
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
    ggsave(plot = allPlot, filename = paste0("output/EDA/hist/", pid, "_", gsub(" ", "_", param), ".png"),
           height = 6, width = 8)
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
    
    file_name <- paste0("output/EDA/", plot_type, "/Oyster_EDA_", group, ".png")
    ggsave(filename = file_name, plot = plot, height = 10, width = 10)
    print(paste0("Plot created for ", subtitle, ": ", group))
  }
}

# Zip all figures
out_dir <- paste0("output/EDA/")
fig_list <- list.files(out_dir, recursive = T)
filename <- paste0("OysterEDA_", Sys.Date())
setwd(out_dir)
zip(filename, files=fig_list)
setwd(wd)
