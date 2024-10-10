# This script runs Analyses for Nekton Species Richness
# .png outputs are created and zipped into "output/Figures/NektonFigures.zip"
# Nekton_SpeciesRichness_Report.pdf is rendered using Nekton_SpeciesRichness.Rmd
library(knitr)
library(readr)
library(dplyr)
library(data.table)
library(utils)
library(rstudioapi)
library(lubridate)
library(ggplot2)
library(scales)
library(tidyr)
library(gridExtra)
library(ggpubr)
library(scales)

# Gets directory of this script and sets it as the working directory
wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

source("../SEACAR_data_location.R")

#Set output directory
out_dir <- "output"

#This script is designed to only determine species richness from the Nekton presence data
param_name <- "Presence/Absence"

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
                na.strings = "")

#Gets the files with the file names containing the desired parameter
file_in <- list.files(seacar_data_location, pattern="All_NEKTON", full=TRUE)

#Sets abbreviation or label to be used in file names
param_file <- "SpeciesRichness"

#Gets the specific file used and removes the directory names
file_short <- tail(str_split(file_in, "/")[[1]], 1)

# File Import ----
#Import data from nekton file
data <- fread(file_in, sep="|", header=TRUE, stringsAsFactors=FALSE,
              na.strings=c("NULL"))

cat(paste("The data file used is:", file_short, sep="\n"))

# Filtering ----
# Filter data for the desired parameter
data <- data[ParameterName==param_name, ]

if(param_name=="Presence/Absence"){
  parameter <- "Species Richness"
}
# Makes sure EffortCorrection is numeric value
data$EffortCorrection_100m2 <- as.numeric(data$EffortCorrection_100m2)

# Remove any data with missing EffortCorrection values
data <- data[!is.na(data$EffortCorrection_100m2),]

# Only keep data that has non-zero EffortCorrection values
data <- data[data$EffortCorrection_100m2!=0,]

# Remove any data with missing ResultValue entries
data <- data[!is.na(data$ResultValue),]

# Create Species Richness values for groups of unique combinations of
# ManagedAreaName, ProgramID, ProgramName, ProgramLocationID, SampleDate,
# GearType, and GearSize_m.
data <- data %>%
  group_by(AreaID, ManagedAreaName, ProgramID, ProgramName, ProgramLocationID,
           SampleDate, GearType, GearSize_m) %>%
  summarise(ParameterName=parameter,
            Year=unique(Year), Month=unique(Month),
            N_Species=sum(ResultValue),
            EffortCorrection_100m2=as.numeric(unique(EffortCorrection_100m2)),
            SpeciesRichness=N_Species/unique(EffortCorrection_100m2),
            .groups = "keep")

# Writes this data that is used by the rest of the script to a text file
fwrite(data, paste0(out_dir,"/Nekton_", param_file, "_UsedData.txt"), sep="|")

# Makes sure SampleDate is being stored as a Date object
data$SampleDate <- as.Date(data$SampleDate)

# Creates a variable with the names of all the managed areas that contain
# species observations
nekton_MA_Include <- unique(data$ManagedAreaName[!is.na(data$N_Species)])

# Puts the managed areas in alphabetical order
nekton_MA_Include <- nekton_MA_Include[order(nekton_MA_Include)]

# Determines the number of managed areas used
n <- length(nekton_MA_Include)

# managed_area_stats ----
# Create summary statistics for each managed area based on Year and Month
# intervals, and each gear type and size.
MA_YM_Stats <- data %>%
  group_by(AreaID, ManagedAreaName, Year, Month, GearType, GearSize_m) %>%
  summarize(ParameterName=parameter,
            N_Data=length(na.omit(SpeciesRichness)),
            Min=min(SpeciesRichness),
            Max=max(SpeciesRichness),
            Median=median(SpeciesRichness),
            Mean=mean(SpeciesRichness),
            StandardDeviation=sd(SpeciesRichness),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '),
            .groups = "keep")
# Puts the data in order based on ManagedAreaName, Year, Month, then GearSize
MA_YM_Stats <- as.data.table(MA_YM_Stats[order(MA_YM_Stats$ManagedAreaName,
                                               MA_YM_Stats$Year,
                                               MA_YM_Stats$Month,
                                               MA_YM_Stats$GearSize_m), ])
# Writes summary statistics to file
fwrite(MA_YM_Stats, paste0(out_dir,"/Nekton_", param_file,
                           "_MA_MMYY_Stats.txt"), sep="|")
# Removes variable storing data to improve computer memory
rm(MA_YM_Stats)

# Create summary statistics for each managed area based on Year intervals,
# and each gear type and size.
MA_Y_Stats <- data %>%
  group_by(AreaID, ManagedAreaName, Year, GearType, GearSize_m) %>%
  summarize(ParameterName=parameter,
            N_Data=length(na.omit(SpeciesRichness)),
            Min=min(SpeciesRichness),
            Max=max(SpeciesRichness),
            Median=median(SpeciesRichness),
            Mean=mean(SpeciesRichness),
            StandardDeviation=sd(SpeciesRichness),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '),
            .groups = "keep")
# Puts the data in order based on ManagedAreaName, Year, then GearSize
MA_Y_Stats <- as.data.table(MA_Y_Stats[order(MA_Y_Stats$ManagedAreaName,
                                             MA_Y_Stats$Year,
                                             MA_Y_Stats$GearSize_m), ])
# Writes summary statistics to file
fwrite(MA_Y_Stats, paste0(out_dir,"/Nekton_", param_file,
                          "_MA_Yr_Stats.txt"), sep="|")

# Create summary statistics for each managed area based on Month intervals,
# and each gear type and size.
MA_M_Stats <- data %>%
  group_by(AreaID, ManagedAreaName, Month, GearType, GearSize_m) %>%
  summarize(ParameterName=parameter,
            N_Data=length(na.omit(SpeciesRichness)),
            Min=min(SpeciesRichness),
            Max=max(SpeciesRichness),
            Median=median(SpeciesRichness),
            Mean=mean(SpeciesRichness),
            StandardDeviation=sd(SpeciesRichness),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '),
            .groups = "keep")
# Puts the data in order based on ManagedAreaName, Month, then GearSize
MA_M_Stats <- as.data.table(MA_M_Stats[order(MA_M_Stats$ManagedAreaName,
                                             MA_M_Stats$Month,
                                             MA_M_Stats$GearSize_m), ])
# Writes summary statistics to file
fwrite(MA_M_Stats, paste0(out_dir,"/Nekton_", param_file,
                          "_MA_Mo_Stats.txt"), sep="|")
# Removes variable storing data to improve computer memory
rm(MA_M_Stats)

# Create summary overall statistics for each managed area based each gear type
# and size.
MA_Ov_Stats <- data %>%
  group_by(AreaID, ManagedAreaName, GearType, GearSize_m) %>%
  summarize(ParameterName=parameter,
            N_Years=length(unique(na.omit(Year))),
            EarliestYear=min(Year),
            LatestYear=max(Year),
            N_Data=length(na.omit(SpeciesRichness)),
            Min=min(SpeciesRichness),
            Max=max(SpeciesRichness),
            Median=median(SpeciesRichness),
            Mean=mean(SpeciesRichness),
            StandardDeviation=sd(SpeciesRichness),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '),
            .groups = "keep")
# Puts the data in order based on ManagedAreaName then GearSize
MA_Ov_Stats <- as.data.table(MA_Ov_Stats[order(MA_Ov_Stats$ManagedAreaName,
                                               MA_Ov_Stats$GearSize_m), ])
# Creates Year_MinRichness and Year_MaxRichness columns
MA_Ov_Stats$Year_MinRichness <- NA
MA_Ov_Stats$Year_MaxRichness <- NA

# Loops through each ManagedAreaName, GearType, and GearSize_m.
# determines what year the minimum and maximum species richness occurred
for(m in 1:nrow(MA_Ov_Stats)){
  # Stores ManagedAreaName, GearType, and GearSize_m for this row
  ma <- MA_Ov_Stats$ManagedAreaName[m]
  gear <- MA_Ov_Stats$GearType[m]
  size <- MA_Ov_Stats$GearSize_m[m]
  # Skips to next row if there are no data for this combination
  if(MA_Ov_Stats$N_Data[m]==0){
    next
  }
  # Gets subset of data from MA_Y_Stats (yearly summary stats) with this
  # combination of ManagedAreaName, GearType, and GearSize_m
  ds <- MA_Y_Stats[MA_Y_Stats$ManagedAreaName==ma &
                     MA_Y_Stats$GearType==gear &
                     MA_Y_Stats$GearSize_m==size,]
  # Gets the minimum and maximum Mean (yearly averages)
  min <- min(ds$Mean)
  max <- max(ds$Mean)
  #Determines what years those minimum and maximum values occured
  year_min <- ds$Year[ds$Mean==min]
  year_max <- ds$Year[ds$Mean==max]
  # Stores the occurrence years of the minimum and maximum into the overall
  # stats for this row
  MA_Ov_Stats$Year_MinRichness[m] <- year_min
  MA_Ov_Stats$Year_MaxRichness[m] <- year_max
}
# Replaces blank ProgramIDs with NA (missing values)
MA_Ov_Stats$ProgramIDs <- replace(MA_Ov_Stats$ProgramIDs,
                                  MA_Ov_Stats$ProgramIDs=="", NA)
MA_Ov_Stats$Programs <- replace(MA_Ov_Stats$Programs,
                                MA_Ov_Stats$Programs=="", NA)
# Write overall statistics to file
fwrite(MA_Ov_Stats, paste0(out_dir,"/Nekton_", param_file,
                           "_MA_Overall_Stats.txt"), sep="|")
# Removes entries from the overall statistics that do not have data.
# Based on presence or absence of EarliestYear
MA_Ov_Stats <- MA_Ov_Stats[!is.na(EarliestYear), ]

# SpeciesRichPlot ----
# Defines standard plot theme: black and white, no major or minor grid lines,
# Arial font. Title is centered, size 12, and blue (hex coded). Subtitle is
# centered, size 10, and blue (hex coded). Legend title is size 10 and the
# legend is left-justified. X-axis title is size 10 and the margins are padded
# at the top and bottom to give more space for angled axis labels. Y-axis title
# is size 10 and margins are padded on the right side to give more space for
# axis labels. Axis labels are size 10 and the x-axis labels are rotated -45
# degrees with a horizontal justification that aligns them with the tick mark
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

# Color palette for SEACAR
color_palette <- c("#005396", "#0088B1", "#00ADAE", "#65CCB3", "#AEE4C1", "#FDEBA8", "#F8CD6D", "#F5A800", "#F17B00")

# Defines and sets variable with standardized gear colors for plots
gear_colors <- c("Trawl (4.8 m)"=color_palette[1],
                 "Trawl (6.1 m)"=color_palette[2],
                 "Tow, Trawl, Diver Count (183 m)"=color_palette[3],
                 "Tow, Trawl, Diver Count (6.1 m)"=color_palette[4])

# Defines and sets variable with standardized gear shapes for plots
gear_shapes <- c("Trawl (4.8 m)"=21,
                 "Trawl (6.1 m)"=22,
                 "Tow, Trawl, Diver Count (183 m)"=24,
                 "Tow, Trawl, Diver Count (6.1 m)"=25)

# Loop that cycles through each managed area with data
if(n==0){
  # Prints a statement if there are no managed areas with appropriate data
  print("There are no monitoring locations that qualify.")
} else {
  for (i in 1:n) {
    ma_i <- nekton_MA_Include[i]
    ma_abrev <- MA_All[ManagedAreaName==ma_i, Abbreviation]
    # Gets data for target managed area
    plot_data <- MA_Y_Stats[ManagedAreaName==ma_i, ]
    # Gets the gear type(s) present for the managed area.
    # Combine type and size into one label for plots
    plot_data$GearType_Plot <- paste0(plot_data$GearType, " (",
                                      plot_data$GearSize_m, " m)")
    # Determines most recent year with available data for managed area
    t_max <- max(MA_Ov_Stats[ManagedAreaName==ma_i, LatestYear])
    # Determines earliest recent year with available data for managed area
    t_min <- min(MA_Ov_Stats[ManagedAreaName==ma_i, EarliestYear])
    # Determines how many years of data are present
    t <- t_max-t_min
    
    # Creates break intervals for plots based on number of years of data
    if(t>=30){
      # Set breaks to every 10 years if more than 30 years of data
      brk <- -10
    }else if(t<30 & t>=10){
      # Set breaks to every 5 years if between 30 and 10 years of data
      brk <- -5
    }else if(t<10 & t>=4){
      # Set breaks to every 2 years if between 10 and 4 years of data
      brk <- -2
    }else if(t<4 & t>=2){
      # Set breaks to every year if between 4 and 2 years of data
      brk <- -1
    }else if(t<2){
      # Set breaks to every year if less than 2 years of data
      brk <- -1
      # Sets t_max to be 1 year greater and t_min to be 1 year lower
      # Forces graph to have at least 3 tick marks
      t_max <- t_max+1
      t_min <- t_min-1
    }
    # Determine range of data values for the managed area
    y_range <- max(plot_data$Mean) - min(plot_data$Mean)
    
    # Determines lower bound of y-axis based on data range. Set based on
    # relation of data range to minimum value. Designed to set lower boundary
    # to be 10% of the data range below the minimum value
    y_min <- if(min(plot_data$Mean)-(0.1*y_range)<0){
      # If 10% of the data range below the minimum value is less than 0,
      # set as 0
      y_min <- 0
    } else {
      # Otherwise set minimum bound as 10% data range below minimum value
      y_min <- min(plot_data$Mean)-(0.1*y_range)
    }
    
    # Sets upper bound of y-axis to be 10% of the data range above the
    # maximum value.
    y_max <- max(plot_data$Mean)+(0.1*y_range)
    
    # Determines what combination of gear are present for managed area
    # and subsets color and shape scheme to be used by plots.
    # Used so only gear combinations present for managed area appear in
    # the legend.
    gear_colors_plot <- gear_colors[unique(plot_data$GearType_Plot)]
    gear_shapes_plot <- gear_shapes[unique(plot_data$GearType_Plot)]
    
    # Creates plot object using plot_data and grouping by the plot gear types.
    # Data is plotted as symbols with connected lines.
    p1 <- ggplot(data=plot_data, group=as.factor(GearType_Plot)) +
      geom_line(aes(x=Year, y=Mean, color=as.factor(GearType_Plot)),
                size=0.75, alpha=1) +
      geom_point(aes(x=Year, y=Mean, fill=as.factor(GearType_Plot),
                     shape=as.factor(GearType_Plot)), size=2,
                 color="#333333", alpha=1) +
      labs(title="Nekton Species Richness",
           subtitle=ma_i,
           x="Year", y=bquote('Richness (species/100'*~m^{2}*')'),
           fill="Gear type", color="Gear type", shape="Gear type") +
      scale_x_continuous(limits=c(t_min-0.25, t_max+0.25),
                         breaks=seq(t_max, t_min, brk)) +
      scale_y_continuous(limits=c(y_min, y_max),
                         breaks=pretty_breaks(n=5)) +
      scale_fill_manual(values=gear_colors_plot) +
      scale_color_manual(values=gear_colors_plot) +
      scale_shape_manual(values=gear_shapes_plot) +
      plot_theme
    # Sets file name of plot created
    outname <- paste0("Nekton_", param_file, "_", ma_abrev, ".png")
    # Saves plot as a png image
    png(paste0(out_dir, "/Figures/", outname),
        width = 8,
        height = 4,
        units = "in",
        res = 200)
    print(p1)
    dev.off()
    
    # Creates a data table object to be shown underneath plots in report
    ResultTable <- MA_Ov_Stats[ManagedAreaName==ma_i, ]
    # Removes location, gear, and parameter information because it is in plot
    # labels
    ResultTable <- ResultTable[,-c("AreaID", "ManagedAreaName",
                                   "ProgramIDs", "Programs", "GearType_Plot",
                                   "ParameterName")]
    # Renames StandardDeviation to StDev to save horizontal space
    ResultTable <- ResultTable %>%
      rename("StDev"="StandardDeviation")
    # Converts all non-integer values to 2 decimal places for space
    ResultTable$Min <- round(ResultTable$Min, digits=2)
    ResultTable$Max <- round(ResultTable$Max, digits=2)
    ResultTable$Median <- round(ResultTable$Median, digits=2)
    ResultTable$Mean <- round(ResultTable$Mean, digits=2)
    ResultTable$StDev <- round(ResultTable$StDev, digits=2)
    # Stores as plot table object
    t1 <- ggtexttable(ResultTable, rows = NULL,
                      theme=ttheme(base_size=7))
    # Combines plot and table into one figure
    print(ggarrange(p1, t1, ncol=1, heights=c(0.85, 0.15)))
    
    # Add extra space at the end to prevent the next figure from being too
    # close. Does not add space after last plot
    if(i!=n){
      cat("\n \n \n \n") 
    }
  }
}

#Gets list of all image files in output/Figures and creates zip directory
fig_list <- list.files(paste0(out_dir, "/Figures"), pattern=".png", full=FALSE)
setwd(paste0(out_dir, "/Figures"))
zip("NektonFigures", files=fig_list)
setwd(wd)

#Renders Nekton_SpeciesRichness.Rmd and writes the report to a pdf and 
#document stored in output directory
file_out <-  paste0("Nekton_", param_file, "_Report")

rmarkdown::render(input = "Nekton_SpeciesRichness.Rmd", 
                  output_format = "pdf_document",
                  output_file = paste0(file_out, ".pdf"),
                  output_dir = out_dir,
                  clean=TRUE)

#Removes unwanted files created in the rendering process
unlink(paste0(out_dir, "/", file_out, ".md"))
unlink(paste0(out_dir, "/", file_out, ".log"))
unlink(paste0(out_dir, "/", file_out, "_files"), recursive=TRUE)