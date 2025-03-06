library(stringr)
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(tidyr)
library(gridExtra)
library(ggpubr)
library(scales)
library(rstudioapi)

# Gets directory of this script and sets it as the working directory
wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

# Create sample location maps? (for MA Report Generation & Atlas)
create_maps <- TRUE

source("../SEACAR_data_location.R")

#Sets abbreviation or label to be used in file names
param_file <- "SpeciesRichness"

# Define output directory
out_dir <- "output"

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
                na.strings = "")

# Declare CW File
files <- list.files(seacar_data_location, full.names = T)
file_in <- str_subset(files, "All_CW")

# Data Import ----
#Import data from coastal wetlands file
data <- fread(file_in, sep="|", header=TRUE, stringsAsFactors=FALSE,
              na.strings=c("NULL"))

file_short <- tail(str_split(file_in, "/")[[1]],1)

cat(paste("The data file used is:", file_short, sep="\n"))

# Filtering ----
# Only interested in Percent Cover measurements
data <- data[ParameterName=="Percent Cover", ]
# Only keep data rows that are Marsh, Marsh succulents, and Mangroves and assoc.
keep_spg <- c("Marsh","Marsh succulents","Mangroves and associates")
data <- data[SpeciesGroup1 %in% keep_spg, ]
# Create ParameterName Column
data$ParameterName <- "Species Richness"
parameter <- "Species Richness"
# Sets units for species richness
unit <- "# of species"
data$ParameterUnits <- unit
cw <- copy(data)

# Remove rows with missing ManagedAreaName
data <- data[!is.na(data$ManagedAreaName),]
data <- data[data$ManagedAreaName!="NA",]
# Remove rows with missing GenusName
data <- data[!is.na(data$GenusName),]
# Remove rows with missing SpeciesName
data <- data[!is.na(data$SpeciesName),]
# Remove rows with missing Months
data <- data[!is.na(data$Month),]
# Remove rows with missing Years
data <- data[!is.na(data$Year),]
# Set ResultValue to be a number value
data$ResultValue <- as.numeric(data$ResultValue)
# Remove rows where ResultValue is 0
data <- data[data$ResultValue!=0,]
# Remove duplicate rows
data <- data[data$MADup==1,]
# Create variable that combines the genus and species name
data$gensp <- paste(data$GenusName, data$SpeciesName, sep=" ")

# Create Species Richness values for groups of unique combinations of
# ManagedAreaName, ProgramID, ProgramName, ProgramLocationID, and SampleDate.
data <- data %>%
  group_by(ManagedAreaName, ProgramID, ProgramName, ProgramLocationID,
           SampleDate, SpeciesGroup1) %>%
  summarise(ParameterName=parameter,
            Year=unique(Year), Month=unique(Month),
            SpeciesRichness=length(unique(gensp)))

# Adds AreaID for each managed area by combining the MA_All datatable to the
# data based on ManagedAreaName
data <- merge.data.frame(MA_All[,c("AreaID", "ManagedAreaName")],
                         data, by="ManagedAreaName")

# Writes this data that is used by the rest of the script to a text file
fwrite(data, paste0(out_dir,"/CoastalWetlands_", param_file, "_UsedData.txt"),
       sep="|")

# Makes sure SampleDate is being stored as a Date object
data$SampleDate <- as.Date(data$SampleDate)

# Creates a variable with the names of all the managed areas that contain
# species observations
cw_MA_Include <- unique(data$ManagedAreaName[!is.na(data$SpeciesRichness)])

# Puts the managed areas in alphabetical order
cw_MA_Include <- cw_MA_Include[order(cw_MA_Include)]

# Determines the number of managed areas used
n <- length(cw_MA_Include)

# Create summary statistics for each managed area based on Year and Month
# intervals.
MA_YM_Stats <- data %>%
  group_by(AreaID, ManagedAreaName, Year, Month, SpeciesGroup1) %>%
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
                             collapse=', '))
# Puts the data in order based on ManagedAreaName, Year, then Month
MA_YM_Stats <- as.data.table(MA_YM_Stats[order(MA_YM_Stats$ManagedAreaName,
                                               MA_YM_Stats$Year,
                                               MA_YM_Stats$Month), ])
# Writes summary statistics to file
fwrite(MA_YM_Stats, paste0(out_dir,"/CoastalWetlands_", param_file,
                           "_MA_MMYY_Stats.txt"), sep="|")
# Removes variable storing data to improve computer memory
rm(MA_YM_Stats)

# Create summary statistics for each managed area based on Year intervals
MA_Y_Stats <- data %>%
  group_by(AreaID, ManagedAreaName, Year, SpeciesGroup1) %>%
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
                             collapse=', '))
# Puts the data in order based on ManagedAreaName then Year
MA_Y_Stats <- as.data.table(MA_Y_Stats[order(MA_Y_Stats$ManagedAreaName,
                                             MA_Y_Stats$Year), ])
# Writes summary statistics to file
fwrite(MA_Y_Stats, paste0(out_dir,"/CoastalWetlands_", param_file,
                          "_MA_Yr_Stats.txt"), sep="|")

# Create summary statistics for each managed area based on Month intervals.
MA_M_Stats <- data %>%
  group_by(AreaID, ManagedAreaName, Month, SpeciesGroup1) %>%
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
                             collapse=', '))
# Puts the data in order based on ManagedAreaName then Month
MA_M_Stats <- as.data.table(MA_M_Stats[order(MA_M_Stats$ManagedAreaName,
                                             MA_M_Stats$Month), ])
# Writes summary statistics to file
fwrite(MA_M_Stats, paste0(out_dir,"/CoastalWetlands_", param_file,
                          "_MA_Mo_Stats.txt"), sep="|")
# Removes variable storing data to improve computer memory
rm(MA_M_Stats)

# Create summary overall statistics for each managed area.
MA_Ov_Stats <- data %>%
  group_by(AreaID, ManagedAreaName, SpeciesGroup1) %>%
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
                             collapse=', '))
# Puts the data in order based on ManagedAreaName
MA_Ov_Stats <- as.data.table(MA_Ov_Stats[order(MA_Ov_Stats$ManagedAreaName), ])
# Creates Year_MinRichness and Year_MaxRichness columns
MA_Ov_Stats$Year_MinRichness <- NA
MA_Ov_Stats$Year_MaxRichness <- NA

# Loops through each ManagedAreaName.
# Determines what year the minimum and maximum species richness occurred
for(m in 1:nrow(MA_Ov_Stats)){
  # Stores ManagedAreaName for this row
  ma <- MA_Ov_Stats$ManagedAreaName[m]
  
  # Skips to next row if there are no data for this combination
  if(MA_Ov_Stats$N_Data[m]==0){
    next
  }
  # Gets subset of data from MA_Y_Stats (yearly summary stats) with this
  # ManagedAreaName
  ds <- MA_Y_Stats[MA_Y_Stats$ManagedAreaName==ma,]
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
fwrite(MA_Ov_Stats, paste0(out_dir,"/CoastalWetlands_", param_file,
                           "_MA_Overall_Stats.txt"), sep="|")
# Removes entries from the overall statistics that do not have data.
# Based on presence or absence of EarliestYear
MA_Ov_Stats <- MA_Ov_Stats[!is.na(MA_Ov_Stats$EarliestYear), ]

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
        legend.text = element_text(hjust=0),
        axis.title.x = element_text(size=10, margin = margin(t = 5, r = 0,
                                                             b = 10, l = 0)),
        axis.title.y = element_text(size=10, margin = margin(t = 0, r = 10,
                                                             b = 0, l = 0)),
        axis.text=element_text(size=10),
        axis.text.x=element_text(angle = -45, hjust = 0))

# Color palette for SEACAR
color_palette <- c("#005396", "#0088B1", "#00ADAE", "#65CCB3", "#AEE4C1", 
                   "#FDEBA8", "#F8CD6D", "#F5A800", "#F17B00")
# All unique SpeciesGroup1 values get assigned a shape and color
cw_groups <- sort(unique(MA_Y_Stats$SpeciesGroup1), decreasing = T)

group_colors <- color_palette[seq_len(length(cw_groups))]
group_shapes <- c(21,22,24,25)
names(group_colors) <- cw_groups
names(group_shapes) <- cw_groups

# Loop that cycles through each managed area with data
if(n==0){
  # Prints a statement if there are no managed areas with appropriate data
  print("There are no monitoring locations that qualify.")
} else {
  for (i in 1:n) {
    ma_i <- cw_MA_Include[i]
    ma_abrev <- MA_All[ManagedAreaName==ma_i, Abbreviation]
    # Gets data for target managed area
    plot_data <- MA_Y_Stats[MA_Y_Stats$ManagedAreaName==ma_i]
    # Determines most recent year with available data for managed area
    t_max <- max(MA_Ov_Stats$LatestYear[MA_Ov_Stats$ManagedAreaName==
                                          ma_i])
    # Determines earliest recent year with available data for managed area
    t_min <- min(MA_Ov_Stats$EarliestYear[MA_Ov_Stats$ManagedAreaName==
                                            ma_i])
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
    
    # Determines what combination of groups are present for managed area
    # and subsets color and shape scheme to be used by plots.
    # Used so only group combinations present for managed area appear in
    # the legend.
    group_colors_plot <- group_colors[unique(plot_data$SpeciesGroup1)]
    group_shapes_plot <- group_shapes[unique(plot_data$SpeciesGroup1)]
    
    # Creates plot object using plot_data.
    # Data is plotted as symbols with connected lines.
    p1 <- ggplot(data=plot_data, group=as.factor(SpeciesGroup1)) +
      geom_line(aes(x=Year, y=Mean, color=as.factor(SpeciesGroup1)),
                size=0.75, alpha=1) +
      geom_point(aes(x=Year, y=Mean, fill=as.factor(SpeciesGroup1),
                     shape=as.factor(SpeciesGroup1)), size=2,
                 color="#333333", alpha=1) +
      labs(title="Coastal Wetlands Species Richness",
           subtitle=ma_i,
           x="Year", y="Richness (# of species)",
           fill="Species group", color="Species group",
           shape="Species group") +
      scale_x_continuous(limits=c(t_min-0.25, t_max+0.25),
                         breaks=seq(t_max, t_min, brk)) +
      scale_y_continuous(limits=c(y_min, y_max),
                         breaks=pretty_breaks(n=5)) +
      scale_fill_manual(values=group_colors_plot) +
      scale_color_manual(values=group_colors_plot) +
      scale_shape_manual(values=group_shapes_plot) +
      plot_theme
    # Sets file name of plot created
    outname <- paste0("CoastalWetlands_", param_file, "_", ma_abrev, ".png")
    # Saves plot as a png image
    png(paste0(out_dir, "/Figures/", outname),
        width = 8,
        height = 4,
        units = "in",
        res = 200)
    print(p1)
    dev.off()
  }
}

#Gets list of all image files in output/Figures and creates zip directory
fig_list <- list.files(paste0(out_dir, "/Figures"), pattern=".png", full=FALSE)
setwd(paste0(out_dir, "/Figures"))
zip("CoastalWetlandsFigures", files=fig_list)
setwd(wd)

if(create_maps){
  source("CW_Create_Maps.R")
}

#Renders CoastalWetlands_SpeciesRichness.Rmd and writes the report to a pdf and 
#Word document stored in output directory
file_out <-  paste0("CoastalWetlands_", param_file, "_Report")

rmarkdown::render(input = "CoastalWetlands_SpeciesRichness.Rmd", 
                  output_format = "pdf_document",
                  output_file = paste0(file_out, ".pdf"),
                  output_dir = out_dir,
                  clean=TRUE)

#Removes unwanted files created in the rendering process
unlink(paste0(out_dir, "/", file_out, ".md"))
unlink(paste0(out_dir, "/", file_out, "_files"), recursive=TRUE)
unlink(paste0(file_out, ".log"))