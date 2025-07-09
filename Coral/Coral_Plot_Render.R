library(SEACAR)
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
library(nlme)
library(stringr)
options(scipen=999)

# Coral Species Richness ----
# Gets directory of this script and sets it as the working directory
wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

# Create sample location maps? (for MA Report Generation & Atlas)
create_maps <- TRUE

source("../SEACAR_data_location.R")

#Set output directory
out_dir <- "output/SpeciesRichness"

# Sets coral file to only care about "SpeciesRichness" files
param_name <- "SpeciesRichness"

#Sets abbreviation or label to be used in file names
param_file <- "SpeciesRichness"

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- SEACAR::ManagedAreas

# Load in table descriptions
tableDesc <- SEACAR::TableDescriptions %>%
  mutate(DescriptionHTML = Description,
         DescriptionLatex = stringi::stri_replace_all_regex(
           Description,
           pattern = c("<i>", "</i>", "&#8805;"),
           replacement = c("*", "*", ">="),
           vectorize = FALSE
         )) %>%
  as.data.table()

# Load in figure captions
figureCaptions <- SEACAR::FigureCaptions

#Gets the files with the file names containing the desired parameter
file_in <- list.files(seacar_data_location, pattern="All_CORAL", full=TRUE)

#Gets the specific file used and removes the directory names
file_short <- tail(str_split(file_in, "/")[[1]], 1)

# Read in data file
data <- fread(file_in, sep="|", header=TRUE, stringsAsFactors=FALSE,
              na.strings=c("NULL","","NA"))

cat(paste("The data file(s) used:", file_short, sep="\n"))

# Make a copy of main data file to save for Coral Percent Cover
data2 <- copy(data)
coral_pc_data <- copy(data) # Save copy for use with maps (Percent Cover)

## Data Filtering ----
# Only keep data for Presence of grazers and reef-dependent species
data <- data[ParameterName=="Presence/Absence" & 
               SpeciesGroup1 %in% c("Grazers and reef dependent species", 
                                    "Reef fish"), ]
coral_sr_data <- copy(data) # Save copy for use with maps (Species Richness)
# Create ParameterName Column
data$ParameterName <- "Species Richness"
parameter <- "Species Richness"
title_param <- "Species Richness - Grazers and Reef-Dependent Species"

# Sets units for species richness
unit <- "# of species"
data$ParameterUnits <- unit

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
# Remove rows with missing SpeciesGroup1
data <- data[!is.na(data$SpeciesGroup1),]
# Set ResultValue to be a number value
data$ResultValue <- as.numeric(data$ResultValue)
# Remove rows where ResultValue is 0 and missing
data <- data[data$ResultValue!=0,]
data <- data[!is.na(data$ResultValue),]
# Remove duplicate rows
# data <- data[data$MADup==1,]
# Create variable that combines the genus and species name
data$gensp <- paste(data$GenusName, data$SpeciesName, sep=" ")

# Create Species Richness values for groups of unique combinations of
# ManagedAreaName, ProgramID, ProgramName, ProgramLocationID, and SampleDate.
data <- data[data$ResultValue==1] %>%
  group_by(AreaID, ManagedAreaName, ProgramID, ProgramName, ProgramLocationID,
           SampleDate) %>%
  summarise(ParameterName=parameter,
            Year=unique(Year), Month=unique(Month),
            SpeciesRichness=length(unique(gensp)))
setDT(data)

# Writes this data that is used by the rest of the script to a text file
fwrite(data, paste0(out_dir,"/Coral_", param_file, "_UsedData.txt"),
       sep="|")

# Makes sure SampleDate is being stored as a Date object
data$SampleDate <- as.Date(data$SampleDate)

# Creates a variable with the names of all the managed areas that contain
# species observations
coral_sr_MA_Include <- unique(data$ManagedAreaName[!is.na(data$SpeciesRichness)])

# Puts the managed areas in alphabetical order
coral_sr_MA_Include <- coral_sr_MA_Include[order(coral_sr_MA_Include)]

# Determines the number of managed areas used
n <- length(coral_sr_MA_Include)

## Managed Area Stats ----
# Create summary statistics for each managed area based on Year and Month
# intervals.
MA_YM_Stats <- data %>%
  group_by(AreaID, ManagedAreaName, Year, Month) %>%
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
# Puts the data in order based on ManagedAreaName, Year, then Month
MA_YM_Stats <- as.data.table(MA_YM_Stats[order(MA_YM_Stats$ManagedAreaName,
                                               MA_YM_Stats$Year,
                                               MA_YM_Stats$Month), ])
# Writes summary statistics to file
fwrite(MA_YM_Stats, paste0(out_dir,"/Coral_", param_file,
                           "_MA_MMYY_Stats.txt"), sep="|")
# Removes variable storing data to improve computer memory
rm(MA_YM_Stats)

# Create summary statistics for each managed area based on Year intervals
MA_Y_Stats <- data %>%
  group_by(AreaID, ManagedAreaName, Year) %>%
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
# Puts the data in order based on ManagedAreaName then Year
MA_Y_Stats <- as.data.table(MA_Y_Stats[order(MA_Y_Stats$ManagedAreaName,
                                             MA_Y_Stats$Year), ])
# Writes summary statistics to file
fwrite(MA_Y_Stats, paste0(out_dir,"/Coral_", param_file,
                          "_MA_Yr_Stats.txt"), sep="|")

# Create summary statistics for each managed area based on Month intervals.
MA_M_Stats <- data %>%
  group_by(AreaID, ManagedAreaName, Month) %>%
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
# Puts the data in order based on ManagedAreaName then Month
MA_M_Stats <- as.data.table(MA_M_Stats[order(MA_M_Stats$ManagedAreaName,
                                             MA_M_Stats$Month), ])
# Writes summary statistics to file
fwrite(MA_M_Stats, paste0(out_dir,"/Coral_", param_file,
                          "_MA_Mo_Stats.txt"), sep="|")
# Removes variable storing data to improve computer memory
rm(MA_M_Stats)

# Create summary overall statistics for each managed area.
MA_Ov_Stats <- data %>%
  group_by(AreaID, ManagedAreaName) %>%
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
  ds <- MA_Y_Stats[ManagedAreaName==ma,]
  # Gets the minimum and maximum Mean (yearly averages)
  min <- min(ds$Mean)
  max <- max(ds$Mean)
  #Determines what years those minimum and maximum values occured
  year_min <- ds[Mean==min, Year]
  year_max <- ds[Mean==max, Year]
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
fwrite(MA_Ov_Stats, paste0(out_dir,"/Coral_", param_file,
                           "_MA_Overall_Stats.txt"), sep="|")
# Removes entries from the overall statistics that do not have data.
# Based on presence or absence of EarliestYear
MA_Ov_Stats <- MA_Ov_Stats[!is.na(MA_Ov_Stats$EarliestYear), ]

## Plot Species Richness ----
# Defines standard plot theme: black and white, no major or minor grid lines,
# Arial font. Title is centered, size 12, and blue (hex coded). Subtitle is
# centered, size 10, and blue (hex coded). Legend title is size 10 and the
# legend is left-justified. X-axis title is size 10 and the margins are padded
# at the top and bottom to give more space for angled axis labels. Y-axis title
# is size 10 and margins are padded on the right side to give more space for
# axis labels. Axis labels are size 10 and the x-axis labels are rotated -45
# degrees with a horizontal justification that aligns them with the tick mark
plot_theme <- SEACAR::SEACAR_plot_theme()

# Color palette for SEACAR
color_palette <- c("#005396", "#0088B1", "#00ADAE", "#65CCB3", "#AEE4C1",
                   "#FDEBA8", "#F8CD6D", "#F5A800", "#F17B00")

# Loop that cycles through each managed area with data
if(n==0){
  # Prints a statement if there are no managed areas with appropriate data
  print("There are no monitoring locations that qualify.")
} else {
  for (i in 1:n) {
    ma_i <- coral_sr_MA_Include[i]
    # Get abbreviated name for filename
    ma_abrev <- MA_All[ManagedAreaName==ma_i, Abbreviation]
    # Gets data for target managed area
    plot_data <- MA_Y_Stats[ManagedAreaName==ma_i, ]
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
    
    # Determines lower bound of y-axis based on data range.
    y_min <- 0
    
    # Sets upper bound of y-axis to be 10% of the data range above the
    # maximum value.
    y_max <- max(plot_data$Mean)+(0.1*y_range)
    
    # Creates plot object using plot_data.
    # Data is plotted as symbols with connected lines.
    p1 <- ggplot(data=plot_data) +
      geom_line(aes(x=Year, y=Mean), color=color_palette[1],
                size=0.75, alpha=1) +
      geom_point(aes(x=Year, y=Mean), fill=color_palette[1],
                 shape=21, size=2, color="#333333", alpha=1) +
      labs(title="Grazers and Reef-Dependent Species Richness",
           subtitle=ma_i,
           x="Year", y="Annual average richness (# of species)") +
      scale_x_continuous(limits=c(t_min-0.25, t_max+0.25),
                         breaks=seq(t_max, t_min, brk)) +
      scale_y_continuous(limits=c(y_min, y_max),
                         breaks=pretty_breaks(n=5)) +
      plot_theme
    # Sets file name of plot created
    outname <- paste0("Coral_", param_file, "_", ma_abrev, ".png")
    # Saves plot as a png image
    png(paste0(out_dir, "/Figures/", outname),
        width = 8,
        height = 4,
        units = "in",
        res = 200)
    print(p1)
    dev.off()
    
    # Creates a data table object to be shown underneath plots in report
    ResultTable <- MA_Ov_Stats[ManagedAreaName==ma_i,]
    # Removes location, and parameter information because it is in plot
    # labels
    ResultTable <- ResultTable[,-c("AreaID", "ManagedAreaName",
                                   "ProgramIDs", "Programs", "ParameterName")]
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
zip("CoralSpeciesRichnessFigures", files=fig_list)
setwd(wd)

## Coral Percent Cover ----
# Use original copy of Coral data made earlier
data <- copy(data2)
# Set seed
seed <- 42
#Set output directory
out_dir <- "output/PercentCover"
# Sets coastal wetland file to only care about "All Parameters" file
param_name <- "PercentCover"
#Sets abbreviation or label to be used in file names
param_file <- "PC"

# Filtering ----
# Only keep data for Percent Cover
# Formerly "Percent Cover - Species Composition"
data <- data[ParameterName=="Percent Cover", ]

# Sets units for percent cover
unit <- "%"
data$ParameterUnits <- unit

# Remove any rows that are not corals
data <- data[SpeciesGroup1 %in% c("Octocorals","Milleporans","Scleractinians"), ]
# Remove rows with missing GenusName
data <- data[!is.na(data$GenusName),]
# Remove rows with missing SpeciesName
data <- data[!is.na(data$SpeciesName),]
# Remove rows with missing Months
data <- data[!is.na(data$Month),]
# Remove rows with missing Years
data <- data[!is.na(data$Year),]
# Remove rows with missing SpeciesGroup1
data <- data[!is.na(data$SpeciesGroup1),]
# Remove rows with missing ResultValue
data <- data[!is.na(data$ResultValue),]
# Remove rows with missing SampleDate
data <- data[!is.na(data$SampleDate),]
# Remove duplicate rows
# data <- data[data$MADup==1,]
# Remove rows with missing ManagedAreaName
data <- data[!is.na(data$ManagedAreaName),]
# Create variable that combines the genus and species name
data$gensp <- paste(data$GenusName, data$SpeciesName, sep=" ")

# Managed Area Stats ----
# Create summary statistics for each managed area based on Year and Month
# intervals.
MA_YM_Stats <- data %>%
  group_by(AreaID, ManagedAreaName, Year, Month) %>%
  summarize(ParameterName=parameter,
            N_Data=length(na.omit(ResultValue)),
            Min=min(ResultValue),
            Max=max(ResultValue),
            Median=median(ResultValue),
            Mean=mean(ResultValue),
            StandardDeviation=sd(ResultValue),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '),
            .groups = "keep")
# Puts the data in order based on ManagedAreaName, Year, then Month
MA_YM_Stats <- as.data.table(MA_YM_Stats[order(MA_YM_Stats$ManagedAreaName,
                                               MA_YM_Stats$Year,
                                               MA_YM_Stats$Month), ])
# Writes summary statistics to file
fwrite(MA_YM_Stats, paste0(out_dir,"/Coral_", param_file,
                           "_MA_MMYY_Stats.txt"), sep="|")
# Removes variable storing data to improve computer memory
rm(MA_YM_Stats)

# Create summary statistics for each managed area based on Year intervals
MA_Y_Stats <- data %>%
  group_by(AreaID, ManagedAreaName, Year) %>%
  summarize(ParameterName=parameter,
            N_Data=length(na.omit(ResultValue)),
            Min=min(ResultValue),
            Max=max(ResultValue),
            Median=median(ResultValue),
            Mean=mean(ResultValue),
            StandardDeviation=sd(ResultValue),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '),
            .groups = "keep")
# Puts the data in order based on ManagedAreaName then Year
MA_Y_Stats <- as.data.table(MA_Y_Stats[order(MA_Y_Stats$ManagedAreaName,
                                             MA_Y_Stats$Year), ])
# Writes summary statistics to file
fwrite(MA_Y_Stats, paste0(out_dir,"/Coral_", param_file,
                          "_MA_Yr_Stats.txt"), sep="|")

# Create summary statistics for each managed area based on Month intervals.
MA_M_Stats <- data %>%
  group_by(AreaID, ManagedAreaName, Month) %>%
  summarize(ParameterName=parameter,
            N_Data=length(na.omit(ResultValue)),
            Min=min(ResultValue),
            Max=max(ResultValue),
            Median=median(ResultValue),
            Mean=mean(ResultValue),
            StandardDeviation=sd(ResultValue),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '),
            .groups = "keep")
# Puts the data in order based on ManagedAreaName then Month
MA_M_Stats <- as.data.table(MA_M_Stats[order(MA_M_Stats$ManagedAreaName,
                                             MA_M_Stats$Month), ])
# Writes summary statistics to file
fwrite(MA_M_Stats, paste0(out_dir,"/Coral_", param_file,
                          "_MA_Mo_Stats.txt"), sep="|")
# Removes variable storing data to improve computer memory
rm(MA_M_Stats)

# Create summary overall statistics for each managed area.
MA_Ov_Stats <- data %>%
  group_by(AreaID, ManagedAreaName) %>%
  summarize(ParameterName=parameter,
            N_Years=length(unique(na.omit(Year))),
            SufficientData=ifelse(N_Years>=5, TRUE, FALSE),
            EarliestYear=min(Year),
            LatestYear=max(Year),
            N_Data=length(na.omit(ResultValue)),
            Min=min(ResultValue),
            Max=max(ResultValue),
            Median=median(ResultValue),
            Mean=mean(ResultValue),
            StandardDeviation=sd(ResultValue),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '),
            .groups = "keep")
# Puts the data in order based on ManagedAreaName
MA_Ov_Stats <- as.data.table(MA_Ov_Stats[order(MA_Ov_Stats$ManagedAreaName), ])

# Replaces blank ProgramIDs with NA (missing values)
MA_Ov_Stats$ProgramIDs <- replace(MA_Ov_Stats$ProgramIDs,
                                  MA_Ov_Stats$ProgramIDs=="", NA)
MA_Ov_Stats$Programs <- replace(MA_Ov_Stats$Programs,
                                MA_Ov_Stats$Programs=="", NA)
# Write overall statistics to file
fwrite(MA_Ov_Stats, paste0(out_dir,"/Coral_", param_file,
                           "_MA_Overall_Stats.txt"), sep="|")
# Creates a variable with the names of all the managed areas that contain
# species observations
coral_pc_MA_Include <- MA_Ov_Stats[!is.na(Mean) & SufficientData==TRUE, unique(ManagedAreaName)]
coral_pc_MA_All <- MA_Ov_Stats[!is.na(Mean), unique(ManagedAreaName)]

# Puts the managed areas in alphabetical order
coral_pc_MA_Include <- coral_pc_MA_Include[order(coral_pc_MA_Include)]

# Determines the number of managed areas used
n <- length(coral_pc_MA_Include)

# LME ----
# Creates blank data frame with number of rows defined by how many managed areas
# are going to be analyzed
lme_stats <- data.frame(matrix(ncol = 5, nrow = n))
# Sets column names for blank data frame
colnames(lme_stats) <- c("AreaID", "ManagedAreaName", "LME_Intercept",
                         "LME_Slope", "LME_p")

# Begins to loop through each managed area for analysis (which have enough data)
for(i in 1:n){
  ma_i <- coral_pc_MA_Include[i]
  # Gets data for current managegd area
  lme_data <- data[ManagedAreaName==ma_i,]
  # Perform LME for relation between ResultValue and Year for current managed area
  AnyCoral<-lme(ResultValue ~ Year,
                random =~1|ProgramLocationID,
                na.action = na.omit,
                data = lme_data)
  # Store information and model fits in appropriate row of data frame
  lme_stats$AreaID[i] <- unique(lme_data$AreaID)
  lme_stats$ManagedAreaName[i] <- ma_i
  lme_stats$LME_Intercept[i] <- AnyCoral$coefficients$fixed[1]
  lme_stats$LME_Slope[i] <- AnyCoral$coefficients$fixed[2]
  lme_stats$LME_p[i] <- anova(AnyCoral)$p[2]
  
  # Clears temporary variables for memory
  rm(lme_data)
  (AnyCoral)
}

# Merges LME stats with overall stats to complete stats for each managed area
lme_stats <- merge.data.frame(MA_Ov_Stats[,-c("Programs", "ProgramIDs")],
                              lme_stats, by=c("AreaID", "ManagedAreaName"), all=TRUE)

# Puts the data in order based on ManagedAreaName
lme_stats <- as.data.table(lme_stats[order(lme_stats$ManagedAreaName), ])

# Write lme statistics to file
fwrite(lme_stats, paste0(out_dir,"/Coral_", param_file,
                         "_LME_Stats.txt"), sep="|")

# Gets lower x and y values based on LME fit to use in plot
lme_plot <- lme_stats %>%
  group_by(AreaID, ManagedAreaName) %>%
  summarize(x=EarliestYear,
            y=LME_Slope*x+LME_Intercept, .groups = "keep")
# Gets upper x and y values based on LME fit to use in plot
lme_plot2 <- lme_stats %>%
  group_by(AreaID, ManagedAreaName) %>%
  summarize(x=LatestYear,
            y=LME_Slope*x+LME_Intercept, .groups = "keep")
# Merges LME fit values for plot into one data frame
lme_plot <- bind_rows(lme_plot, lme_plot2)
rm(lme_plot2)
# Puts LME plot data fram in alphabetical order by managed area
lme_plot <- as.data.frame(lme_plot[order(lme_plot$ManagedAreaName), ])
lme_plot <- setDT(lme_plot[!is.na(lme_plot$y),])

# Trendlines_ManagedArea ----
# Create jitter object that sets the height and width
# Sets seed to be reproducible
# plot_jitter <- position_jitter(width = 0.2, height = 0.2, seed=seed)
plot_jitter <- position_jitter(width = 0.2, height = 0, seed=seed)

# Loop that cycles through each managed area with data
if(n==0){
  # Prints a statement if there are no managed areas with appropriate data
  print("There are no locations that qualify.")
} else {
  for (i in 1:length(coral_pc_MA_All)) { 
    ma_i <- coral_pc_MA_All[i]
    # Get abbreviated name for filename
    ma_abrev <- MA_All[ManagedAreaName==ma_i, Abbreviation]
    # Gets data for target managed area
    plot_data <- data[ManagedAreaName==ma_i,]
    if(ma_i %in% coral_pc_MA_Include){
      lme_plot_data <- lme_plot[ManagedAreaName==ma_i,]
    }
    
    # Determines most recent year with available data for managed area
    maxyr <- max(MA_Ov_Stats[ManagedAreaName==ma_i, LatestYear])
    # Determines earliest recent year with available data for managed area
    minyr <- min(MA_Ov_Stats[ManagedAreaName==ma_i, EarliestYear])
    # Determines how many years of data are present
    nyrs <- maxyr-minyr+1
    
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    
    # Creates break intervals for plots based on number of years of data
    if(nyrs>=30){
      # Set breaks to every 10 years if more than 30 years of data
      brk <- 10
    }else if(nyrs>=10){
      # Set breaks to every 5 years if between 30 and 10 years of data
      brk <- 5
    }else if(nyrs>=5){
      # Set breaks to every 2 years if between 10 and 5 years of data
      brk <- 2
    }else{
      # Ensure 5 years are included on axis
      total_ticks <- 5
      extra_years <- total_ticks - nyrs
      # Always add 1 year before the first year
      years_before <- min(1, extra_years)
      years_after <- extra_years - years_before
      # Adjust min and max year, without going beyond current year
      minyr <- minyr - years_before
      maxyr <- min(maxyr + years_after, current_year)
      # Re-check if we have enough years (in case maxyr hit current year)
      minyr <- max(minyr, maxyr - (total_ticks - 1))
      brk <- 1
    }
    # Determine range of data values for the managed area
    y_range <- max(plot_data$ResultValue) - min(plot_data$ResultValue)
    
    # Sets y_min
    if(ma_i %in% coral_pc_MA_Include){
      # y_min <- min(min(lme_plot_data$y), min(plot_data$ResultValue))
      y_min <- min(min(lme_plot_data$y), 0)
    } else {
      y_min <- 0
    }
    
    # Sets upper bound of y-axis to be 10% of the data range above the
    # maximum value, unless both min and max are 0.
    if(max(plot_data$ResultValue)==0 & y_min==0){
      y_max <- max(plot_data$ResultValue)+1
    } else {
      y_max <- max(plot_data$ResultValue)+(0.1*y_range)
    }
    
    # Creates plot object using plot_data.
    # Data is plotted as a point pot with jitter to show concentrations
    # that overlap. LME fit is plotted as a line
    p1 <- ggplot(data=plot_data) +
      geom_point(aes(x=Year, y=ResultValue), 
                 position=plot_jitter, shape=21, size=2,
                 color="#333333", fill="#cccccc", alpha=1) +
      {if(ma_i %in% coral_pc_MA_Include){
        geom_line(data=lme_plot_data, aes(x=x, y=y),
                  color="#000099", size=1.2, alpha=0.7)
      }} +
      labs(title="Coral Percent Cover",
           subtitle=ma_i,
           x="Year", y="Percent cover (%)") +
      scale_x_continuous(limits = c(minyr-0.25, maxyr+0.25),
                         breaks = seq(minyr, maxyr, brk)) +
      scale_y_continuous(limits=c(y_min, y_max),
                         breaks=pretty_breaks(n=5)) +
      plot_theme
    # Sets file name of plot created
    outname <- paste0("Coral_", param_file, "_", ma_abrev, ".png")
    # Saves plot as a png image
    png(paste0(out_dir, "/Figures/", outname),
        width = 8,
        height = 4,
        units = "in",
        res = 200)
    print(p1)
    dev.off()
    
    # Creates a data table object to be shown underneath plots in report
    ResultTable <- lme_stats[ManagedAreaName==ma_i,]
    # Removes location, and parameter information because it is in plot
    # labels
    ResultTable <- select(ResultTable, -c("AreaID", "ManagedAreaName",
                                          "ParameterName"))
    # Renames StandardDeviation to StDev to save horizontal space
    ResultTable <- ResultTable %>%
      rename("StDev"="StandardDeviation")
    # Converts all non-integer values to 2 decimal places for space
    ResultTable$Min <- round(ResultTable$Min, digits=2)
    ResultTable$Max <- round(ResultTable$Max, digits=2)
    ResultTable$Median <- round(ResultTable$Median, digits=2)
    ResultTable$Mean <- round(ResultTable$Mean, digits=2)
    ResultTable$StDev <- round(ResultTable$StDev, digits=2)
    ResultTable$LME_Intercept <- round(ResultTable$LME_Intercept, digits=2)
    ResultTable$LME_Slope <- round(ResultTable$LME_Slope, digits=2)
    ResultTable$LME_p <- round(ResultTable$LME_p, digits=4)
    # Stores as plot table object
    t1 <- ggtexttable(ResultTable, rows = NULL,
                      theme=ttheme(base_size=7)) %>%
      tab_add_footnote(text="LME_p < 0.00005 appear as 0 due to rounding.",
                       size=10, face="italic")
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
zip("CoralPCFigures", files=fig_list)
setwd(wd)

if(create_maps){
  source("Coral_Create_Maps.R")
}

# Render both reports
report_types <- c("SpeciesRichness","PercentCover")
for(file_type in c("HTML", "PDF")){
  descriptionColumn <- ifelse(file_type=="PDF", "DescriptionLatex", "DescriptionHTML")
  tableFormat <- ifelse(file_type=="PDF", "latex", "simple")
  for(report_type in report_types){
    file_out <-  paste0("Coral_", report_type, "_Report")
    template <- ifelse(report_type=="SpeciesRichness", 
                       "Coral_SpeciesRichness.Rmd", "Coral_PC.Rmd")
    rmarkdown::render(input = template, 
                      output_format = paste0(tolower(file_type),"_document"),
                      output_file = paste0(file_out, ".", tolower(file_type)),
                      output_dir = paste0("output/", report_type),
                      clean=TRUE)
    #Removes unwanted files created in the rendering process
    unlink(paste0(out_dir, "/", file_out, ".md"))
    unlink(paste0(out_dir, "/", file_out, "_files"), recursive=TRUE)
    unlink(paste0(file_out, ".log"))
  }
}
