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
library(knitr)
library(kableExtra)
library(glue)
library(patchwork)
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

#Sets abbreviation or label to be used in file names
param_file <- "SpeciesRichness"

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- SEACAR::ManagedAreas

# Load in figure captions (for use in)
figureCaptions <- SEACAR::FigureCaptions

#Gets the files with the file names containing the desired parameter
file_in <- list.files(seacar_data_location, pattern="All_CORAL", full=TRUE)

#Gets the specific file used and removes the directory names
file_short <- tail(str_split(file_in, "/")[[1]], 1)

# Read in data file
data <- fread(file_in, sep="|", header=TRUE, stringsAsFactors=FALSE, na.strings="NULL")
# Apply Managed Area transformation - de-concatenate MA names
data <- setDT(SEACAR::clean_managed_areas(data, "ma", keep_na = FALSE))

cat(paste("The data file(s) used:", file_short, sep="\n"))

# # Make a copy of main data file to save for Coral Percent Cover
# data2 <- copy(data)
# coral_pc_data <- copy(data) # Save copy for use with maps (Percent Cover)

## Data Filtering ----
# Only keep data for Presence of grazers and reef-dependent species
data_sr <- data[ParameterName=="Presence/Absence" & 
                  SpeciesGroup1 %in% c("Grazers and reef dependent species", 
                                       "Reef fish"), ]
# coral_sr_data <- copy(data) # Save copy for use with maps (Species Richness)

# Rename P/A to Species Richness
data_sr$ParameterName <- "Species Richness"
parameter <- "Species Richness"
title_param <- "Species Richness - Grazers and Reef-Dependent Species"

# Sets units for species richness
unit <- "# of species"
data_sr$ParameterUnits <- unit

# Remove rows with missing ManagedAreaName, GenusName, SpeciesName, Months, Years, SpeciesGroup1
data_sr <- data_sr[!is.na(ManagedAreaName) & ManagedAreaName!="NA" & !is.na(GenusName) & 
                     !is.na(SpeciesName) & !is.na(Month) & !is.na(Year) & !is.na(SpeciesGroup1)]
# Remove rows where ResultValue is 0 and missing
data_sr <- data_sr[ResultValue!=0 & !is.na(ResultValue)]

# Create Species Richness values for groups of unique combinations of
# ManagedAreaName, ProgramID, ProgramName, ProgramLocationID, and SampleDate.
data_sr <- data_sr[ResultValue==1] %>%
  group_by(AreaID, ManagedAreaName, ProgramID, ProgramName, ProgramLocationID,
           SampleDate) %>%
  summarise(ParameterName=parameter,
            Year=unique(Year), Month=unique(Month),
            SpeciesRichness=length(unique(CommonIdentifier))) %>%
  as.data.table()

# Writes this data that is used by the rest of the script to a text file
fwrite(data_sr, paste0(out_dir,"/Coral_", param_file, "_UsedData.txt"), sep="|")

# Makes sure SampleDate is being stored as a Date object
data_sr$SampleDate <- as.Date(data_sr$SampleDate)

# Creates a variable with the names of all the managed areas that contain
# species observations, in alphabetical order
coral_sr_MA_Include <- data_sr[!is.na(SpeciesRichness), unique(ManagedAreaName)] %>% sort()

# Determines the number of managed areas used
n <- length(coral_sr_MA_Include)

## Managed Area Stats ----
# Create summary statistics for each managed area based on Year and Month
# intervals.
MA_YM_Stats <- data_sr %>%
  group_by(AreaID, ManagedAreaName, Year, Month) %>%
  summarize(ParameterName=parameter,
            N_Data=length(na.omit(SpeciesRichness)),
            Min=min(SpeciesRichness),
            Max=max(SpeciesRichness),
            Median=median(SpeciesRichness),
            Mean=mean(SpeciesRichness),
            StandardDeviation=sd(SpeciesRichness),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE), collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE), collapse=', '),
            .groups = "keep") %>%
  arrange(ManagedAreaName, Year, Month) %>%
  as.data.table()
# Writes summary statistics to file
fwrite(MA_YM_Stats, paste0(out_dir,"/Coral_", param_file, "_MA_MMYY_Stats.txt"), sep="|")
# Removes variable storing data to improve computer memory
rm(MA_YM_Stats)

# Create summary statistics for each managed area based on Year intervals
MA_Y_Stats <- data_sr %>%
  group_by(AreaID, ManagedAreaName, Year) %>%
  summarize(ParameterName=parameter,
            N_Data=length(na.omit(SpeciesRichness)),
            Min=min(SpeciesRichness),
            Max=max(SpeciesRichness),
            Median=median(SpeciesRichness),
            Mean=mean(SpeciesRichness),
            StandardDeviation=sd(SpeciesRichness),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE), collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE), collapse=', '),
            .groups = "keep") %>%
  arrange(ManagedAreaName, Year) %>%
  as.data.table()
# Writes summary statistics to file
fwrite(MA_Y_Stats, paste0(out_dir,"/Coral_", param_file, "_MA_Yr_Stats.txt"), sep="|")

# Create summary statistics for each managed area based on Month intervals.
MA_M_Stats <- data_sr %>%
  group_by(AreaID, ManagedAreaName, Month) %>%
  summarize(ParameterName=parameter,
            N_Data=length(na.omit(SpeciesRichness)),
            Min=min(SpeciesRichness),
            Max=max(SpeciesRichness),
            Median=median(SpeciesRichness),
            Mean=mean(SpeciesRichness),
            StandardDeviation=sd(SpeciesRichness),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE), collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE), collapse=', '),
            .groups = "keep") %>%
  arrange(ManagedAreaName, Month) %>%
  as.data.table()
# Writes summary statistics to file
fwrite(MA_M_Stats, paste0(out_dir,"/Coral_", param_file, "_MA_Mo_Stats.txt"), sep="|")
# Removes variable storing data to improve computer memory
rm(MA_M_Stats)

# Create summary overall statistics for each managed area.
MA_Ov_Stats <- data_sr %>%
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
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE), collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE), collapse=', '),
            Year_MinRichness = NA,
            Year_MaxRichness = NA,
            .groups = "keep") %>% 
  arrange(ManagedAreaName) %>%
  as.data.table()

# Loops through each ManagedAreaName.
# Determines what year the minimum and maximum species richness occurred
for(m in 1:nrow(MA_Ov_Stats)){
  # Stores ManagedAreaName for this row
  ma <- MA_Ov_Stats$ManagedAreaName[m]
  
  # Skips to next row if there are no data for this combination
  if(MA_Ov_Stats$N_Data[m]==0){
    next
  }
  # Gets the minimum and maximum Mean (yearly averages)
  min <- MA_Y_Stats[ManagedAreaName==ma, min(Mean)]
  max <- MA_Y_Stats[ManagedAreaName==ma, max(Mean)]
  #Determines what years those minimum and maximum values occured
  year_min <- MA_Y_Stats[ManagedAreaName==ma & Mean == min, Year]
  year_max <- MA_Y_Stats[ManagedAreaName==ma & Mean == max, Year]
  # Stores the occurrence years of the minimum and maximum into the overall
  # stats for this row
  MA_Ov_Stats$Year_MinRichness[m] <- year_min
  MA_Ov_Stats$Year_MaxRichness[m] <- year_max
}
# Write overall statistics to file
fwrite(MA_Ov_Stats, paste0(out_dir,"/Coral_", param_file, "_MA_Overall_Stats.txt"), sep="|")
# Removes entries from the overall statistics that do not have data.
# Based on presence or absence of EarliestYear
MA_Ov_Stats <- MA_Ov_Stats[!is.na(MA_Ov_Stats$EarliestYear), ]

## Plot Species Richness ----
# Color palette for SEACAR
color_palette <- SEACAR::seacar_palette1

# Remove previous plots before rendering new plots
plot_files <- c(list.files("output/PercentCover/Figures", full=T),
                list.files("output/SpeciesRichness/Figures", full=T),
                list.files("output/CommunityComposition/Figures", full=T))
file.remove(plot_files)

# Loop that cycles through each managed area with data
if(n==0){
  # Prints a statement if there are no managed areas with appropriate data
  print("There are no monitoring locations that qualify.")
} else {
  for(i in 1:n){
    ma <- coral_sr_MA_Include[i]
    # Get abbreviated name for filename
    ma_abrev <- MA_All[ManagedAreaName==ma, Abbreviation]
    # Gets data for target managed area
    plot_data <- MA_Y_Stats[ManagedAreaName==ma, ]
    # Determines most recent year with available data for managed area
    t_max <- MA_Ov_Stats[ManagedAreaName==ma, max(LatestYear)]
    # Determines earliest recent year with available data for managed area
    t_min <- MA_Ov_Stats[ManagedAreaName==ma, min(EarliestYear)]
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
                linewidth=0.75, alpha=1) +
      geom_point(aes(x=Year, y=Mean), fill=color_palette[1],
                 shape=21, size=2, color="#333333", alpha=1) +
      labs(title="Grazers and Reef-Dependent Species Richness",
           subtitle=ma,
           x="Year", y="Annual average richness (# of species)") +
      scale_x_continuous(limits=c(t_min-0.25, t_max+0.25),
                         breaks=seq(t_max, t_min, brk)) +
      scale_y_continuous(limits=c(y_min, y_max),
                         breaks=pretty_breaks(n=5)) +
      SEACAR::SEACAR_plot_theme()
    # Sets file name of plot created
    outname <- paste0("Coral_", param_file, "_", ma_abrev, ".png")
    # Saves plot as a png image
    ggsave(filename = paste0(out_dir, "/Figures/", outname), 
           plot = p1,
           width = 8,
           height = 4,
           units = "in")
  }
}

#Gets list of all image files in output/Figures and creates zip directory
fig_list <- list.files(paste0(out_dir, "/Figures"), pattern=".png", full=FALSE)
setwd(paste0(out_dir, "/Figures"))
zip("CoralSpeciesRichnessFigures", files=fig_list)
setwd(wd)

## Coral Percent Cover ----
# Set seed
seed <- 42
#Set output directory
out_dir <- "output/PercentCover"
#Sets abbreviation or label to be used in file names
param_file <- "PC"

# Filtering ----
# Only keep data for Percent Cover
# Formerly "Percent Cover - Species Composition"
data_pc <- data[ParameterName=="Percent Cover", ]

# Sets units for percent cover
unit <- "%"
data_pc$ParameterUnits <- unit

# Remove any rows that are not corals, missing GenusName, SpeciesName, Months, Years, 
# SpeciesGroup1, ResultValue, SampleDate, ManagedAreaName
data_pc <- data_pc[SpeciesGroup1 %in% c("Octocorals","Milleporans","Scleractinians") & 
                     !is.na(GenusName) & !is.na(SpeciesName) & !is.na(Month) & 
                     !is.na(Year) & !is.na(SpeciesGroup1) & !is.na(ResultValue) & 
                     !is.na(SampleDate) & !is.na(ManagedAreaName)]

# # Writes this data that is used by the rest of the script to a text file (too large to share)
# fwrite(data_pc, paste0(out_dir,"/Coral_", param_file, "_UsedData.txt"), sep="|")

# Managed Area Stats ----
# Create summary statistics for each managed area based on Year and Month
# intervals.
MA_YM_Stats <- data_pc %>%
  group_by(AreaID, ManagedAreaName, Year, Month) %>%
  summarize(ParameterName=parameter,
            N_Data=length(na.omit(ResultValue)),
            Min=min(ResultValue),
            Max=max(ResultValue),
            Median=median(ResultValue),
            Mean=mean(ResultValue),
            StandardDeviation=sd(ResultValue),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE), collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE), collapse=', '),
            .groups = "keep") %>%
  arrange(ManagedAreaName, Year, Month) %>%
  as.data.table()
# Writes summary statistics to file
fwrite(MA_YM_Stats, paste0(out_dir,"/Coral_", param_file, "_MA_MMYY_Stats.txt"), sep="|")
# Removes variable storing data to improve computer memory
rm(MA_YM_Stats)

# Create summary statistics for each managed area based on Year intervals
MA_Y_Stats <- data_pc %>%
  group_by(AreaID, ManagedAreaName, Year) %>%
  summarize(ParameterName=parameter,
            N_Data=length(na.omit(ResultValue)),
            Min=min(ResultValue),
            Max=max(ResultValue),
            Median=median(ResultValue),
            Mean=mean(ResultValue),
            StandardDeviation=sd(ResultValue),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE), collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE), collapse=', '),
            .groups = "keep") %>%
  arrange(ManagedAreaName, Year) %>%
  as.data.table()
# Writes summary statistics to file
fwrite(MA_Y_Stats, paste0(out_dir,"/Coral_", param_file, "_MA_Yr_Stats.txt"), sep="|")

# Create summary statistics for each managed area based on Month intervals.
MA_M_Stats <- data_pc %>%
  group_by(AreaID, ManagedAreaName, Month) %>%
  summarize(ParameterName=parameter,
            N_Data=length(na.omit(ResultValue)),
            Min=min(ResultValue),
            Max=max(ResultValue),
            Median=median(ResultValue),
            Mean=mean(ResultValue),
            StandardDeviation=sd(ResultValue),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE), collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE), collapse=', '),
            .groups = "keep") %>%
  arrange(ManagedAreaName, Month) %>%
  as.data.table()
# Writes summary statistics to file
fwrite(MA_M_Stats, paste0(out_dir,"/Coral_", param_file, "_MA_Mo_Stats.txt"), sep="|")
# Removes variable storing data to improve computer memory
rm(MA_M_Stats)

# Create summary overall statistics for each managed area.
MA_Ov_Stats <- data_pc %>%
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
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE), collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE), collapse=', '),
            .groups = "keep") %>%
  arrange(ManagedAreaName) %>%
  as.data.table()

# Write overall statistics to file
fwrite(MA_Ov_Stats, paste0(out_dir,"/Coral_", param_file, "_MA_Overall_Stats.txt"), sep="|")
# Creates a variable with the names of all the managed areas that contain
# species observations
coral_pc_MA_Include <- MA_Ov_Stats[!is.na(Mean) & SufficientData==TRUE, sort(unique(ManagedAreaName))]
coral_pc_MA_All <- MA_Ov_Stats[!is.na(Mean), sort(unique(ManagedAreaName))]

# Determines the number of managed areas used
n <- length(coral_pc_MA_Include)

# LME ----
# Creates blank data frame with number of rows defined by how many managed areas
# are going to be analyzed
lme_stats <- data.frame(matrix(ncol = 5, nrow = n))
# Sets column names for blank data frame
colnames(lme_stats) <- c("AreaID", "ManagedAreaName", "LME_Intercept", "LME_Slope", "LME_p")

# Begins to loop through each managed area for analysis (which have enough data)
for(i in 1:n){
  ma <- coral_pc_MA_Include[i]
  # Gets data for current managegd area
  lme_data <- data_pc[ManagedAreaName==ma,]
  # params
  pars <- lmeControl(
    maxIter = 200,
    msMaxIter = 500,
    msMaxEval = 1000,
    tolerance = 1e-6,
    msTol = 1e-7
  )
  # Perform LME for relation between ResultValue and Year for current managed area
  fit <- try(
    nlme::lme(
      ResultValue ~ Year,
      random = ~1 | LocationID,
      na.action = na.omit,
      data = lme_data,
      control = pars
    ),
    silent = TRUE
  )
  
  if(inherits(fit, "try-error")) {
    message("LME failed for: ", ma)
    lme_stats$AreaID[i] <- unique(lme_data$AreaID)
    lme_stats$ManagedAreaName[i] <- ma
    lme_stats$LME_Intercept[i] <- NA
    lme_stats$LME_Slope[i] <- NA
    lme_stats$LME_p[i] <- NA
    next
  }
  
  # Store information and model fits in appropriate row of data frame
  lme_stats$AreaID[i] <- unique(lme_data$AreaID)
  lme_stats$ManagedAreaName[i] <- ma
  lme_stats$LME_Intercept[i] <- fit$coefficients$fixed[1]
  lme_stats$LME_Slope[i] <- fit$coefficients$fixed[2]
  lme_stats$LME_p[i] <- anova(fit)$p[2]
  
  # Clears temporary variables for memory
  rm(lme_data)
  rm(fit)
}

# Merges LME stats with overall stats to complete stats for each managed area
lme_stats <- merge.data.frame(MA_Ov_Stats[,-c("Programs", "ProgramIDs")],
                              lme_stats, by=c("AreaID", "ManagedAreaName"), all=TRUE)

## Add in TrendText and TrendIcon columns
lme_stats <- lme_stats %>% 
  mutate(TrendText = ifelse(LME_p <= 0.05 & LME_Slope > 0, "Increasing trend",
                            ifelse(LME_p <= 0.05 & LME_Slope < 0, "Decreasing trend", "No detectable trend")),
         TrendIcon = ifelse(LME_p <= 0.05 & LME_Slope > 0, 1,
                            ifelse(LME_p <= 0.05 & LME_Slope < 0, -1, 0))) %>%
  arrange(ManagedAreaName) %>% 
  as.data.table()

#filling remaining values in TrendText + TrendIcon column
lme_stats[SufficientData==FALSE, TrendText := "Insufficient data"]
lme_stats[SufficientData==FALSE, TrendIcon := 2]
lme_stats[SufficientData==TRUE & is.na(LME_Slope), TrendText := "Model did not fit the available data"]
lme_stats[SufficientData==TRUE & is.na(LME_Slope), TrendIcon := 3]

# Write lme statistics to file
fwrite(lme_stats, paste0(out_dir,"/Coral_", param_file, "_LME_Stats.txt"), sep="|")

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
  for(i in 1:length(coral_pc_MA_All)){ 
    ma <- coral_pc_MA_All[i]
    # Get abbreviated name for filename
    ma_abrev <- MA_All[ManagedAreaName==ma, Abbreviation]
    # Gets data for target managed area
    plot_data <- data_pc[ManagedAreaName==ma]
    if(ma %in% coral_pc_MA_Include){
      lme_plot_data <- lme_plot[ManagedAreaName==ma,]
    }
    
    # Determines most recent year with available data for managed area
    maxyr <- MA_Ov_Stats[ManagedAreaName==ma, max(LatestYear)]
    # Determines earliest recent year with available data for managed area
    minyr <- MA_Ov_Stats[ManagedAreaName==ma, min(EarliestYear)]
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
    } else {
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
    if(ma %in% coral_pc_MA_Include){
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
      {if(ma %in% coral_pc_MA_Include){
        geom_line(data=lme_plot_data, aes(x=x, y=y),
                  color="#000099", linewidth=1.2, alpha=0.7)
      }} +
      labs(title="Coral Percent Cover",
           subtitle=ma,
           x="Year", y="Percent cover (%)") +
      scale_x_continuous(limits = c(minyr-0.25, maxyr+0.25),
                         breaks = seq(minyr, maxyr, brk)) +
      scale_y_continuous(limits=c(y_min, y_max),
                         breaks=pretty_breaks(n=5)) +
      SEACAR::SEACAR_plot_theme()
    # Sets file name of plot created
    outname <- paste0("Coral_", param_file, "_", ma_abrev, ".png")
    # Saves plot as a png image
    ggsave(filename = paste0(out_dir, "/Figures/", outname), 
           plot = p1,
           width = 8,
           height = 4,
           units = "in")
  }
}

#Gets list of all PC image files and creates zip directory
fig_list <- list.files(paste0(out_dir, "/Figures"), pattern=".png", full=FALSE)
setwd(paste0(out_dir, "/Figures"))
zip("CoralPCFigures", files=fig_list)
setwd(wd)

## Coral Community Composition ----
parameter <- "Species Richness (Community Composition)"
param_file <- "CC"
out_dir <- "output/CommunityComposition"
# Coral species groups included in community composition
comp_species_groups <- c("Corallimorpharians", "Cyanobacteria", "Macroalgae", 
                         "Milleporans", "Octocorals", "Porifera", "Scleractinians", 
                         "Zoanthids", "Others")
# Programs to exclude (ID_981 only reports scleractinians)
exclude_progs <- c(981)
# Filter for the presence of those species groups for each area and LocationID
# This gives us species richness values
data_cc <- data %>% 
  filter(
    ParameterName=="Presence/Absence",
    ResultValue==1,
    SpeciesGroup1 %in% comp_species_groups,
    !ProgramID %in% exclude_progs
  ) %>%
  group_by(AreaID, ManagedAreaName, ProgramID, ProgramName, LocationID,
           SampleDate, SpeciesGroup1) %>%
  reframe(
    ParameterName = parameter,
    Year = unique(Year),
    Month = unique(Month),
    SpeciesRichness = length(unique(CommonIdentifier))
  )

## Managed Area Stats ----
# Create summary statistics for each managed area based on Year and Month
# intervals.
MA_YM_Stats <- data_cc %>%
  group_by(AreaID, ManagedAreaName, Year, Month) %>%
  summarize(ParameterName=parameter,
            N_Data=length(na.omit(SpeciesRichness)),
            Min=min(SpeciesRichness),
            Max=max(SpeciesRichness),
            Median=median(SpeciesRichness),
            Mean=mean(SpeciesRichness),
            StandardDeviation=sd(SpeciesRichness),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE), collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE), collapse=', '),
            IncludedSpeciesGroups = paste(sort(unique(SpeciesGroup1), decreasing=FALSE), collapse=', '),
            .groups = "keep") %>%
  arrange(ManagedAreaName, Year, Month) %>%
  as.data.table()
# Writes summary statistics to file
fwrite(MA_YM_Stats, paste0(out_dir,"/Coral_", param_file, "_MA_MMYY_Stats.txt"), sep="|")
# Removes variable storing data to improve computer memory
rm(MA_YM_Stats)

# Create summary statistics for each managed area based on Year intervals
MA_Y_Stats <- data_cc %>%
  group_by(AreaID, ManagedAreaName, Year) %>%
  summarize(ParameterName=parameter,
            N_Data=length(na.omit(SpeciesRichness)),
            Min=min(SpeciesRichness),
            Max=max(SpeciesRichness),
            Median=median(SpeciesRichness),
            Mean=mean(SpeciesRichness),
            StandardDeviation=sd(SpeciesRichness),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE), collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE), collapse=', '),
            IncludedSpeciesGroups = paste(sort(unique(SpeciesGroup1), decreasing=FALSE), collapse=', '),
            .groups = "keep") %>%
  arrange(ManagedAreaName, Year) %>%
  as.data.table()
# Writes summary statistics to file
fwrite(MA_Y_Stats, paste0(out_dir,"/Coral_", param_file, "_MA_Yr_Stats.txt"), sep="|")

# Create summary statistics for each managed area based on Month intervals.
MA_M_Stats <- data_cc %>%
  group_by(AreaID, ManagedAreaName, Month) %>%
  summarize(ParameterName=parameter,
            N_Data=length(na.omit(SpeciesRichness)),
            Min=min(SpeciesRichness),
            Max=max(SpeciesRichness),
            Median=median(SpeciesRichness),
            Mean=mean(SpeciesRichness),
            StandardDeviation=sd(SpeciesRichness),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE), collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE), collapse=', '),
            IncludedSpeciesGroups = paste(sort(unique(SpeciesGroup1), decreasing=FALSE), collapse=', '),
            .groups = "keep") %>%
  arrange(ManagedAreaName, Month) %>%
  as.data.table()
# Writes summary statistics to file
fwrite(MA_M_Stats, paste0(out_dir,"/Coral_", param_file, "_MA_Mo_Stats.txt"), sep="|")
# Removes variable storing data to improve computer memory
rm(MA_M_Stats)

# Create summary overall statistics for each managed area.
MA_Ov_Stats <- data_cc %>%
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
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE), collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE), collapse=', '),
            IncludedSpeciesGroups = paste(sort(unique(SpeciesGroup1), decreasing=FALSE), collapse=', '),
            Year_MinRichness = NA,
            Year_MaxRichness = NA,
            .groups = "keep") %>% 
  arrange(ManagedAreaName) %>%
  as.data.table()

# Loops through each ManagedAreaName.
# Determines what year the minimum and maximum species richness occurred
for(m in 1:nrow(MA_Ov_Stats)){
  # Stores ManagedAreaName for this row
  ma <- MA_Ov_Stats$ManagedAreaName[m]
  
  # Skips to next row if there are no data for this combination
  if(MA_Ov_Stats$N_Data[m]==0){
    next
  }
  # Gets the minimum and maximum Mean (yearly averages)
  min <- MA_Y_Stats[ManagedAreaName==ma, min(Mean)]
  max <- MA_Y_Stats[ManagedAreaName==ma, max(Mean)]
  #Determines what years those minimum and maximum values occured
  year_min <- MA_Y_Stats[ManagedAreaName==ma & Mean == min, Year]
  year_max <- MA_Y_Stats[ManagedAreaName==ma & Mean == max, Year]
  # Stores the occurrence years of the minimum and maximum into the overall
  # stats for this row
  MA_Ov_Stats$Year_MinRichness[m] <- year_min
  MA_Ov_Stats$Year_MaxRichness[m] <- year_max
}
# Write overall statistics to file
fwrite(MA_Ov_Stats, paste0(out_dir,"/Coral_", param_file, "_MA_Overall_Stats.txt"), sep="|")

### Create plots
# Condense species richness values into annual averages, min/max, etc.
comp_plot_data <- data_cc %>%
  group_by(AreaID, ManagedAreaName, ParameterName, Year, SpeciesGroup1) %>%
  reframe(
    N_Years = length(unique(na.omit(Year))),
    EarliestYear = min(Year),
    LatestYear = max(Year),
    N_Data = length(na.omit(SpeciesRichness)),
    Min = min(SpeciesRichness),
    Max = max(SpeciesRichness),
    Median = median(SpeciesRichness),
    Mean = mean(SpeciesRichness),
    StandardDeviation = sd(SpeciesRichness),
    Programs = paste(sort(unique(ProgramName), decreasing = FALSE), collapse = ", "),
    ProgramIDs = paste(sort(unique(ProgramID), decreasing = FALSE), collapse = ", ")
  ) %>%
  as.data.table()
# Write overall statistics to file
fwrite(comp_plot_data, paste0(out_dir,"/Coral_", param_file, "_UsedData.txt"), sep="|")

# Color palette for SEACAR
color_palette <- SEACAR::seacar_palette1
# All unique SG1 groups
sg1 <- sort(unique(comp_plot_data$SpeciesGroup1))
# Create palette for all SG2 values so it is consistent across all MAs
sg1_palette <- color_palette[seq(1, length(sg1))]
names(sg1_palette) <- sg1

# # # Include common names?
# sg_common <- c("Cephalopods", "Cartilaginous fishes", "Decapod crustaceans", 
#                "Bony fishes", "Other Chordata", "Marine turtles")
# names(sg_common) <- sg1

progs <- data.frame(
  ProgramID = comp_plot_data %>% separate_longer_delim(ProgramIDs, ", ") %>% pull(ProgramIDs) %>% unique(),
  ProgramName = comp_plot_data %>% separate_longer_delim(Programs, ", ") %>% pull(Programs) %>% unique()
)
pids <- unique(progs$ProgramID)
pids_palette <- viridis::viridis(length(pids))
names(pids_palette) <- pids

for(ma in unique(comp_plot_data$ManagedAreaName)){
  ma_abrev <- MA_All[ManagedAreaName==ma, Abbreviation]
  # Gets data for target managed area
  plot_data <- comp_plot_data[ManagedAreaName==ma, ]
  # Determines most recent year with available data for managed area
  maxyr <- max(plot_data$LatestYear)
  # Determines earliest recent year with available data for managed area
  minyr <- min(plot_data$EarliestYear)
  # Determines how many years of data are present
  nyrs <- maxyr-minyr+1
  
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  # Creates break intervals for plots based on number of years of data
  if(nyrs>=30){
    brk <- 10
  } else if(nyrs>=10){
    brk <- 4
  } else if(nyrs>=4){
    brk <- 2
  } else {
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
  
  ## Legend labels - grab list of unique SG1 for this MA
  sp_list <- unique(plot_data$SpeciesGroup1)
  sp_list <- sp_list[order(match(sp_list, names(sg1_palette)))]
  
  # # Create common name labels for legend display
  # sp_labels <- sapply(sp_list, function(x){sg_common[[x]]})
  sp_labels <- sp_list
  
  # Creates plot object using plot_data and grouping by the plot gear types.
  # Data is plotted as symbols with connected lines.
  p1 <- ggplot(data=plot_data, 
               aes(fill = SpeciesGroup1, y=Mean, x=Year)) +
    geom_bar(position="stack", stat="identity", width = 0.9) +
    labs(
      title="Coral Community Composition",
      subtitle=ma,
      x="Year",
      y=paste0("Annual average richness (number of species)")
    ) +
    scale_fill_manual(
      name = "Species group",
      values = sg1_palette,
      labels = sp_labels,
      na.value = "grey50"
    ) +
    scale_x_continuous(limits = c(minyr-0.50, maxyr+0.50), 
                       breaks = seq(minyr, maxyr, brk)) +
    SEACAR::SEACAR_plot_theme()
  
  timeline_data <- plot_data %>% 
    separate_longer_delim(ProgramIDs, delim = ", ") %>%
    arrange(ProgramIDs, Year) %>%
    group_by(ProgramIDs) %>% 
    mutate(
      is_gap = Year - lag(Year, default = first(Year)) > 1,
      block_id = cumsum(is_gap)
    ) %>%
    group_by(ProgramIDs, block_id) %>% 
    reframe(
      startYear = min(Year)-0.5,
      endYear = max(Year)+0.5
    ) %>% 
    select(-block_id) %>%
    mutate(ProgramIDs = as.factor(ProgramIDs)) %>%
    arrange(desc(ProgramIDs))
  
  # Add in program names from progs
  timeline_data <- timeline_data %>% 
    left_join(progs, join_by(ProgramIDs==ProgramID))
  
  # Create data for labels
  label_data <- timeline_data %>%
    group_by(ProgramIDs, ProgramName) %>%
    summarise(start_label = min(startYear),
              end_label = max(endYear), 
              .groups = "drop") %>%
    mutate(mid_label = (start_label + end_label) / 2)
  
  # ProgramID timeline
  timeline_plot <- ggplot(timeline_data) +
    geom_linerange(
      aes(xmin = startYear, xmax = endYear, y = ProgramIDs, color = ProgramIDs), 
      alpha = 0.4, linewidth = 6
    ) +
    geom_text(
      data = label_data,
      aes(y = ProgramIDs, label = ProgramName),
      x = min(label_data$start_label),
      color = "black",
      size = 2,
      vjust = 0.5,
      hjust = -0.01
    ) +
    scale_x_continuous(limits = c(minyr-0.5, maxyr+0.5),
                       breaks = seq(minyr, maxyr, brk)) +
    scale_y_discrete(limits = rev) +
    scale_color_manual(values = pids_palette) +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.box.margin = margin(0, 0, 0, 0)
    )
  
  plot <- p1 / timeline_plot + plot_layout(heights = c(5, 2))
  
  # Sets file name of plot created
  outname <- paste0("Coral_Community_Composition_", ma_abrev, ".png")
  # Saves plot as a png image
  ggsave(filename = paste0(out_dir, "/Figures/", outname), 
         plot = plot,
         width = 8,
         height = 7,
         units = "in")
}

#Gets list of all CC image files and creates zip directory
fig_list <- list.files(paste0(out_dir, "/Figures"), pattern=".png", full=FALSE)
setwd(paste0(out_dir, "/Figures"))
zip("CoralCCFigures", files=fig_list)
setwd(wd)

if(create_maps){
  coral_pc_data <- data
  coral_sr_data <- data[ParameterName=="Presence/Absence" & 
                          SpeciesGroup1 %in% c("Grazers and reef dependent species", "Reef fish")]
  coral_cc_data <- data[ParameterName=="Presence/Absence" & 
                          ResultValue==1 & 
                          SpeciesGroup1 %in% comp_species_groups & 
                          !ProgramID %in% exclude_progs]
  analysis <- "ma"
  hab <- "Coral Reef"
  source("../AllHabitats_Maps.R", echo = T, chdir = T)
}

##### Generate Table Descriptions
# Read in Coral SR stats
coral_sr_stats <- fread(paste0("output/SpeciesRichness/Coral_SpeciesRichness_MA_Overall_Stats.txt"))
# Read in Coral PC stats
coral_pc_stats <- fread(paste0("output/PercentCover/Coral_PC_LME_Stats.txt"))
# Read in Coral CC stats
coral_cc_stats <- fread(paste0("output/CommunityComposition/Coral_CC_MA_Overall_Stats.txt"))
# Empty table to store results
descriptionTable <- data.table()
# Loop through MAs and apply necessary functions
for(ma in unique(c(coral_pc_stats$ManagedAreaName, coral_sr_stats$ManagedAreaName, coral_cc_stats$ManagedAreaName))){
  if(ma %in% coral_pc_stats$ManagedAreaName){
    descriptionText <- SEACAR::generate_description(data = coral_pc_stats[ManagedAreaName==ma, ], habitat = "Coral")
    descriptionTable <- bind_rows(descriptionTable, descriptionText)
  }
  if(ma %in% coral_sr_stats$ManagedAreaName){
    descriptionText <- SEACAR::generate_description(data = coral_sr_stats[ManagedAreaName==ma, ], habitat = "Coral")
    descriptionTable <- bind_rows(descriptionTable, descriptionText)
  }
  if(ma %in% coral_cc_stats$ManagedAreaName){
    descriptionText <- SEACAR::generate_description(data = coral_cc_stats[ManagedAreaName==ma, ], habitat = "Coral")
    descriptionTable <- bind_rows(descriptionTable, descriptionText)
  }
}

# Write .csv of text results
fwrite(descriptionTable, file = "output/coral_tableDescriptions.csv")

# Render both reports
report_types <- c("SpeciesRichness", "PercentCover", "CommunityComposition")
for(file_type in c("HTML", "PDF")){
  descriptionColumn <- ifelse(file_type=="PDF", "DescriptionLatex", "DescriptionHTML")
  tableFormat <- ifelse(file_type=="PDF", "latex", "simple")
  for(report_type in report_types){
    file_out <-  paste0("Coral_", report_type, "_Report")
    template <- if(report_type=="SpeciesRichness"){
      "Coral_SpeciesRichness.Rmd"
    } else if(report_type=="PercentCover"){
      "Coral_PC.Rmd"
    } else if(report_type=="CommunityComposition"){
      "Coral_CC.Rmd"
    }
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
