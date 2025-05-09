library(knitr)
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(tidyr)
library(gridExtra)
library(hrbrthemes)
library(nlme)
library(ggpubr)

data <- fread(coral_file_in, sep="|", header=TRUE, stringsAsFactors=FALSE)

data2 <- data

params_to_plot <- c("Percent Cover", "Species Richness")

MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE, na.strings = "")

######################
### DATA FILTERING ###
######################

if("Percent Cover" %in% params_to_plot){
  
  out_dir <- "output/Data/Coral/PercentCover"
  param_name <- "Percent Cover"
  param_file <- "PC"
  seed <- 42
  
  # Only keep data for Percent Cover
  data <- data[data$ParameterName==param_name]
  
  # Simplify ParameterName to Percent Cover
  data$ParameterName <- param_name
  
  # Sets units for percent cover
  unit <- "%"
  data$ParameterUnits <- unit
  
  # Remove any rows that are not corals
  data <- data[SpeciesGroup1=="Octocoral"|
                 SpeciesGroup1=="Milleporans"|
                 SpeciesGroup1=="Scleractinian", ]
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
  # Remove rows with missing ResultValue
  data <- data[!is.na(data$ResultValue),]
  # Remove rows with missing SampleDate
  data <- data[!is.na(data$SampleDate),]
  # Remove duplicate rows
  data <- data[data$MADup==1,]
  # Create variable that combines the genus and species name
  data$gensp <- paste(data$GenusName, data$SpeciesName, sep=" ")
  # Corrects Managed Area names to be consistent with official names
  data$ManagedAreaName[data$ManagedAreaName=="Florida Keys NMS"] <-
    "Florida Keys National Marine Sanctuary"
  data$ManagedAreaName[data$ManagedAreaName==
                         "Biscayne Bay-Cape Florida to Monroe County Line"] <-
    "Biscayne Bay-Cape Florida to Monroe County Line Aquatic Preserve"
  data$ManagedAreaName[data$ManagedAreaName=="Coupon Bight"] <-
    "Coupon Bight Aquatic Preserve"
  data$ManagedAreaName[data$ManagedAreaName=="Coral ECA"] <-
    "Southeast Florida Coral Reef Ecosystem Conservation Area"
  
  # Adds AreaID for each managed area by combining the MA_All datatable to the
  # data based on ManagedAreaName
  data <- merge.data.frame(MA_All[,c("AreaID", "ManagedAreaName")],
                           data, by=c("AreaID", "ManagedAreaName"), all=TRUE)
  ###############
  ## SUM STATS ##
  ###############
  
  # Create summary statistics for each managed area based on Year and Month
  # intervals.
  MA_YM_Stats <- data %>%
    dplyr::group_by(AreaID, ManagedAreaName, Year, Month) %>%
    dplyr::summarize(ParameterName=param_name,
              N_Data=length(na.omit(ResultValue)),
              Min=min(ResultValue),
              Max=max(ResultValue),
              Median=median(ResultValue),
              Mean=mean(ResultValue),
              StandardDeviation=sd(ResultValue),
              Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                             collapse=', '),
              ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                               collapse=', '))
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
    dplyr::group_by(AreaID, ManagedAreaName, Year) %>%
    dplyr::summarize(ParameterName=param_name,
              N_Data=length(na.omit(ResultValue)),
              Min=min(ResultValue),
              Max=max(ResultValue),
              Median=median(ResultValue),
              Mean=mean(ResultValue),
              StandardDeviation=sd(ResultValue),
              Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                             collapse=', '),
              ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                               collapse=', '))
  # Puts the data in order based on ManagedAreaName then Year
  MA_Y_Stats <- as.data.table(MA_Y_Stats[order(MA_Y_Stats$ManagedAreaName,
                                               MA_Y_Stats$Year), ])
  # Writes summary statistics to file
  fwrite(MA_Y_Stats, paste0(out_dir,"/Coral_", param_file,
                            "_MA_Yr_Stats.txt"), sep="|")
  
  # Create summary statistics for each managed area based on Month intervals.
  MA_M_Stats <- data %>%
    dplyr::group_by(AreaID, ManagedAreaName, Month) %>%
    dplyr::summarize(ParameterName=param_name,
              N_Data=length(na.omit(ResultValue)),
              Min=min(ResultValue),
              Max=max(ResultValue),
              Median=median(ResultValue),
              Mean=mean(ResultValue),
              StandardDeviation=sd(ResultValue),
              Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                             collapse=', '),
              ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                               collapse=', '))
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
    dplyr::group_by(AreaID, ManagedAreaName) %>%
    dplyr::summarize(ParameterName=param_name,
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
                               collapse=', '))
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
  MA_Include <- unique(MA_Ov_Stats$ManagedAreaName[!is.na(MA_Ov_Stats$Mean)&
                                                     MA_Ov_Stats$SufficientData==
                                                     TRUE])
  
  # Puts the managed areas in alphabetical order
  MA_Include <- MA_Include[order(MA_Include)]
  
  # Determines the number of managed areas used
  n <- length(MA_Include)
  
  coral_pc_MA_Include <- MA_Include
  
  ###############
  ##### LME #####
  ###############
  
  # Creates blank data frame with number of rows defined by how many managed areas
  # are going to be analyzed
  lme_stats <- data.frame(matrix(ncol = 5, nrow = n))
  # Sets column names for blank data frame
  colnames(lme_stats) <- c("AreaID", "ManagedAreaName", "LME_Intercept",
                           "LME_Slope", "LME_p")
  
  # Begins to loop through each managed area for analysis
  for(i in 1:n){
    # Gets data for current managegd area
    lme_data <- data[data$ManagedAreaName==MA_Include[i],]
    # Perform LME for relation between ResultValue and Year for current managed area
    AnyCoral<-lme(ResultValue ~ Year,
                  random =~1|ProgramLocationID,
                  na.action = na.omit,
                  data = lme_data)
    # Store information and model fits in appropriate row of data frame
    lme_stats$AreaID[i] <- unique(lme_data$AreaID)
    lme_stats$ManagedAreaName[i] <- MA_Include[i]
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
  lme_stats <- as.data.frame(lme_stats[order(lme_stats$ManagedAreaName), ])
  
  # Write lme statistics to file
  fwrite(lme_stats, paste0(out_dir,"/Coral_", param_file,
                           "_LME_Stats.txt"), sep="|")
  
  # Gets lower x and y values based on LME fit to use in plot
  lme_plot <- lme_stats %>%
    dplyr::group_by(AreaID, ManagedAreaName) %>%
    dplyr::summarize(x=EarliestYear,
              y=LME_Slope*x+LME_Intercept)
  # Gets upper x and y values based on LME fit to use in plot
  lme_plot2 <- lme_stats %>%
    dplyr::group_by(AreaID, ManagedAreaName) %>%
    dplyr::summarize(x=LatestYear,
              y=LME_Slope*x+LME_Intercept)
  # Merges LME fit values for plot into one data frame
  lme_plot <- bind_rows(lme_plot, lme_plot2)
  rm(lme_plot2)
  # Puts LME plot data fram in alphabetical order by managed area
  lme_plot <- as.data.frame(lme_plot[order(lme_plot$ManagedAreaName), ])
  lme_plot <- lme_plot[!is.na(lme_plot$y),]
  
  # unqiue data filename for later access
  data_pc <- data
  lme_plot_pc <- lme_plot
  MA_Ov_Stats_pc <- MA_Ov_Stats

}

if("Species Richness" %in% params_to_plot){
  
  out_dir <- "output/Data/Coral/SpeciesRichness"
  param_file <- "SpeciesRichness"
  
  # Only keep data for Presence of grazers and reef-dependent species
  data <- data2[data2$ParameterName=="Presence/Absence" & 
                  data2$SpeciesGroup1 %in% c("Grazers and reef dependent species", "Reef Fish")]
  
  # Create ParameterName Column
  data$ParameterName <- "Species Richness"
  parameter <- "Species Richness"
  
  # Sets units for species richness
  unit <- "# of species"
  data$ParameterUnits <- unit
  
  # Remove rows with missing ManagedAreaName
  data <- data[!is.na(data$ManagedAreaName),]
  data <- data[data$ManagedAreaName!="NA",]
  # Remove rows with missing GenusName
  # data <- data[!is.na(data$GenusName),]
  # Remove rows with missing SpeciesName
  # data <- data[!is.na(data$SpeciesName),]
  # Remove rows with missing Months
  data <- data[!is.na(data$Month),]
  # Remove rows with missing Years
  data <- data[!is.na(data$Year),]
  # Remove rows with missing SpeciesGroup1
  data <- data[!is.na(data$SpeciesGroup1),]
  # Remove rows with invasive species
  data <- data[data$SpeciesGroup1!="Invasive",]
  # Set ResultValue to be a number value
  data$ResultValue <- as.numeric(data$ResultValue)
  # Remove rows where ResultValue is 0 and missing
  data <- data[data$ResultValue!=0,]
  data <- data[!is.na(data$ResultValue),]
  # Remove duplicate rows
  data <- data[data$MADup==1 & data$Include==1,]
  
  # Create variable that combines the genus and species name
  # data$gensp <- paste(data$GenusName, data$SpeciesName, sep=" ")
  
  # Create Species Richness values for groups of unique combinations of
  # ManagedAreaName, ProgramID, ProgramName, ProgramLocationID, and SampleDate.
  data <- data[data$ResultValue==1] %>%
    group_by(ManagedAreaName, ProgramID, ProgramName, ProgramLocationID, SampleDate) %>%
    summarise(ParameterName=parameter, Year=unique(Year), Month=unique(Month), 
              SpeciesRichness=length(unique(CommonIdentifier))) #CommonIdentifier was gensp
  
  # Adds AreaID for each managed area by combining the MA_All datatable to the
  # data based on ManagedAreaName
  data <- merge.data.frame(MA_All[,c("AreaID", "ManagedAreaName")],
                           data, by="ManagedAreaName", all=TRUE)
  
  # Writes this data that is used by the rest of the script to a text file
  fwrite(data, paste0(out_dir,"/Coral_", param_file, "_UsedData.txt"),
         sep="|")
  
  # Makes sure SampleDate is being stored as a Date object
  data$SampleDate <- as.Date(data$SampleDate)
  
  # Creates a variable with the names of all the managed areas that contain
  # species observations
  MA_Include <- unique(data$ManagedAreaName[!is.na(data$SpeciesRichness)])
  
  # Puts the managed areas in alphabetical order
  MA_Include <- MA_Include[order(MA_Include)]
  
  coral_sr_MA_Include <- MA_Include
  
  # Determines the number of managed areas used
  n <- length(MA_Include)
  
  #####################
  ### SUMMARY STATS ###
  #####################
  
  # Create summary statistics for each managed area based on Year and Month
  # intervals.
  MA_YM_Stats <- data %>%
    dplyr::group_by(AreaID, ManagedAreaName, Year, Month) %>%
    dplyr::summarize(ParameterName=parameter,
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
  fwrite(MA_YM_Stats, paste0(out_dir,"/Coral_", param_file,
                             "_MA_MMYY_Stats.txt"), sep="|")
  # Removes variable storing data to improve computer memory
  rm(MA_YM_Stats)
  
  # Create summary statistics for each managed area based on Year intervals
  MA_Y_Stats <- data %>%
    dplyr::group_by(AreaID, ManagedAreaName, Year) %>%
    dplyr::summarize(ParameterName=parameter,
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
  fwrite(MA_Y_Stats, paste0(out_dir,"/Coral_", param_file,
                            "_MA_Yr_Stats.txt"), sep="|")
  
  # Create summary statistics for each managed area based on Month intervals.
  MA_M_Stats <- data %>%
    dplyr::group_by(AreaID, ManagedAreaName, Month) %>%
    dplyr::summarize(ParameterName=parameter,
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
  fwrite(MA_M_Stats, paste0(out_dir,"/Coral_", param_file,
                            "_MA_Mo_Stats.txt"), sep="|")
  # Removes variable storing data to improve computer memory
  rm(MA_M_Stats)
  
  # Create summary overall statistics for each managed area.
  MA_Ov_Stats <- data %>%
    dplyr::group_by(AreaID, ManagedAreaName) %>%
    dplyr::summarize(ParameterName=parameter,
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
    man <- MA_Ov_Stats$ManagedAreaName[m]
    
    # Skips to next row if there are no data for this combination
    if(MA_Ov_Stats$N_Data[m]==0){
      next
    }
    # Gets subset of data from MA_Y_Stats (yearly summary stats) with this
    # ManagedAreaName
    ds <- MA_Y_Stats[MA_Y_Stats$ManagedAreaName==man,]
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
  fwrite(MA_Ov_Stats, paste0(out_dir,"/Coral_", param_file,
                             "_MA_Overall_Stats.txt"), sep="|")
  # Removes entries from the overall statistics that do not have data.
  # Based on presence or absence of EarliestYear
  MA_Ov_Stats <- MA_Ov_Stats[!is.na(MA_Ov_Stats$EarliestYear), ]
  
  MA_Y_Stats_sr <- MA_Y_Stats
  MA_Ov_Stats_sr <- MA_Ov_Stats
  
}

######################
### START PLOTTING ###
######################

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

# Create jitter object that sets the height and width
# Sets seed to be reproducible
plot_jitter <- position_jitter(width = 0.2, height = 0.2, seed=seed)
# Color palette for SEACAR
color_palette <- c("#005396", "#0088B1", "#00ADAE", "#65CCB3", "#AEE4C1",
                            "#FDEBA8", "#F8CD6D", "#F5A800", "#F17B00")
                            
##########################
### PLOTTING FUNCTIONS ###
##########################

plot_coral_pc <- function(ma, data = data_pc, lme_plot = lme_plot_pc, MA_Ov_Stats = MA_Ov_Stats_pc){
  # Gets data for target managed area
  plot_data <- data[data$ManagedAreaName==ma,]
  
  lme_plot_data <- lme_plot[lme_plot$ManagedAreaName==ma,]
  # Determines most recent year with available data for managed area
  t_max <- max(MA_Ov_Stats$LatestYear[MA_Ov_Stats$ManagedAreaName==ma])
  # Determines earliest recent year with available data for managed area
  t_min <- min(MA_Ov_Stats$EarliestYear[MA_Ov_Stats$ManagedAreaName==ma])
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
  y_range <- max(plot_data$ResultValue) - min(plot_data$ResultValue)
  
  # Sets y_min to be -1
  y_min <- -1
  
  # Sets upper bound of y-axis to be 10% of the data range above the
  # maximum value.
  y_max <- max(plot_data$ResultValue)+(0.1*y_range)
  
  
  # Creates plot object using plot_data.
  # Data is plotted as a point pot with jitter to show concentrations
  # that overlap. LME fit is plotted as a line
  p1 <- ggplot(data=plot_data) +
    geom_point(aes(x=Year, y=ResultValue), 
               position=plot_jitter, shape=21, size=2,
               color="#333333", fill="#cccccc", alpha=1) +
    geom_line(data=lme_plot_data, aes(x=x, y=y),
              color="#000099", size=2, alpha=0.8) +
    labs(title="Coral Percent Cover",
         subtitle=ma,
         x="Year", y="Percent cover (%)") +
    scale_x_continuous(limits=c(t_min-0.25, t_max+0.25),
                       breaks=seq(t_max, t_min, brk)) +
    scale_y_continuous(limits=c(y_min, y_max),
                       breaks=pretty_breaks(n=5)) +
    plot_theme
  # Sets file name of plot created
  outname <- paste0("Coral_", param_file, "_", gsub(" ", "", ma),
                    ".png")
  # # Saves plot as a png image
  # png(paste0(out_dir, "/Figures/", outname),
  #     width = 8,
  #     height = 4,
  #     units = "in",
  #     res = 200)
  # print(p1)
  # dev.off()
  
  # Creates a data table object to be shown underneath plots in report
  ResultTable <-
    lme_stats[lme_stats$ManagedAreaName==ma,]
  # Removes location, and parameter information because it is in plot
  # labels
  ResultTable <- select(ResultTable, -c("AreaID", "ManagedAreaName",
                                        "ParameterName"))
  # Renames StandardDeviation to StDev to save horizontal space
  ResultTable <- ResultTable %>%
    dplyr::rename("StDev"="StandardDeviation")
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

plot_coral_sr <- function(ma, MA_Y_Stats = MA_Y_Stats_sr, MA_Ov_Stats = MA_Ov_Stats_sr){
  # Gets data for target managed area
  plot_data <- MA_Y_Stats[MA_Y_Stats$ManagedAreaName==ma]
  # Determines most recent year with available data for managed area
  t_max <- max(MA_Ov_Stats$LatestYear[MA_Ov_Stats$ManagedAreaName==ma])
  # Determines earliest recent year with available data for managed area
  t_min <- min(MA_Ov_Stats$EarliestYear[MA_Ov_Stats$ManagedAreaName==ma])
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
  
  
  # Creates plot object using plot_data.
  # Data is plotted as symbols with connected lines.
  p1 <- ggplot(data=plot_data) +
    # geom_line(aes(x=Year, y=Mean), color=color_palette[1],
    #           size=0.75, alpha=1) +
    geom_point(aes(x=Year, y=Mean), fill=color_palette[1],
               shape=21, size=2, color="#333333", alpha=1) +
    labs(title="Grazers and Reef-Dependent Species Richness",
         subtitle=ma,
         x="Year", y="Richness (# of species)") +
    scale_x_continuous(limits=c(t_min-0.25, t_max+0.25),
                       breaks=seq(t_max, t_min, brk)) +
    scale_y_continuous(limits=c(y_min, y_max),
                       breaks=pretty_breaks(n=5)) +
    plot_theme
  # Sets file name of plot created
  outname <- paste0("Coral_", param_file, "_", gsub(" ", "", ma),
                    ".png")
  # Saves plot as a png image
  # png(paste0(out_dir, "/Figures/", outname),
  #     width = 8,
  #     height = 4,
  #     units = "in",
  #     res = 200)
  # print(p1)
  # dev.off()
  
  # Creates a data table object to be shown underneath plots in report
  ResultTable <-
    MA_Ov_Stats[MA_Ov_Stats$ManagedAreaName==ma,]
  # Removes location, and parameter information because it is in plot
  # labels
  ResultTable <- ResultTable[,-c("AreaID", "ManagedAreaName",
                                 "ProgramIDs", "Programs", "ParameterName")]
  # Renames StandardDeviation to StDev to save horizontal space
  ResultTable <- ResultTable %>%
    dplyr::rename("StDev"="StandardDeviation")
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

coral_managed_areas <- unique(c(coral_pc_MA_Include, coral_sr_MA_Include))