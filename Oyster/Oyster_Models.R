library(brms)
library(Rmisc)
library(rstan)
library(rstantools)
library(stringr)
library(data.table)
library(sf)
library(tidyverse)
library(doFuture)
library(tictoc)
library(doRNG)
library(rstudioapi)
library(ggpubr)

wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

# Set output directory
out_dir <- "output"

# Bring in functions for oyster plotting
source("Oyster_Functions.R")

file_in <- str_subset(
  list.files("C:/SEACAR Data/SEACARdata/", full.names = TRUE), 
  "OYSTER")
oysterraw <- fread(file_in, sep="|", na.strings=c("NULL"))
oysterraw2 <- pivot_wider(oysterraw, names_from="ParameterName",
                          values_from="ResultValue")
setDT(oysterraw2)
setnames(oysterraw2, c("Density", "Percent Live", "Shell Height",
                       "Number of Oysters Counted - Live",
                       "Number of Oysters Counted - Dead",
                       "Number of Oysters Counted - Total", "Reef Height"),
         c("Density_m2", "PercentLive_pct", "ShellHeight_mm",
           "Number_of_Oysters_Counted_Live_Count",
           "Number_of_Oysters_Counted_Dead_Count",
           "Number_of_Oysters_Counted_Total_Count",
           "ReefHeight_mm"))
oysterraw2[, ObsIndex := seq(1:nrow(oysterraw2))]

oysterraw <- oysterraw2
rm(oysterraw2)

# Output from oyster ID script (will make autonomous)
new_crosswalk2 <- readRDS("C:/Users/Hill_T/Desktop/SEACAR GitHub/QAQC-Tools/OysterReefID/output/test/medianBuffer_14/new_crosswalk2.rds")

oysterraw2 <- merge(oysterraw[, -c("UniversalReefID")], new_crosswalk2 %>% select(c("UniversalReefID", "LocationID")), by = "LocationID")
oysterraw <- oysterraw2
rm(oysterraw2)

oysterraw[, `:=` (RowID=as.integer(RowID),
                  ProgramID=as.integer(ProgramID),
                  LocationID=as.integer(LocationID),
                  ProgramName=as.character(ProgramName),
                  ProgramLocationID=as.character(ProgramLocationID),
                  QuadIdentifier=as.character(QuadIdentifier),
                  ReefIdentifier=as.character(ReefIdentifier),
                  UniversalReefID=as.factor(UniversalReefID),
                  LiveDate=as.integer(ifelse(!is.na(LiveDate_Qualifier) &
                                               str_detect(LiveDate,
                                                          "....-..-.."), 
                                             paste0(str_sub(LiveDate, 1, 4)), 
                                             round(as.numeric(LiveDate)))),
                  LiveDate_Qualifier=as.character(LiveDate_Qualifier),
                  LiveDate_MinEstDate=as.numeric(LiveDate_MinEstDate),
                  LiveDate_MaxEstDate=as.numeric(LiveDate_MaxEstDate),
                  SampleAge_Stdev=as.numeric(SampleAge_Stdev),
                  #GISUniqueID=as.logical(GISUniqueID),
                  Year=as.integer(Year),
                  Month=as.integer(Month),
                  ManagedAreaName=as.character(ManagedAreaName),
                  SurveyMethod=as.character(SurveyMethod),
                  PercentLiveMethod=as.character(PercentLiveMethod),
                  HabitatClassification=as.character(HabitatClassification),
                  MinimumSizeMeasured_mm=as.character(MinimumSizeMeasured_mm),
                  NumberMeasured_n=as.character(NumberMeasured_n),
                  QuadSize_m2=as.numeric(QuadSize_m2),
                  MADup=as.integer(MADup),
                  Density_m2=as.numeric(Density_m2),
                  PercentLive_pct=as.numeric(PercentLive_pct),
                  ShellHeight_mm=as.numeric(ShellHeight_mm),
                  Number_of_Oysters_Counted_Total_Count =
                    as.integer(Number_of_Oysters_Counted_Total_Count),
                  Number_of_Oysters_Counted_Live_Count =
                    as.integer(Number_of_Oysters_Counted_Live_Count),
                  Number_of_Oysters_Counted_Dead_Count =
                    as.integer(Number_of_Oysters_Counted_Dead_Count),
                  ObsIndex=as.integer(ObsIndex))]

#Calculate Density_m2 values for ProgramID==4016 & 4042
oysterraw[ProgramID==4016, Density_m2 :=
            Number_of_Oysters_Counted_Live_Count/as.numeric(QuadSize_m2)]
oysterraw[ProgramID==4042 & !is.na(Number_of_Oysters_Counted_Live_Count),
          Density_m2 :=
            Number_of_Oysters_Counted_Live_Count/as.numeric(QuadSize_m2)]

#Remove "25" values from total counts column, make all "PercentLiveMethod"
#values the same, and calculate estimated live Density for ProgramID==5074 and 
oysterraw <- oysterraw[RowID %in%
                         setdiff(
                           oysterraw[, RowID],
                           oysterraw[ProgramID ==5074 &
                                       Number_of_Oysters_Counted_Total_Count==25, RowID]), ]
oysterraw[ProgramID==5074, PercentLiveMethod := "Estimated percent"]
oysterraw[ProgramID==5074, SampleDate :=
            unique(oysterraw[ProgramID==5074 &
                               !is.na(Number_of_Oysters_Counted_Total_Count),
                             SampleDate])[1]]

#Some PercentLiveMethod values for ID4042 are NA
oysterraw[ProgramID==4042 | ProgramID==4016,
          PercentLiveMethod := "Point-intercept"]

#make sure quadrat identifiers are unique
oysterraw[, QuadIdentifier_old := QuadIdentifier]
oysterraw[, QuadIdentifier := paste(UniversalReefID,
                                    LocationID, Year, Month,
                                    QuadIdentifier_old, sep="_")]

oysterraw[, MA_plotlab := paste0(ManagedAreaName, "_", HabitatClassification)]
subtidal <- c(4044, 5007, 5071, 5073)
oysterraw[, Subtidal := ifelse(ProgramID %in% subtidal, 1, 0)][, Subtidal := as.logical(Subtidal)]

#Create variables for relative year and size class category for data that
#should be included in analyses and counts of live oysters measured
for(i in unique(oysterraw$ManagedAreaName)){
  oysterraw[ManagedAreaName==i & !is.na(LiveDate), `:=`
            (RelYear=(LiveDate-min(LiveDate))+1,
              YearDiff=min(LiveDate)-1,
              #adding 1 to each RelYear to avoid min(RelYear)==0,
              #because it is used later as an index for plotting years so
              #it needs to start from 1
              SizeClass=fcase(ShellHeight_mm >= 25 &
                                ShellHeight_mm < 75, "25to75mm",
                              ShellHeight_mm >= 75, "o75mm",
                              default=NA))]
  
  oysterraw[ManagedAreaName==i & !is.na(LiveDate),
            counts := length(ShellHeight_mm), by=c("QuadIdentifier")]
}

# Ensure RelYear column is listed as "years"
# oysterraw$RelYear <- time_length(oysterraw$RelYear, "years")

#Remove unrealistically high shell heights from ID_5017
oysterraw <- setdiff(oysterraw, oysterraw[ProgramID==5017 & ShellHeight_mm >= 165, ])

#Create data table to save model results
oysterresults <- data.table(indicator=character(),
                            managed_area=character(),
                            habitat_class=character(),
                            size_class=character(),
                            live_date_qual=character(),
                            n_programs=integer(),
                            programs=list(),
                            filename=character(),
                            effect=character(),
                            component=character(),
                            group=character(),
                            term=character(),
                            estimate=numeric(),
                            std.error=numeric(),
                            conf.low=numeric(),
                            conf.high=numeric())

#How many years of data for each managed area/habitat class/indicator combination?
# Apply the transformations based on non-missing values
setDT(oysterraw)
oysterraw[!is.na(Density_m2), `:=` (nyrpar="Density_m2",
                                    nyears=length(unique(Year))),
          by=MA_plotlab]
oysterraw[!is.na(PercentLive_pct), `:=` (nyrpar="PercentLive_pct",
                                         nyears=length(unique(Year))),
          by=MA_plotlab]
oysterraw[!is.na(ShellHeight_mm), `:=` (nyrpar="ShellHeight_mm",
                                        nyears=length(unique(Year))),
          by=MA_plotlab]
MAinclude <- distinct(oysterraw[, .(MA_plotlab, nyrpar, nyears)])
# View(MAinclude[!is.na(nyrpar) & nyears >= 5, ])

oysterraw[str_detect(MA_plotlab, "Pine Island Sound"),
           `:=` (MA_plotlab=ifelse(str_detect(ProgramLocationID,
                                              "Reference") |
                                     str_detect(ProgramLocationID,
                                                "Control"),
                                   "Pine Island Sound Aquatic Preserve_Natural",
                                   "Pine Island Sound Aquatic Preserve_Restored"),
                 HabitatClassification=ifelse(str_detect(ProgramLocationID,
                                                         "Reference") |
                                                str_detect(ProgramLocationID,
                                                           "Control"),
                                              "Natural", "Restored"))]

### Managed Area Statistics -----
## Density -----
oysterraw$SizeClass[oysterraw$SizeClass=="25to75mm"] <- "25-75mm"
oysterraw$SizeClass[oysterraw$SizeClass=="35to75mm"] <- "35-75mm"
oysterraw$SizeClass[oysterraw$SizeClass=="o75mm"] <- ">75mm"

# Create summary statistics for each managed area based on Year and Month
# intervals.
MA_YM_Stats <- oysterraw[oysterraw$nyrpar=="Density_m2",] %>%
  group_by(AreaID, ManagedAreaName, Year, Month, nyrpar,
           LiveDate_Qualifier, SizeClass, HabitatClassification) %>%
  dplyr::summarize(N_Data=length(Density_m2[!is.na(Density_m2)]),
                   Min=min(Density_m2[!is.na(Density_m2)]),
                   Max=max(Density_m2[!is.na(Density_m2)]),
                   Median=median(Density_m2[!is.na(Density_m2)]),
                   Mean=mean(Density_m2[!is.na(Density_m2)]),
                   StandardDeviation=sd(Density_m2[!is.na(Density_m2)]),
                   Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                                  collapse=', '),
                   ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                                    collapse=', '))
setnames(MA_YM_Stats, c("nyrpar", "LiveDate_Qualifier",
                        "HabitatClassification"),
         c("ParameterName", "ShellType", "HabitatType"))
MA_YM_Stats$ShellType[MA_YM_Stats$ShellType=="Exact"] <- "Live Oyster Shells"
MA_YM_Stats$ShellType[MA_YM_Stats$ShellType=="Estimate"] <- "Dead Oyster Shells"
# Puts the data in order based on ManagedAreaName, Year, then Month
MA_YM_Stats <- as.data.table(MA_YM_Stats[order(MA_YM_Stats$ManagedAreaName,
                                               MA_YM_Stats$Year,
                                               MA_YM_Stats$Month,
                                               MA_YM_Stats$ShellType,
                                               MA_YM_Stats$SizeClass,
                                               MA_YM_Stats$HabitatType), ])
# Writes summary statistics to file
fwrite(MA_YM_Stats, paste0(out_dir,"/Density/Oyster_Dens_MA_MMYY_Stats.txt"),
       sep="|")
# Removes variable storing data to improve computer memory
rm(MA_YM_Stats)

# Create summary statistics for each managed area based on Year intervals
MA_Y_Stats <- oysterraw[oysterraw$nyrpar=="Density_m2",] %>%
  group_by(AreaID, ManagedAreaName, Year, nyrpar, LiveDate_Qualifier,
           SizeClass, HabitatClassification) %>%
  dplyr::summarize(N_Data=length(Density_m2[!is.na(Density_m2)]),
                   Min=min(Density_m2[!is.na(Density_m2)]),
                   Max=max(Density_m2[!is.na(Density_m2)]),
                   Median=median(Density_m2[!is.na(Density_m2)]),
                   Mean=mean(Density_m2[!is.na(Density_m2)]),
                   StandardDeviation=sd(Density_m2[!is.na(Density_m2)]),
                   Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                                  collapse=', '),
                   ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                                    collapse=', '))
setnames(MA_Y_Stats, c("nyrpar", "LiveDate_Qualifier",
                       "HabitatClassification"),
         c("ParameterName", "ShellType", "HabitatType"))
MA_Y_Stats$ShellType[MA_Y_Stats$ShellType=="Exact"] <- "Live Oyster Shells"
MA_Y_Stats$ShellType[MA_Y_Stats$ShellType=="Estimate"] <- "Dead Oyster Shells"
# Puts the data in order based on ManagedAreaName then Year
MA_Y_Stats <- as.data.table(MA_Y_Stats[order(MA_Y_Stats$ManagedAreaName,
                                             MA_Y_Stats$Year,
                                             MA_Y_Stats$ShellType,
                                             MA_Y_Stats$SizeClass,
                                             MA_Y_Stats$HabitatType), ])
# Writes summary statistics to file
fwrite(MA_Y_Stats, paste0(out_dir,"/Density/Oyster_Dens_MA_Yr_Stats.txt"),
       sep="|")
# Removes variable storing data to improve computer memory
rm(MA_Y_Stats)

# Create summary statistics for each managed area based on Month intervals.
MA_M_Stats <- oysterraw[oysterraw$nyrpar=="Density_m2",] %>%
  group_by(AreaID, ManagedAreaName, Month, nyrpar,
           LiveDate_Qualifier, SizeClass,
           HabitatClassification) %>%
  dplyr::summarize(N_Data=length(Density_m2[!is.na(Density_m2)]),
                   Min=min(Density_m2[!is.na(Density_m2)]),
                   Max=max(Density_m2[!is.na(Density_m2)]),
                   Median=median(Density_m2[!is.na(Density_m2)]),
                   Mean=mean(Density_m2[!is.na(Density_m2)]),
                   StandardDeviation=sd(Density_m2[!is.na(Density_m2)]),
                   Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                                  collapse=', '),
                   ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                                    collapse=', '))
setnames(MA_M_Stats, c("nyrpar", "LiveDate_Qualifier",
                       "HabitatClassification"),
         c("ParameterName", "ShellType", "HabitatType"))
MA_M_Stats$ShellType[MA_M_Stats$ShellType=="Exact"] <- "Live Oyster Shells"
MA_M_Stats$ShellType[MA_M_Stats$ShellType=="Estimate"] <- "Dead Oyster Shells"
# Puts the data in order based on ManagedAreaName then Month
MA_M_Stats <- as.data.table(MA_M_Stats[order(MA_M_Stats$ManagedAreaName,
                                             MA_M_Stats$Month,
                                             MA_M_Stats$ShellType,
                                             MA_M_Stats$SizeClass,
                                             MA_M_Stats$HabitatType), ])
# Writes summary statistics to file
fwrite(MA_M_Stats, paste0(out_dir,"/Density/Oyster_Dens_MA_Mo_Stats.txt"),
       sep="|")
# Removes variable storing data to improve computer memory
rm(MA_M_Stats)

# Create summary overall statistics for each managed area.
MA_Ov_Stats <- oysterraw[oysterraw$nyrpar=="Density_m2",] %>%
  group_by(AreaID, ManagedAreaName, nyrpar,
           LiveDate_Qualifier, SizeClass,
           HabitatClassification) %>%
  dplyr::summarize(N_Years=length(unique(
    LiveDate[!is.na(LiveDate) & !is.na(Density_m2)])),
    SufficientData=ifelse(N_Years>=5, TRUE, FALSE),
    EarliestLiveDate=min(LiveDate[!is.na(Density_m2)]),
    LatestLiveDate=max(LiveDate[!is.na(Density_m2)]),
    LastSampleDate=max(SampleDate),
    N_Data=length(Density_m2[!is.na(Density_m2)]),
    Min=min(Density_m2[!is.na(Density_m2)]),
    Max=max(Density_m2[!is.na(Density_m2)]),
    Median=median(Density_m2[!is.na(Density_m2)]),
    Mean=mean(Density_m2[!is.na(Density_m2)]),
    StandardDeviation=sd(Density_m2[!is.na(Density_m2)]),
    Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                   collapse=', '),
    ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                     collapse=', '))
setnames(MA_Ov_Stats, c("nyrpar", "LiveDate_Qualifier",
                        "HabitatClassification"),
         c("ParameterName", "ShellType", "HabitatType"))
MA_Ov_Stats$ShellType[MA_Ov_Stats$ShellType=="Exact"] <- "Live Oyster Shells"
MA_Ov_Stats$ShellType[MA_Ov_Stats$ShellType=="Estimate"] <- "Dead Oyster Shells"
# Puts the data in order based on ManagedAreaName
MA_Ov_Stats <- as.data.table(MA_Ov_Stats[order(MA_Ov_Stats$ManagedAreaName,
                                               MA_Ov_Stats$ShellType,
                                               MA_Ov_Stats$SizeClass,
                                               MA_Ov_Stats$HabitatType), ])

# Replaces blank ProgramIDs with NA (missing values)
MA_Ov_Stats$ProgramIDs <- replace(MA_Ov_Stats$ProgramIDs,
                                  MA_Ov_Stats$ProgramIDs=="", NA)
MA_Ov_Stats$Programs <- replace(MA_Ov_Stats$Programs,
                                MA_Ov_Stats$Programs=="", NA)
# Write overall statistics to file
fwrite(MA_Ov_Stats, paste0(out_dir,"/Density/Oyster_Dens_MA_Overall_Stats.txt"),
       sep="|")
# Removes variable storing data to improve computer memory
rm(MA_Ov_Stats)

## Shell Height -----
# Create summary statistics for each managed area based on Year and Month
# intervals.
MA_YM_Stats <- oysterraw[oysterraw$nyrpar=="ShellHeight_mm",] %>%
  group_by(AreaID, ManagedAreaName, Year, Month, nyrpar,
           LiveDate_Qualifier, SizeClass, HabitatClassification) %>%
  dplyr::summarize(N_Data=length(ShellHeight_mm[!is.na(ShellHeight_mm)]),
                   Min=min(ShellHeight_mm[!is.na(ShellHeight_mm)]),
                   Max=max(ShellHeight_mm[!is.na(ShellHeight_mm)]),
                   Median=median(ShellHeight_mm[!is.na(ShellHeight_mm)]),
                   Mean=mean(ShellHeight_mm[!is.na(ShellHeight_mm)]),
                   StandardDeviation=sd(ShellHeight_mm[!is.na(ShellHeight_mm)]),
                   Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                                  collapse=', '),
                   ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                                    collapse=', '))
setnames(MA_YM_Stats, c("nyrpar", "LiveDate_Qualifier",
                        "HabitatClassification"),
         c("ParameterName", "ShellType", "HabitatType"))
MA_YM_Stats$ShellType[MA_YM_Stats$ShellType=="Exact"] <- "Live Oyster Shells"
MA_YM_Stats$ShellType[MA_YM_Stats$ShellType=="Estimate"] <- "Dead Oyster Shells"
# Puts the data in order based on ManagedAreaName, Year, then Month
MA_YM_Stats <- as.data.table(MA_YM_Stats[order(MA_YM_Stats$ManagedAreaName,
                                               MA_YM_Stats$Year,
                                               MA_YM_Stats$Month,
                                               MA_YM_Stats$ShellType,
                                               MA_YM_Stats$SizeClass,
                                               MA_YM_Stats$HabitatType), ])
# Writes summary statistics to file
fwrite(MA_YM_Stats, paste0(out_dir,"/Shell_Height/Oyster_SH_MA_MMYY_Stats.txt"),
       sep="|")
# Removes variable storing data to improve computer memory
rm(MA_YM_Stats)

# Create summary statistics for each managed area based on Year intervals
MA_Y_Stats <- oysterraw[oysterraw$nyrpar=="ShellHeight_mm",] %>%
  group_by(AreaID, ManagedAreaName, Year, nyrpar, LiveDate_Qualifier,
           SizeClass, HabitatClassification) %>%
  dplyr::summarize(N_Data=length(ShellHeight_mm[!is.na(ShellHeight_mm)]),
                   Min=min(ShellHeight_mm[!is.na(ShellHeight_mm)]),
                   Max=max(ShellHeight_mm[!is.na(ShellHeight_mm)]),
                   Median=median(ShellHeight_mm[!is.na(ShellHeight_mm)]),
                   Mean=mean(ShellHeight_mm[!is.na(ShellHeight_mm)]),
                   StandardDeviation=sd(ShellHeight_mm[!is.na(ShellHeight_mm)]),
                   Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                                  collapse=', '),
                   ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                                    collapse=', '))
setnames(MA_Y_Stats, c("nyrpar", "LiveDate_Qualifier",
                       "HabitatClassification"),
         c("ParameterName", "ShellType", "HabitatType"))
MA_Y_Stats$ShellType[MA_Y_Stats$ShellType=="Exact"] <- "Live Oyster Shells"
MA_Y_Stats$ShellType[MA_Y_Stats$ShellType=="Estimate"] <- "Dead Oyster Shells"
# Puts the data in order based on ManagedAreaName then Year
MA_Y_Stats <- as.data.table(MA_Y_Stats[order(MA_Y_Stats$ManagedAreaName,
                                             MA_Y_Stats$Year,
                                             MA_Y_Stats$ShellType,
                                             MA_Y_Stats$SizeClass,
                                             MA_Y_Stats$HabitatType), ])
# Writes summary statistics to file
fwrite(MA_Y_Stats, paste0(out_dir,"/Shell_Height/Oyster_SH_MA_Yr_Stats.txt"),
       sep="|")
# Removes variable storing data to improve computer memory
rm(MA_Y_Stats)

# Create summary statistics for each managed area based on Month intervals.
MA_M_Stats <- oysterraw[oysterraw$nyrpar=="ShellHeight_mm",] %>%
  group_by(AreaID, ManagedAreaName, Month, nyrpar,
           LiveDate_Qualifier, SizeClass,
           HabitatClassification) %>%
  dplyr::summarize(N_Data=length(ShellHeight_mm[!is.na(ShellHeight_mm)]),
                   Min=min(ShellHeight_mm[!is.na(ShellHeight_mm)]),
                   Max=max(ShellHeight_mm[!is.na(ShellHeight_mm)]),
                   Median=median(ShellHeight_mm[!is.na(ShellHeight_mm)]),
                   Mean=mean(ShellHeight_mm[!is.na(ShellHeight_mm)]),
                   StandardDeviation=sd(ShellHeight_mm[!is.na(ShellHeight_mm)]),
                   Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                                  collapse=', '),
                   ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                                    collapse=', '))
setnames(MA_M_Stats, c("nyrpar", "LiveDate_Qualifier",
                       "HabitatClassification"),
         c("ParameterName", "ShellType", "HabitatType"))
MA_M_Stats$ShellType[MA_M_Stats$ShellType=="Exact"] <- "Live Oyster Shells"
MA_M_Stats$ShellType[MA_M_Stats$ShellType=="Estimate"] <- "Dead Oyster Shells"
# Puts the data in order based on ManagedAreaName then Month
MA_M_Stats <- as.data.table(MA_M_Stats[order(MA_M_Stats$ManagedAreaName,
                                             MA_M_Stats$Month,
                                             MA_M_Stats$ShellType,
                                             MA_M_Stats$SizeClass,
                                             MA_M_Stats$HabitatType), ])
# Writes summary statistics to file
fwrite(MA_M_Stats, paste0(out_dir,"/Shell_Height/Oyster_SH_MA_Mo_Stats.txt"),
       sep="|")
# Removes variable storing data to improve computer memory
rm(MA_M_Stats)

# Create summary overall statistics for each managed area.
MA_Ov_Stats <- oysterraw[oysterraw$nyrpar=="ShellHeight_mm",] %>%
  group_by(AreaID, ManagedAreaName, nyrpar,
           LiveDate_Qualifier, SizeClass,
           HabitatClassification) %>%
  dplyr::summarize(N_Years=length(unique(
    LiveDate[!is.na(LiveDate) & !is.na(ShellHeight_mm)])),
    SufficientData=ifelse(N_Years>=5, TRUE, FALSE),
    EarliestLiveDate=min(LiveDate[!is.na(ShellHeight_mm)]),
    LatestLiveDate=max(LiveDate[!is.na(ShellHeight_mm)]),
    LastSampleDate=max(SampleDate),
    N_Data=length(ShellHeight_mm[!is.na(ShellHeight_mm)]),
    Min=min(ShellHeight_mm[!is.na(ShellHeight_mm)]),
    Max=max(ShellHeight_mm[!is.na(ShellHeight_mm)]),
    Median=median(ShellHeight_mm[!is.na(ShellHeight_mm)]),
    Mean=mean(ShellHeight_mm[!is.na(ShellHeight_mm)]),
    StandardDeviation=sd(ShellHeight_mm[!is.na(ShellHeight_mm)]),
    Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                   collapse=', '),
    ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                     collapse=', '))
setnames(MA_Ov_Stats, c("nyrpar", "LiveDate_Qualifier",
                        "HabitatClassification"),
         c("ParameterName", "ShellType", "HabitatType"))
MA_Ov_Stats$ShellType[MA_Ov_Stats$ShellType=="Exact"] <- "Live Oyster Shells"
MA_Ov_Stats$ShellType[MA_Ov_Stats$ShellType=="Estimate"] <- "Dead Oyster Shells"
# Puts the data in order based on ManagedAreaName
MA_Ov_Stats <- as.data.table(MA_Ov_Stats[order(MA_Ov_Stats$ManagedAreaName,
                                               MA_Ov_Stats$ShellType,
                                               MA_Ov_Stats$SizeClass,
                                               MA_Ov_Stats$HabitatType), ])

# Replaces blank ProgramIDs with NA (missing values)
MA_Ov_Stats$ProgramIDs <- replace(MA_Ov_Stats$ProgramIDs,
                                  MA_Ov_Stats$ProgramIDs=="", NA)
MA_Ov_Stats$Programs <- replace(MA_Ov_Stats$Programs,
                                MA_Ov_Stats$Programs=="", NA)
# Write overall statistics to file
fwrite(MA_Ov_Stats, paste0(out_dir,"/Shell_Height/Oyster_SH_MA_Overall_Stats.txt"),
       sep="|")
# Removes variable storing data to improve computer memory
rm(MA_Ov_Stats)

## Percent Live -----
# Create summary statistics for each managed area based on Year and Month
# intervals.
MA_YM_Stats <- oysterraw[oysterraw$nyrpar=="PercentLive_pct",] %>%
  group_by(AreaID, ManagedAreaName, Year, Month, nyrpar,
           LiveDate_Qualifier, SizeClass, HabitatClassification) %>%
  dplyr::summarize(N_Data=length(PercentLive_pct[!is.na(PercentLive_pct)]),
                   Min=min(PercentLive_pct[!is.na(PercentLive_pct)]),
                   Max=max(PercentLive_pct[!is.na(PercentLive_pct)]),
                   Median=median(PercentLive_pct[!is.na(PercentLive_pct)]),
                   Mean=mean(PercentLive_pct[!is.na(PercentLive_pct)]),
                   StandardDeviation=sd(PercentLive_pct[!is.na(PercentLive_pct)]),
                   Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                                  collapse=', '),
                   ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                                    collapse=', '))
setnames(MA_YM_Stats, c("nyrpar", "LiveDate_Qualifier",
                        "HabitatClassification"),
         c("ParameterName", "ShellType", "HabitatType"))
MA_YM_Stats$ShellType[MA_YM_Stats$ShellType=="Exact"] <- "Live Oyster Shells"
MA_YM_Stats$ShellType[MA_YM_Stats$ShellType=="Estimate"] <- "Dead Oyster Shells"
# Puts the data in order based on ManagedAreaName, Year, then Month
MA_YM_Stats <- as.data.table(MA_YM_Stats[order(MA_YM_Stats$ManagedAreaName,
                                               MA_YM_Stats$Year,
                                               MA_YM_Stats$Month,
                                               MA_YM_Stats$ShellType,
                                               MA_YM_Stats$SizeClass,
                                               MA_YM_Stats$HabitatType), ])
# Writes summary statistics to file
fwrite(MA_YM_Stats, paste0(out_dir,"/Percent_Live/Oyster_PrcLive_MA_MMYY_Stats.txt"),
       sep="|")
# Removes variable storing data to improve computer memory
rm(MA_YM_Stats)

# Create summary statistics for each managed area based on Year intervals
MA_Y_Stats <- oysterraw[oysterraw$nyrpar=="PercentLive_pct",] %>%
  group_by(AreaID, ManagedAreaName, Year, nyrpar, LiveDate_Qualifier,
           SizeClass, HabitatClassification) %>%
  dplyr::summarize(N_Data=length(PercentLive_pct[!is.na(PercentLive_pct)]),
                   Min=min(PercentLive_pct[!is.na(PercentLive_pct)]),
                   Max=max(PercentLive_pct[!is.na(PercentLive_pct)]),
                   Median=median(PercentLive_pct[!is.na(PercentLive_pct)]),
                   Mean=mean(PercentLive_pct[!is.na(PercentLive_pct)]),
                   StandardDeviation=sd(PercentLive_pct[!is.na(PercentLive_pct)]),
                   Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                                  collapse=', '),
                   ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                                    collapse=', '))
setnames(MA_Y_Stats, c("nyrpar", "LiveDate_Qualifier",
                       "HabitatClassification"),
         c("ParameterName", "ShellType", "HabitatType"))
MA_Y_Stats$ShellType[MA_Y_Stats$ShellType=="Exact"] <- "Live Oyster Shells"
MA_Y_Stats$ShellType[MA_Y_Stats$ShellType=="Estimate"] <- "Dead Oyster Shells"
# Puts the data in order based on ManagedAreaName then Year
MA_Y_Stats <- as.data.table(MA_Y_Stats[order(MA_Y_Stats$ManagedAreaName,
                                             MA_Y_Stats$Year,
                                             MA_Y_Stats$ShellType,
                                             MA_Y_Stats$SizeClass,
                                             MA_Y_Stats$HabitatType), ])
# Writes summary statistics to file
fwrite(MA_Y_Stats, paste0(out_dir,"/Percent_Live/Oyster_PrcLive_MA_Yr_Stats.txt"),
       sep="|")
# Removes variable storing data to improve computer memory
rm(MA_Y_Stats)

# Create summary statistics for each managed area based on Month intervals.
MA_M_Stats <- oysterraw[oysterraw$nyrpar=="PercentLive_pct",] %>%
  group_by(AreaID, ManagedAreaName, Month, nyrpar,
           LiveDate_Qualifier, SizeClass,
           HabitatClassification) %>%
  dplyr::summarize(N_Data=length(PercentLive_pct[!is.na(PercentLive_pct)]),
                   Min=min(PercentLive_pct[!is.na(PercentLive_pct)]),
                   Max=max(PercentLive_pct[!is.na(PercentLive_pct)]),
                   Median=median(PercentLive_pct[!is.na(PercentLive_pct)]),
                   Mean=mean(PercentLive_pct[!is.na(PercentLive_pct)]),
                   StandardDeviation=sd(PercentLive_pct[!is.na(PercentLive_pct)]),
                   Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                                  collapse=', '),
                   ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                                    collapse=', '))
setnames(MA_M_Stats, c("nyrpar", "LiveDate_Qualifier",
                       "HabitatClassification"),
         c("ParameterName", "ShellType", "HabitatType"))
MA_M_Stats$ShellType[MA_M_Stats$ShellType=="Exact"] <- "Live Oyster Shells"
MA_M_Stats$ShellType[MA_M_Stats$ShellType=="Estimate"] <- "Dead Oyster Shells"
# Puts the data in order based on ManagedAreaName then Month
MA_M_Stats <- as.data.table(MA_M_Stats[order(MA_M_Stats$ManagedAreaName,
                                             MA_M_Stats$Month,
                                             MA_M_Stats$ShellType,
                                             MA_M_Stats$SizeClass,
                                             MA_M_Stats$HabitatType), ])
# Writes summary statistics to file
fwrite(MA_M_Stats, paste0(out_dir,"/Percent_Live/Oyster_PrcLive_MA_Mo_Stats.txt"),
       sep="|")
# Removes variable storing data to improve computer memory
rm(MA_M_Stats)

# Create summary overall statistics for each managed area.
MA_Ov_Stats <- oysterraw[oysterraw$nyrpar=="PercentLive_pct",] %>%
  group_by(AreaID, ManagedAreaName, nyrpar,
           LiveDate_Qualifier, SizeClass,
           HabitatClassification) %>%
  dplyr::summarize(N_Years=length(unique(
    LiveDate[!is.na(LiveDate) & !is.na(PercentLive_pct)])),
    SufficientData=ifelse(N_Years>=5, TRUE, FALSE),
    EarliestLiveDate=min(LiveDate[!is.na(PercentLive_pct)]),
    LatestLiveDate=max(LiveDate[!is.na(PercentLive_pct)]),
    LastSampleDate=max(SampleDate),
    N_Data=length(PercentLive_pct[!is.na(PercentLive_pct)]),
    Min=min(PercentLive_pct[!is.na(PercentLive_pct)]),
    Max=max(PercentLive_pct[!is.na(PercentLive_pct)]),
    Median=median(PercentLive_pct[!is.na(PercentLive_pct)]),
    Mean=mean(PercentLive_pct[!is.na(PercentLive_pct)]),
    StandardDeviation=sd(PercentLive_pct[!is.na(PercentLive_pct)]),
    Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                   collapse=', '),
    ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                     collapse=', '))
setnames(MA_Ov_Stats, c("nyrpar", "LiveDate_Qualifier",
                        "HabitatClassification"),
         c("ParameterName", "ShellType", "HabitatType"))
MA_Ov_Stats$ShellType[MA_Ov_Stats$ShellType=="Exact"] <- "Live Oyster Shells"
MA_Ov_Stats$ShellType[MA_Ov_Stats$ShellType=="Estimate"] <- "Dead Oyster Shells"
# Puts the data in order based on ManagedAreaName
MA_Ov_Stats <- as.data.table(MA_Ov_Stats[order(MA_Ov_Stats$ManagedAreaName,
                                               MA_Ov_Stats$ShellType,
                                               MA_Ov_Stats$SizeClass,
                                               MA_Ov_Stats$HabitatType), ])

# Replaces blank ProgramIDs with NA (missing values)
MA_Ov_Stats$ProgramIDs <- replace(MA_Ov_Stats$ProgramIDs,
                                  MA_Ov_Stats$ProgramIDs=="", NA)
MA_Ov_Stats$Programs <- replace(MA_Ov_Stats$Programs,
                                MA_Ov_Stats$Programs=="", NA)
# Write overall statistics to file
fwrite(MA_Ov_Stats, paste0(out_dir,"/Percent_Live/Oyster_PrcLive_MA_Overall_Stats.txt"),
       sep="|")
# Removes variable storing data to improve computer memory
rm(MA_Ov_Stats)

#Plotting ----
# LiveDate Threshold -----------------------------------------------------
oysterraw <- oysterraw[oysterraw$LiveDate>=1960,]
for(i in unique(oysterraw$ManagedAreaName)){
      oysterraw[ManagedAreaName==i & !is.na(LiveDate), `:=`
                (RelYear=(LiveDate-min(LiveDate))+1,
                  YearDiff=min(LiveDate)-1)]
}

# Plot theme and setup -----
plot_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(family="Arial"),
        plot.title=element_text(hjust=0.5, size=12, color="#314963"),
        plot.subtitle=element_text(hjust=0.5, size=10, color="#314963"),
        legend.title=element_text(size=10),
        legend.text = element_text(hjust = 0),
        axis.title.x = element_text(size=10, margin = margin(t = 5, r = 0,
                                                             b = 10, l = 0)),
        axis.title.y = element_text(size=10, margin = margin(t = 0, r = 10,
                                                             b = 0, l = 0)),
        axis.text=element_text(size=10),
        axis.text.x=element_text(angle = -45, hjust = 0))

plot_jitter <- position_jitter(width = 0.1, height = 0.1, seed=42)

# Parallel, num cores setup
ncores <- 4
nchains <- 4
threads <- 10
iter <- 3000
warmup <- 1000

# At least 5 years of data are required in order to run model analyses
# Function checks N years of data, returns T or F
suff_years <- function(data){length(unique(data$Year))>=5}

## Apalachicola Bay Aquatic Preserve_Natural ----------------------------------------

#Exclude the five samples that don't have counts less than the "NumberMeasured"
#value for the corresponding program (see variable exploration graphs in the
#25to75mm section for the rationale and graphs for this step.)
numValves <- unique(oysterraw[, c("ProgramID", "RelYear", "counts",
                                   "QuadIdentifier", "Subtidal", "QuadSize_m2",
                                   "LiveDate_Qualifier", "NumberMeasured_n")])
exclude_samps <- subset(numValves, numValves$NumberMeasured_n=="20" &
                          numValves$counts > 19)$QuadIdentifier
ab_sho25 <- oysterraw[!is.na(ShellHeight_mm) & 
                         ShellHeight_mm >= 25 &
                         MA_plotlab=="Apalachicola Bay Aquatic Preserve_Natural" & 
                         QuadIdentifier %in% setdiff(
                           oysterraw[!is.na(ShellHeight_mm) &
                                        ManagedAreaName==
                                        "Apalachicola Bay Aquatic Preserve",
                                      QuadIdentifier], exclude_samps), ]

saveRDS(ab_sho25, paste0('output/model_results/data/ab_sho25_', Sys.Date(), '.rds'))


### abap-25 to 75mm -------------------------------------------------------

ab_sh25to75 <- ab_sho25[ShellHeight_mm < 75, ]
saveRDS(ab_sh25to75, paste0('output/model_results/data/ab_sh25to75_', Sys.Date(), '.rds'))

if(suff_years(subset(ab_sh25to75, ab_sh25to75$LiveDate_Qualifier!="Estimate"))){
  ab_sh25to75_glmm <- brm(formula=ShellHeight_mm | trunc(lb=25, ub=75) ~
                            RelYear+QuadSize_m2+(1 | UniversalReefID),
                          data=subset(ab_sh25to75, ab_sh25to75$LiveDate_Qualifier!="Estimate"),
                          family=gaussian, cores=ncores,
                          control=list(adapt_delta=0.995, max_treedepth=20),
                          iter=iter, warmup=warmup, chains=nchains, thin=3, seed=5699,
                          backend="rstan", threads=threading(threads),
                          file="output/model_results/GLMMs/ab_sh25to75_glmm4b.rds")
  
  models1 <- list(ab_sh25to75_glmm)
} else {models1 <- NULL}

# Create model results tables and save diagnostic plots
data1 <- ab_sh25to75

### ABAP->75mm ------------------------------------------------------------

ab_sho75 <- ab_sho25[ShellHeight_mm >= 75, ]

saveRDS(ab_sho75, paste0('output/model_results/data/ab_sho75_', Sys.Date(), '.rds'))

if(suff_years(subset(ab_sho75, ab_sho75$LiveDate_Qualifier!="Estimate"))){
  ab_sho75_glmm <- brm(formula=ShellHeight_mm | trunc(lb=75, ub=250) ~
                         RelYear+(1 | UniversalReefID),
                       data=subset(ab_sho75, 
                                   ab_sho75$LiveDate_Qualifier!="Estimate"), 
                       family=gaussian, cores=ncores,
                       control= list(adapt_delta=0.995, max_treedepth=20),
                       iter=iter, warmup=warmup, chains=nchains, thin=3, seed=3639,
                       backend="rstan", threads=threading(threads),
                       file="output/model_results/GLMMs/ab_sho75_glmm4b.rds")
  
  models2 <- list(ab_sho75_glmm)
} else {models2 <- NULL}

# Create model results tables and save diagnostic plots and marginal effects plots
data2 <- ab_sho75
modresultssh(data1, models1, data2, models2, "Size class", meplotzoom=FALSE)

## Apalachicola National Estuarine Research Reserve_Natural -------------------

an_sho25 <- oysterraw[!is.na(ShellHeight_mm) & 
                        !is.na(LiveDate) &
                        ShellHeight_mm >= 25 &
                        MA_plotlab==
                        "Apalachicola National Estuarine Research Reserve_Natural" & 
                        QuadIdentifier %in%
                        setdiff(oysterraw[!is.na(ShellHeight_mm) &
                                            ManagedAreaName==
                                            "Apalachicola National Estuarine Research Reserve",
                                          QuadIdentifier], exclude_samps), ]

saveRDS(an_sho25, paste0('output/model_results/data/an_sho25_', Sys.Date(), '.rds'))


### ANERR-25 to 75mm -------------------------------------------------------

an_sh25to75 <- subset(an_sho25, an_sho25$ShellHeight_mm < 75)

saveRDS(an_sh25to75, paste0('output/model_results/data/an_sh25to75_',
                            Sys.Date(), '.rds'))

if(suff_years(subset(an_sh25to75, an_sh25to75$LiveDate_Qualifier!="Estimate"))){
  an_sh25to75_glmm <- brm(formula=ShellHeight_mm | trunc(lb=25, ub=75) ~
                            RelYear+QuadSize_m2+(1 | UniversalReefID),
                          data=subset(an_sh25to75, an_sh25to75$LiveDate_Qualifier!="Estimate"),
                          family=gaussian, cores=ncores,
                          control=list(adapt_delta=0.995, max_treedepth=20),
                          iter=iter, warmup=warmup, chains=nchains, thin=3, seed=5699,
                          backend="rstan", threads=threading(threads),
                          file="output/model_results/GLMMs/an_sh25to75_glmm4b.rds")
  
  models1 <- list(an_sh25to75_glmm)
} else {models1 <- NULL}

# Create model results tables and save diagnostic plots and marginal effects plots
data1 <- an_sh25to75

### ANERR->75mm -------------------------------------------------------

an_sho75 <- an_sho25[ShellHeight_mm >= 75, ]

saveRDS(an_sho75, paste0('output/model_results/data/an_sho75_', Sys.Date(), '.rds'))

if(suff_years(subset(an_sho75, an_sho75$LiveDate_Qualifier!="Estimate"))){
  an_sho75_glmm <- brm(formula=ShellHeight_mm | trunc(lb=75, ub=250) ~
                         RelYear+(1 | UniversalReefID),
                       data=subset(an_sho75, an_sho75$LiveDate_Qualifier!=
                                     "Estimate"), family=gaussian, cores=ncores,
                       control= list(adapt_delta=0.995, max_treedepth=20),
                       iter=iter, warmup=warmup, chains=nchains, thin=3, seed=3639,
                       backend="rstan", threads=threading(threads),
                       file="output/model_results/GLMMs/an_sho75_glmm4b.rds")
  models2 <- list(an_sho75_glmm)
} else {models2 <- NULL}

# Create model results tables and save diagnostic plots and marginal effects plots
data2 <- an_sho75
#modresults(data, models, "Size class", meplotzoom=TRUE)
modresultssh(data1, models1, data2, models2, "Size class", meplotzoom=FALSE)

## Estero Bay Aquatic Preserve_Natural ---------------------------------------

eb_sho25 <- oysterraw[!is.na(ShellHeight_mm) & 
                        ShellHeight_mm >= 25 &
                        MA_plotlab=="Estero Bay Aquatic Preserve_Natural", ]

saveRDS(eb_sho25, paste0('output/model_results/data/eb_sho25_', Sys.Date(), '.rds'))


### EBAP-25 to 75mm -------------------------------------------------------

eb_sh25to75 <- subset(eb_sho25, eb_sho25$ShellHeight_mm < 75)
eb_sh25to75 <- eb_sh25to75[!is.na(eb_sh25to75$QuadSize_m2),]

saveRDS(eb_sh25to75, paste0('output/model_results/data/eb_sh25to75_',
                            Sys.Date(), '.rds'))

# Does not update models because this subset of data have NA for QuadSize_m2 values
if(suff_years(subset(eb_sh25to75,eb_sh25to75$LiveDate_Qualifier=="Exact"))){
  eb_sh25to75_glmm <- brm(formula=ShellHeight_mm ~
                            RelYear+QuadSize_m2+(0+RelYear | UniversalReefID),
                          data=subset(eb_sh25to75,
                                      eb_sh25to75$LiveDate_Qualifier=="Exact"),
                          family=gaussian, cores=ncores,
                          control= list(adapt_delta=0.995, max_treedepth=20),
                          iter=iter, warmup=warmup, chains=nchains, thin=3, seed=6881,
                          backend="rstan", threads=threading(threads),
                          file="output/model_results/GLMMs/eb_sh25to75_glmm5.rds")
  
  models1 <- list(eb_sh25to75_glmm)
} else {models1 <- NULL}

# Create model results tables and save diagnostic plots and marginal effects plots
data1 <- eb_sh25to75

### EBAP->75mm -------------------------------------------------------

eb_sho75 <- eb_sho25[ShellHeight_mm >= 75, ]

saveRDS(eb_sho75, paste0('output/model_results/data/eb_sho75_', Sys.Date(), '.rds'))

if(suff_years(subset(eb_sho75, eb_sho75$LiveDate_Qualifier=="Exact"))){
  eb_sho75_glmm <- brm(formula=ShellHeight_mm ~
                         RelYear+(1 | UniversalReefID),
                       data=subset(eb_sho75, eb_sho75$LiveDate_Qualifier=="Exact"),
                       family=gaussian, cores=ncores,
                       control=list(adapt_delta=0.995, max_treedepth=20), iter=iter,
                       warmup=warmup, chains=nchains, thin=3, seed=3138,
                       backend="rstan", threads=threading(threads),
                       file="output/model_results/GLMMs/eb_sho75_glmm4.rds")
  models2 <- list(eb_sho75_glmm)
} else {models2 <- NULL}

# Create model results tables and save diagnostic plots and marginal effects plots
data2 <- eb_sho75

modresultssh(data1, models1, data2, models2, "Size class", meplotzoom=FALSE)

## Guana River Marsh Aquatic Preserve_Natural ---------------------------------

grm_sho25 <- oysterraw[!is.na(ShellHeight_mm) & 
                         ShellHeight_mm >= 25 &
                         MA_plotlab==
                         "Guana River Marsh Aquatic Preserve_Natural", ]

saveRDS(grm_sho25, paste0('output/model_results/data/grm_sho25_',
                          Sys.Date(), '.rds'))


### GRMAP-25 to 75mm -------------------------------------------------------

grm_sh25to75 <- subset(grm_sho25, grm_sho25$ShellHeight_mm < 75)

saveRDS(grm_sh25to75, paste0('output/model_results/data/grm_sh25to75_',
                             Sys.Date(), '.rds'))



if(suff_years(subset(grm_sh25to75, grm_sh25to75$LiveDate_Qualifier=="Exact"))){
  # NumberMeasred_n contains NA, removed from formula
  grm_sh25to75_glmm <- brm(formula=ShellHeight_mm | trunc(lb=25, ub=75) ~
                             RelYear+(1 | UniversalReefID),
                           data=subset(grm_sh25to75,
                                       grm_sh25to75$LiveDate_Qualifier=="Exact"),
                           family=gaussian, cores=ncores,
                           control= list(adapt_delta=0.8, max_treedepth=10),
                           iter=iter, warmup=warmup, chains=nchains, thin=3,
                           seed=3457, backend="rstan", threads=threading(threads),
                           file="output/model_results/GLMMs/grm_sh25to75_glmm4.rds")
  
  models1 <- list(grm_sh25to75_glmm)
} else {models1 <- NULL}

# Create model results tables and save diagnostic plots and marginal effects plots
data1 <- grm_sh25to75

### GRMAP->75mm -------------------------------------------------------

grm_sho75 <- grm_sho25[ShellHeight_mm >= 75, ]

saveRDS(grm_sho75, paste0('output/model_results/data/grm_sho75_',
                          Sys.Date(), '.rds'))

if(suff_years(subset(grm_sho75, grm_sho75$LiveDate_Qualifier=="Exact"))){
  grm_sho75_glmm <- brm(formula=ShellHeight_mm | trunc(lb=75, ub=250) ~
                          RelYear+(1 | UniversalReefID),
                        data=subset(grm_sho75,
                                    grm_sho75$LiveDate_Qualifier=="Exact"),
                        family=gaussian, cores=ncores,
                        control= list(adapt_delta=0.8, max_treedepth=10),
                        iter=iter, warmup=warmup, chains=nchains, thin=3,
                        seed=4352, backend="rstan", threads=threading(threads),
                        file="output/model_results/GLMMs/grm_sho75_glmm4.rds")
  models2 <- list(grm_sho75_glmm)
} else {models2 <- NULL}

# Create model results tables and save diagnostic plots and marginal effects plots
data2 <- grm_sho75
modresultssh(data1, models1, data2, models2, "Size class", meplotzoom=FALSE)

## Guana Tolomato Matanzas National Estuarine Research Reserve_Natural --------

gtmn_sho25 <- oysterraw[!is.na(ShellHeight_mm) & 
                          ShellHeight_mm >= 25 &
                          MA_plotlab==
                          "Guana Tolomato Matanzas National Estuarine Research Reserve_Natural", ]

saveRDS(gtmn_sho25, paste0('output/model_results/data/gtmn_sho25_',
                           Sys.Date(), '.rds'))

### GTMNERR-25 to 75mm -------------------------------------------------------

gtmn_sh25to75 <- subset(gtmn_sho25, gtmn_sho25$ShellHeight_mm < 75)

saveRDS(gtmn_sh25to75, paste0('output/model_results/data/gtmn_sh25to75_',
                              Sys.Date(), '.rds'))

if(suff_years(subset(gtmn_sh25to75, gtmn_sh25to75$LiveDate_Qualifier != "Estimate"))){
  gtmn_sh25to75_glmm <- brm(formula=ShellHeight_mm | trunc(lb=25, ub=75) ~
                              RelYear+(1 | UniversalReefID),
                            data=subset(gtmn_sh25to75,
                                        gtmn_sh25to75$LiveDate_Qualifier != "Estimate"),
                            family=gaussian, cores=ncores,
                            control=list(adapt_delta=0.8, max_treedepth=20),
                            iter=iter, warmup=warmup, chains=nchains, thin=3,
                            seed=7844, backend="rstan", threads=threading(threads),
                            file="output/model_results/GLMMs/gtmn_sh25to75_glmm5.rds")
  
  models1 <- list(gtmn_sh25to75_glmm)
} else {models1 <- NULL}

# Create model results tables and save diagnostic plots and marginal effects plots
data1 <- gtmn_sh25to75

### GTMNERR->75mm -------------------------------------------------------

gtmn_sho75 <- gtmn_sho25[ShellHeight_mm >= 75, ]

saveRDS(gtmn_sho75, paste0('output/model_results/data/gtmn_sho75_',
                           Sys.Date(), '.rds'))

if(suff_years(subset(gtmn_sho75, gtmn_sho75$LiveDate_Qualifier != "Estimate"))){
  gtmn_sho75_glmm <- brm(formula=ShellHeight_mm | trunc(lb=75, ub=250) ~
                           RelYear + (1 | UniversalReefID),
                         data=subset(gtmn_sho75,
                                     gtmn_sho75$LiveDate_Qualifier != "Estimate"),
                         family=gaussian,
                         cores=ncores, control=list(adapt_delta=0.995, max_treedepth=20),
                         iter=iter, warmup=warmup, chains=nchains, thin=3,
                         seed=5332, backend="rstan", threads=threading(threads),
                         file="output/model_results/GLMMs/gtmn_sho75_glmm6.rds")
  models2 <- list(gtmn_sho75_glmm)
} else {models2 <- NULL}

# Create model results tables and save diagnostic plots and marginal effects plots
data2 <- gtmn_sho75
modresultssh(data1, models1, data2, models2, "Size class", meplotzoom=FALSE)

## Indian River-Vero Beach to Ft. Pierce Aquatic Preserve_Natural -------------
irvbfp_sho25 <- oysterraw[!is.na(ShellHeight_mm) & 
                            ShellHeight_mm >= 25 &
                            MA_plotlab==
                            "Indian River-Vero Beach to Ft. Pierce Aquatic Preserve_Natural", ]

saveRDS(irvbfp_sho25, paste0('output/model_results/data/irvbfp_sho25_',
                             Sys.Date(), '.rds'))


### IRVBFPAP-25 to 75mm -------------------------------------------------------

irvbfp_sh25to75 <- subset(irvbfp_sho25, irvbfp_sho25$ShellHeight_mm < 75)

saveRDS(irvbfp_sh25to75, paste0('output/model_results/data/irvbfp_sh25to75_',
                                Sys.Date(), '.rds'))

if(suff_years(subset(irvbfp_sh25to75, irvbfp_sh25to75$LiveDate_Qualifier != "Estimate"))){
  irvbfp_sh25to75_glmm <- brm(formula=ShellHeight_mm | trunc(lb=25, ub=75) ~ 
                                RelYear+(1 | UniversalReefID),
                              data=subset(irvbfp_sh25to75,
                                          irvbfp_sh25to75$LiveDate_Qualifier != "Estimate"),
                              family=gaussian, cores=ncores,
                              control=list(adapt_delta=0.8, max_treedepth=20),
                              iter=iter, warmup=warmup, chains=nchains, thin=3,
                              seed=7844, backend="rstan", threads=threading(threads),
                              file="output/model_results/GLMMs/irvbfp_sh25to75_glmm.rds")
  
  models1 <- list(irvbfp_sh25to75_glmm)
} else {models1 <- NULL}

# Create model results tables and save diagnostic plots and marginal effects plots
data1 <- irvbfp_sh25to75

### IRVBFPAP->75mm -------------------------------------------------------

irvbfp_sho75 <- irvbfp_sho25[ShellHeight_mm >= 75, ]

saveRDS(irvbfp_sho75, paste0('output/model_results/data/irvbfp_sho75_',
                             Sys.Date(), '.rds'))

if(suff_years(subset(irvbfp_sho75, irvbfp_sho75$LiveDate_Qualifier != "Estimate"))){
  irvbfp_sho75_glmm <- brm(formula=ShellHeight_mm | trunc(lb=75, ub=250) ~
                             RelYear + (1 | UniversalReefID),
                           data=subset(irvbfp_sho75,
                                       irvbfp_sho75$LiveDate_Qualifier != "Estimate"),
                           family=gaussian,
                           cores=ncores, control=list(adapt_delta=0.995, max_treedepth=20),
                           iter=iter, warmup=warmup, chains=nchains, thin=3,
                           seed=5332, backend="rstan", threads=threading(threads),
                           file="output/model_results/GLMMs/irvbfp_sho75_glmm.rds")
  models2 <- list(irvbfp_sho75_glmm)
} else {models2 <- NULL}

# Create model results tables and save diagnostic plots and marginal effects plots
data2 <- irvbfp_sho75
modresultssh(data1, models1, data2, models2, "Size class", meplotzoom=FALSE)

## Lemon Bay Aquatic Preserve_Natural ---------------------------------------

lb_sho25 <- oysterraw[!is.na(ShellHeight_mm) & 
                        ShellHeight_mm >= 25 &
                        MA_plotlab=="Lemon Bay Aquatic Preserve_Natural", ]

saveRDS(lb_sho25, paste0('output/model_results/data/lb_sho25_',
                         Sys.Date(), '.rds'))

### LBAP-25 to 75mm -------------------------------------------------------

lb_sh25to75 <- subset(lb_sho25, lb_sho25$ShellHeight_mm < 75)

saveRDS(lb_sh25to75, paste0('output/model_results/data/lb_sh25to75_', Sys.Date(), '.rds'))

if(suff_years(subset(lb_sh25to75, lb_sh25to75$LiveDate_Qualifier != "Estimate"))){
  lb_sh25to75_glmm <- brm(formula=ShellHeight_mm | trunc(lb=25, ub=75) ~
                            RelYear+(1 | UniversalReefID),
                          data=subset(lb_sh25to75,
                                      lb_sh25to75$LiveDate_Qualifier != "Estimate"),
                          family=gaussian, cores=ncores,
                          control=list(adapt_delta=0.8, max_treedepth=20),
                          iter=iter, warmup=warmup, chains=nchains, thin=3,
                          seed=7844, backend="rstan", threads=threading(threads),
                          file="output/model_results/GLMMs/lb_sh25to75_glmm.rds")
  
  models1 <- list(lb_sh25to75_glmm)
} else {models1 <- NULL}

# Create model results tables and save diagnostic plots and marginal effects plots
data1 <- lb_sh25to75

### LBAP->75mm -------------------------------------------------------

lb_sho75 <- lb_sho25[ShellHeight_mm >= 75, ]

saveRDS(lb_sho75, paste0('output/model_results/data/lb_sho75_', Sys.Date(), '.rds'))

if(suff_years(subset(lb_sho75, lb_sho75$LiveDate_Qualifier != "Estimate"))){
  lb_sho75_glmm <- brm(formula=ShellHeight_mm | trunc(lb=25, ub=75) ~
                            RelYear+(1 | UniversalReefID),
                          data=subset(lb_sho75,
                                      lb_sho75$LiveDate_Qualifier != "Estimate"),
                          family=gaussian, cores=ncores,
                          control=list(adapt_delta=0.8, max_treedepth=20),
                          iter=iter, warmup=warmup, chains=nchains, thin=3,
                          seed=7844, backend="rstan", threads=threading(threads),
                          file="output/model_results/GLMMs/lb_sho75_glmm.rds")
  
  models2 <- list(lb_sho75_glmm)
} else {models2 <- NULL}

#Important: note that time-averaging is not accounted for in the model fit for
#the data on shell height >75mm. The measurement error approach I was taking
#did not result in any models that converged, possibly because the combination
#of the data and degree of measurement error leads to multiple possible
#solutions. This means the model reported in this section makes the unrealistic
#assumption that the estimated sample ages are exactly correct.

# Create model results tables and save diagnostic plots and marginal effects plots
data2 <- lb_sho75
modresultssh(data1, models1, data2, models2, "Size class", meplotzoom=FALSE)

##### Density -----
oysterraw$YearDiff <- oysterraw$LiveDate-oysterraw$RelYear
# #Make a collapsed version of the oysterraw table for density
oysterraw_den <- oysterraw[, c("ProgramID", "ProgramName", "LocationID",
                               "ProgramLocationID", "QuadIdentifier",
                               "ReefIdentifier", "LiveDate",
                               "LiveDate_Qualifier", "SampleDate", "Year",
                               "Month", "ManagedAreaName", "Region",
                               "SurveyMethod", "HabitatClassification",
                               "QuadSize_m2", "MADup", "Density_m2",
                               "Number_of_Oysters_Counted_Total_Count",
                               "Number_of_Oysters_Counted_Live_Count",
                               "Number_of_Oysters_Counted_Dead_Count",
                               "ObsIndex", "UniversalReefID",
                               "MA_plotlab", "Subtidal", "RelYear", "YearDiff")]
oysterraw_den[!is.na(Density_m2), DensIndex := ObsIndex]
oysterraw_den[!is.na(Number_of_Oysters_Counted_Total_Count), NTotIndex := ObsIndex]
oysterraw_den[!is.na(Number_of_Oysters_Counted_Live_Count), NLiveIndex := ObsIndex]
oysterraw_den[!is.na(Number_of_Oysters_Counted_Dead_Count), NDeadIndex := ObsIndex]
oysterraw_den[, ObsIndex := NULL]

oysterraw_den <- unique(oysterraw_den)
oysterraw_den <- oysterraw_den %>%
  dplyr::group_by(ProgramID, ProgramName, LocationID, ProgramLocationID,
                  QuadIdentifier, ReefIdentifier, LiveDate,
                  LiveDate_Qualifier, SampleDate, Year, Month,
                  ManagedAreaName, Region, SurveyMethod,
                  HabitatClassification, QuadSize_m2, MADup, UniversalReefID, 
                  MA_plotlab, Subtidal) %>%
  tidyr::fill(Density_m2, Number_of_Oysters_Counted_Total_Count,
              Number_of_Oysters_Counted_Live_Count,
              Number_of_Oysters_Counted_Dead_Count,
              DensIndex, NTotIndex, NLiveIndex, NDeadIndex) %>%
  tidyr::fill(Density_m2, Number_of_Oysters_Counted_Total_Count,
              Number_of_Oysters_Counted_Live_Count,
              Number_of_Oysters_Counted_Dead_Count,
              DensIndex, NTotIndex, NLiveIndex, NDeadIndex,
              .direction='up') %>%
  dplyr::distinct()

oysterraw_den <- subset(oysterraw_den, !is.na(oysterraw_den$Density_m2) |
                          !is.na(oysterraw_den$Number_of_Oysters_Counted_Total_Count) |
                          !is.na(oysterraw_den$Number_of_Oysters_Counted_Live_Count) |
                          !is.na(oysterraw_den$Number_of_Oysters_Counted_Dead_Count) |
                          !is.na(oysterraw_den$DensIndex) |
                          !is.na(oysterraw_den$NTotIndex) |
                          !is.na(oysterraw_den$NLiveIndex) |
                          !is.na(oysterraw_den$NDeadIndex))
setDT(oysterraw_den)

#Remove NAs in Density_m2 column
oysterraw_den <- subset(oysterraw_den, !is.na(oysterraw_den$Density_m2))

#Summarize density data by managed area
den_all_sum <- summarySE(oysterraw_den, measurevar='Density_m2',
                         groupvars=c('ManagedAreaName', 'Year'))

## Raw density results -----------------------------------------------------


### Apalachicola Bay Aquatic Preserve_Natural ----------------------------------------

ab_n <- subset(oysterraw_den,
               oysterraw_den$MA_plotlab==
                 "Apalachicola Bay Aquatic Preserve_Natural")
ab_n[, Density_m2 := as.integer(round(Density_m2))]
saveRDS(ab_n, paste0('output/model_results/data/ab_n_', Sys.Date(), '.rds'))

ab_den_glmm <- brm(formula=Density_m2 ~
                     RelYear+(0+RelYear | UniversalReefID), data=ab_n,
                   family=negbinomial, cores=ncores,
                   control= list(adapt_delta=0.995, max_treedepth=20),
                   iter=iter,
                   warmup=warmup, chains=nchains, init=0, thin=3, seed=5512,
                   backend="rstan", threads=threading(threads),
                   file="output/model_results/GLMMs/ab_den_glmm9.rds")

# ab_den_glmm <- update(ab_den_glmm, 
#                       newdata = ab_n,
#                       family=negbinomial, cores=ncores,
#                       control= list(adapt_delta=0.995, max_treedepth=20),
#                       iter=iter,
#                       warmup=warmup, chains=nchains, init=0, thin=3, seed=5512,
#                       backend="rstan", threads=threading(threads),
#                       file="output/model_results/GLMMs/ab_den_glmm9.rds")

# Create model results tables and save diagnostic plots and marginal effects plots
data <- ab_n
models <- list(ab_den_glmm) 
modresults(data, models, "Density", meplotzoom=FALSE)


### Apalachicola National Estuarine Research Reserve_Natural ----------------------------------------

an_n <- subset(oysterraw_den,
               oysterraw_den$MA_plotlab==
                 "Apalachicola National Estuarine Research Reserve_Natural")
an_n[, Density_m2 := as.integer(round(Density_m2))]
saveRDS(an_n, paste0('output/model_results/data/an_n_', Sys.Date(), '.rds'))

an_den_glmm <- brm(formula=Density_m2 ~
                     RelYear+Subtidal+(0+RelYear | UniversalReefID),
                   data=an_n, family=zero_inflated_negbinomial, cores=ncores,
                   control= list(adapt_delta=0.995, max_treedepth=20), iter=iter,
                   warmup=warmup, chains=nchains, init=0, thin=3, seed=4677,
                   backend="rstan", threads=threading(threads),
                   file="output/model_results/GLMMs/an_den_glmm11.rds")

# an_den_glmm <- update(an_den_glmm, 
#                       newdata = an_n,
#                       family=zero_inflated_negbinomial, cores=ncores,
#                       control= list(adapt_delta=0.995, max_treedepth=20), iter=iter,
#                       warmup=warmup, chains=nchains, init=0, thin=3, seed=4677,
#                       backend="rstan", threads=threading(threads),
#                       file="output/model_results/GLMMs/an_den_glmm11.rds")

# Create model results tables and save diagnostic plots and marginal effects plots
data <- an_n
models <- list(an_den_glmm) 
modresults(data, models, "Density", meplotzoom=FALSE)

### Estero Bay Aquatic Preserve_Natural ----------------------------------------

eb_n <- subset(oysterraw_den,
               oysterraw_den$MA_plotlab=="Estero Bay Aquatic Preserve_Natural")
eb_n[, Density_m2 := as.integer(round(Density_m2))]
saveRDS(eb_n, paste0('output/model_results/data/eb_n_', Sys.Date(), '.rds'))

eb_den_glmm <- brm(formula=Density_m2 ~
                     RelYear+(1 | UniversalReefID), data=eb_n,
                   family=zero_inflated_negbinomial, cores=ncores,
                   control= list(adapt_delta=0.995, max_treedepth=20), iter=iter,
                   warmup=warmup, chains=nchains, init=0, thin=3, seed=1298,
                   backend="rstan", threads=threading(threads),
                   file="output/model_results/GLMMs/eb_den_glmm10.rds")

# eb_den_glmm <- update(eb_den_glmm, 
#                       newdata = eb_n,
#                       family=zero_inflated_negbinomial, cores=ncores,
#                       control= list(adapt_delta=0.995, max_treedepth=20), iter=iter,
#                       warmup=warmup, chains=nchains, init=0, thin=3, seed=1298,
#                       backend="rstan", threads=threading(threads),
#                       file="output/model_results/GLMMs/eb_den_glmm10.rds")

# Create model results tables and save diagnostic plots and marginal effects plots
data <- eb_n
models <- list(eb_den_glmm) 
modresults(data, models, "Density", meplotzoom=FALSE)

### Guana River Marsh Aquatic Preserve_Natural ----------------------------------------

grm_n <- subset(oysterraw_den,
                oysterraw_den$MA_plotlab==
                  "Guana River Marsh Aquatic Preserve_Natural")
grm_n[, Density_m2 := as.integer(round(Density_m2))]
saveRDS(grm_n, paste0('output/model_results/data/grm_n_',
                      Sys.Date(), '.rds'))

grm_den_glmm <- brm(formula=Density_m2 ~
                      RelYear+(1 | UniversalReefID), data=grm_n,
                    family=zero_inflated_negbinomial, cores=2,
                    control= list(adapt_delta=0.995, max_treedepth=20),
                    iter=iter, warmup=warmup, chains=nchains, init=0, thin=3,
                    seed=9875, backend="rstan", threads=threading(threads),
                    file="output/model_results/GLMMs/grm_den_glmm6.rds")

# grm_den_glmm <- update(grm_den_glmm, 
#                        newdata = grm_n,
#                        family=zero_inflated_negbinomial, cores=2,
#                        control= list(adapt_delta=0.995, max_treedepth=20),
#                        iter=iter, warmup=warmup, chains=nchains, init=0, thin=3,
#                        seed=9875, backend="rstan", threads=threading(threads),
#                        file="output/model_results/GLMMs/grm_den_glmm6.rds")

# Create model results tables and save diagnostic plots and marginal effects plots
data <- grm_n
models <- list(grm_den_glmm) 
modresults(data, models, "Density", meplotzoom=FALSE)

### Guana Tolomato Matanzas National Estuarine Research Reserve_Natural ----------------------------------------

gtmn_n <- subset(oysterraw_den,
                 oysterraw_den$MA_plotlab==
                   "Guana Tolomato Matanzas National Estuarine Research Reserve_Natural")
gtmn_n[, Density_m2 := as.integer(round(Density_m2))]
saveRDS(gtmn_n, paste0('output/model_results/data/gtmn_n_', Sys.Date(), '.rds'))

gtmn_den_glmm <- brm(formula=Density_m2 ~
                       RelYear+(1 | UniversalReefID),
                     data=gtmn_n, family=zero_inflated_negbinomial, cores=ncores,
                     control= list(adapt_delta=0.995, max_treedepth=20),
                     iter=iter, warmup=warmup, chains=nchains, init=0, thin=3,
                     seed=3647, backend="rstan", threads=threading(threads),
                     file="output/model_results/GLMMs/gtmn_den_glmm18.rds")

# gtmn_den_glmm <- update(gtmn_den_glmm, 
#                         newdata = gtmn_n,
#                         family=zero_inflated_negbinomial, cores=ncores,
#                         control= list(adapt_delta=0.995, max_treedepth=20),
#                         iter=iter, warmup=warmup, chains=nchains, init=0, thin=3,
#                         seed=3647, backend="rstan", threads=threading(threads),
#                         file="output/model_results/GLMMs/gtmn_den_glmm18.rds")

# Create model results tables and save diagnostic plots and marginal effects plots
data <- gtmn_n
models <- list(gtmn_den_glmm) 
modresults(data, models, "Density", meplotzoom=FALSE)

### Indian River-Vero Beach to Ft. Pierce Aquatic Preserve_Natural ----------------------------------------

irvb_n <- subset(oysterraw_den,
                 oysterraw_den$MA_plotlab==
                   "Indian River-Vero Beach to Ft. Pierce Aquatic Preserve_Natural")
irvb_n[, Density_m2 := as.integer(round(Density_m2))]
saveRDS(irvb_n, paste0('output/model_results/data/irvb_n_',
                       Sys.Date(), '.rds'))

irvb_den_glmm <- brm(formula=Density_m2 ~
                       RelYear+(0+RelYear | UniversalReefID),
                     data=irvb_n,
                     family=negbinomial, cores=ncores,
                     control= list(adapt_delta=0.995, max_treedepth=20),
                     iter=iter,
                     warmup=warmup, chains=nchains, init=0, thin=3, seed=5512,
                     backend="rstan", threads=threading(threads),
                     file="output/model_results/GLMMs/irvb_den_glmm9.rds")

# irvb_den_glmm <- update(irvb_den_glmm, 
#                         newdata = irvb_n,
#                         family=negbinomial, cores=ncores,
#                         control= list(adapt_delta=0.995, max_treedepth=20),
#                         iter=iter,
#                         warmup=warmup, chains=nchains, init=0, thin=3, seed=5512,
#                         backend="rstan", threads=threading(threads),
#                         file="output/model_results/GLMMs/irvb_den_glmm9.rds")

# Create model results tables and save diagnostic plots and marginal effects plots
data <- irvb_n
models <- list(irvb_den_glmm) 
modresults(data, models, "Density", meplotzoom=FALSE)

### Jensen Beach to Jupiter Inlet Aquatic Preserve_Natural ----------------------------------------

jbji_n <- subset(oysterraw_den,
                 oysterraw_den$MA_plotlab==
                   "Jensen Beach to Jupiter Inlet Aquatic Preserve_Natural")
jbji_n[, Density_m2 := as.integer(round(Density_m2))]
saveRDS(jbji_n, paste0('output/model_results/data/jbji_n_',
                       Sys.Date(), '.rds'))

jbji_den_glmm <- brm(formula=Density_m2 ~
                       RelYear+(0+RelYear | UniversalReefID),
                     data=jbji_n,
                     family=negbinomial, cores=ncores,
                     control= list(adapt_delta=0.995, max_treedepth=20),
                     iter=iter,
                     warmup=warmup, chains=nchains, init=0, thin=3, seed=5512,
                     backend="rstan", threads=threading(threads),
                     file="output/model_results/GLMMs/jbji_den_glmm9.rds")

# jbji_den_glmm <- update(irvb_den_glmm, 
#                         newdata = jbji_n,
#                         family=negbinomial, cores=ncores,
#                         control= list(adapt_delta=0.995, max_treedepth=20),
#                         iter=iter,
#                         warmup=warmup, chains=nchains, init=0, thin=3, seed=5512,
#                         backend="rstan", threads=threading(threads),
#                         file="output/model_results/GLMMs/jbji_den_glmm9.rds")

# Create model results tables and save diagnostic plots and marginal effects plots
data <- jbji_n
models <- list(jbji_den_glmm) 
modresults(data, models, "Density", meplotzoom=FALSE)

### Lemon Bay Aquatic Preserve_Natural ----------------------------------------

lb_n <- subset(oysterraw_den,
               oysterraw_den$MA_plotlab=="Lemon Bay Aquatic Preserve_Natural")
lb_n[, Density_m2 := as.integer(round(Density_m2))]
saveRDS(lb_n, paste0('output/model_results/data/lb_n_', Sys.Date(), '.rds'))

lb_den_glmm <- brm(formula=Density_m2 ~
                     RelYear+(1 | ReefIdentifier), data=lb_n,
                   family=zero_inflated_negbinomial, cores=2,
                   control= list(adapt_delta=0.995, max_treedepth=20),
                   iter=iter, warmup=warmup, chains=nchains, init=0, thin=3,
                   seed=4612, backend="rstan", threads=threading(threads),
                   file="output/model_results/GLMMs/lb_den_glmm6.rds")

# lb_den_glmm <- update(lb_den_glmm, 
#                       newdata = lb_n,
#                       family=zero_inflated_negbinomial, cores=2,
#                       control= list(adapt_delta=0.995, max_treedepth=20),
#                       iter=iter, warmup=warmup, chains=nchains, init=0, thin=3,
#                       seed=4612, backend="rstan", threads=threading(threads),
#                       file="output/model_results/GLMMs/lb_den_glmm6.rds")

# Create model results tables and save diagnostic plots and marginal effects plots
data <- lb_n
models <- list(lb_den_glmm) 
modresults(data, models, "Density", meplotzoom=FALSE)

### Pine Island Sound Aquatic Preserve_Natural ----------------------------------------

oysterraw_den[str_detect(MA_plotlab, "Pine Island Sound"), `:=`
              (MA_plotlab=ifelse(str_detect(ProgramLocationID, "Reference") |
                                   str_detect(ProgramLocationID, "Control"),
                                 "Pine Island Sound Aquatic Preserve_Natural",
                                 "Pine Island Sound Aquatic Preserve_Restored"),
                HabitatClassification=ifelse(str_detect(ProgramLocationID,
                                                        "Reference") |
                                               str_detect(ProgramLocationID,
                                                          "Control"),
                                             "Natural", "Restored"))]
pis_n <- subset(oysterraw_den,
                oysterraw_den$MA_plotlab==
                  "Pine Island Sound Aquatic Preserve_Natural")
pis_n[, `:=` (Density_m2=as.integer(round(Density_m2)),
              Treatment=ifelse(UniversalReefID==170711,
                               "Reference", "Control"))]
saveRDS(pis_n, paste0('output/model_results/data/pis_n_', Sys.Date(), '.rds'))

pis_den_glmm <- brm(formula=Density_m2 ~
                      RelYear+(0+RelYear | UniversalReefID),
                    data=pis_n, family=zero_inflated_negbinomial, cores=ncores,
                    control= list(adapt_delta=0.995, max_treedepth=20),
                    iter=iter, warmup=warmup, chains=nchains, init=0, thin=3,
                    seed=5243, backend="rstan", threads=threading(threads),
                    file="output/model_results/GLMMs/pis_den_glmm9.rds")

# pis_den_glmm <- update(pis_den_glmm, 
#                        newdata = pis_n,
#                        family=zero_inflated_negbinomial, cores=ncores,
#                        control= list(adapt_delta=0.995, max_treedepth=20),
#                        iter=iter, warmup=warmup, chains=nchains, init=0, thin=3,
#                        seed=5243, backend="rstan", threads=threading(threads),
#                        file="output/model_results/GLMMs/pis_den_glmm9.rds")

# Create model results tables and save diagnostic plots and marginal effects plots
data <- pis_n
models <- list(pis_den_glmm) 
modresults(data, models, "Density", meplotzoom=FALSE)


### Pine Island Sound Aquatic Preserve_Restored ----------------------------------------

pisr_n <- subset(oysterraw_den,
                 oysterraw_den$MA_plotlab==
                   "Pine Island Sound Aquatic Preserve_Restored")
pisr_n[, `:=` (Density_m2=as.integer(round(Density_m2)),
               Treatment=ifelse(UniversalReefID==170711,
                                "Reference", "Control"))]
saveRDS(pisr_n, paste0('output/model_results/data/pisr_n_', Sys.Date(), '.rds'))

pisr_den_glmm <- brm(formula=Density_m2 ~
                       RelYear+QuadSize_m2, data=pisr_n,
                     family=zero_inflated_negbinomial,
                     prior=set_prior("uniform(0,5)", class="b", lb=0, ub=5),
                     cores=ncores, control= list(adapt_delta=0.995, max_treedepth=20),
                     iter=iter, warmup=warmup, chains=nchains, init=0, thin=3,
                     seed=8441, backend="rstan", threads=threading(threads),
                     file="output/model_results/GLMMs/pisr_den_glmm12.rds")

# pisr_den_glmm <- update(pisr_den_glmm, 
#                         newdata = pisr_n,
#                         family=zero_inflated_negbinomial,
#                         prior=set_prior("uniform(0,5)", class="b", lb=0, ub=5),
#                         cores=ncores, control= list(adapt_delta=0.995, max_treedepth=20),
#                         iter=iter, warmup=warmup, chains=nchains, init=0, thin=3,
#                         seed=8441, backend="rstan", threads=threading(threads),
#                         file="output/model_results/GLMMs/pisr_den_glmm12.rds")

# Create model results tables and save diagnostic plots and marginal effects plots
data <- pisr_n
models <- list(pisr_den_glmm) 
modresults(data, models, "Density", meplotzoom=FALSE)

PI_R <- nrow(subset(oysterresults,
                    oysterresults$managed_area==
                      "Pine Island Sound Aquatic Preserve" &
                      oysterresults$indicator=="Density" &
                      oysterresults$habitat_class=="Restored"))

oysterresults$group[is.na(oysterresults$group)] <- NA

if(PI_R>0){
  oysterresults$group[oysterresults$managed_area==
                        "Pine Island Sound Aquatic Preserve" &
                        oysterresults$indicator=="Density" &
                        oysterresults$habitat_class=="Restored"] <-
    c(NA, NA, NA)
  
  oysterresults$term[oysterresults$managed_area==
                       "Pine Island Sound Aquatic Preserve" &
                       oysterresults$indicator=="Density" &
                       oysterresults$habitat_class=="Restored"] <-
    c("(Intercept)", "RelYear", "QuadSize_m2")
}

##### Percent Live -----

#Make a collapsed version of the oysterraw table for percent live
oysterraw_pct <- oysterraw[, c("ProgramID", "ProgramName", "ProgramLocationID",
                               "QuadIdentifier", "ReefIdentifier", "LiveDate",
                               "LiveDate_Qualifier", "SampleDate", "Year",
                               "Month", "ManagedAreaName", "Region",
                               "SurveyMethod", "PercentLiveMethod",
                               "HabitatClassification", "QuadSize_m2", "MADup",
                               "PercentLive_pct",
                               "Number_of_Oysters_Counted_Total_Count",
                               "Number_of_Oysters_Counted_Live_Count",
                               "Number_of_Oysters_Counted_Dead_Count",
                               "ObsIndex", "UniversalReefID",
                               "MA_plotlab", "Subtidal", "RelYear", "YearDiff")]
oysterraw_pct[!is.na(PercentLive_pct), PctIndex := ObsIndex]
oysterraw_pct[!is.na(Number_of_Oysters_Counted_Total_Count),
              NTotIndex := ObsIndex]
oysterraw_pct[!is.na(Number_of_Oysters_Counted_Live_Count),
              NLiveIndex := ObsIndex]
oysterraw_pct[!is.na(Number_of_Oysters_Counted_Dead_Count),
              NDeadIndex := ObsIndex]
oysterraw_pct[, ObsIndex := NULL]

oysterraw_pct <- unique(oysterraw_pct)
oysterraw_pct <- oysterraw_pct %>%
  dplyr::group_by(ProgramID, ProgramName, ProgramLocationID, QuadIdentifier,
                  ReefIdentifier, LiveDate, LiveDate_Qualifier, SampleDate,
                  Year, Month, ManagedAreaName, Region, SurveyMethod,
                  PercentLiveMethod, HabitatClassification, QuadSize_m2,
                  MADup, UniversalReefID, MA_plotlab, Subtidal,
                  RelYear) %>%
  tidyr::fill(PercentLive_pct, Number_of_Oysters_Counted_Total_Count,
              Number_of_Oysters_Counted_Live_Count,
              Number_of_Oysters_Counted_Dead_Count,
              PctIndex, NTotIndex, NLiveIndex, NDeadIndex) %>%
  tidyr::fill(PercentLive_pct, Number_of_Oysters_Counted_Total_Count,
              Number_of_Oysters_Counted_Live_Count,
              Number_of_Oysters_Counted_Dead_Count,
              PctIndex, NTotIndex, NLiveIndex, NDeadIndex, 
              .direction='up') %>%
  dplyr::distinct()

oysterraw_pct <- subset(oysterraw_pct, !is.na(oysterraw_pct$PercentLive_pct) |
                          !is.na(oysterraw_pct$Number_of_Oysters_Counted_Total_Count) |
                          !is.na(oysterraw_pct$Number_of_Oysters_Counted_Live_Count) |
                          !is.na(oysterraw_pct$Number_of_Oysters_Counted_Dead_Count) |
                          !is.na(oysterraw_pct$PctIndex) |
                          !is.na(oysterraw_pct$NTotIndex) |
                          !is.na(oysterraw_pct$NLiveIndex) |
                          !is.na(oysterraw_pct$NDeadIndex))
setDT(oysterraw_pct)

#Calculate PercentLive_pct values for some ProgramIDs where it is missing.
#Couldn't include at the start of the script because need to use the counts columns
#rather than the QuadSize_m2 column which is filled for the whole combined table.
oysterraw_pct[ProgramID==972 | ProgramID==4014 | ProgramID==4044,
              PercentLive_pct :=
                (Number_of_Oysters_Counted_Live_Count/
                   (Number_of_Oysters_Counted_Live_Count+
                      Number_of_Oysters_Counted_Dead_Count) * 100)]

#Filter NAs for PercentLive_pct (these are related to 1) programs that do
#counts to measure density, but do not estimate percent live and
#2) Programs that are listed as measuring percent live by a Point-intercept
#method, which cannot be calculated from counts.
oysterraw_pct <- oysterraw_pct[!is.na(PercentLive_pct), ]

#Add column of decimal versions of percent live values
oysterraw_pct[, PercentLive_dec := PercentLive_pct/100] 

#Summarize percent live values
pct_all_sum <- summarySE(oysterraw_pct, measurevar='PercentLive_pct',
                         groupvars=c('ManagedAreaName', 'Year', 'PercentLiveMethod'))


## Apalachicola Bay Aquatic Preserve_Natural ----------------------------------------

abap_p <- subset(oysterraw_pct,
                 oysterraw_pct$MA_plotlab==
                   "Apalachicola Bay Aquatic Preserve_Natural")
saveRDS(abap_p, paste0('output/model_results/data/abap_p_', Sys.Date(), '.rds'))

abap_p_binom <- data.table(ProgramID=character(), ProgramLocationID=character(),
                           QuadIdentifier=character(), Year=integer(),
                           ManagedAreaName=character(),
                           PercentLiveMethod=character(),
                           UniversalReefID=factor(), Region=character(),
                           MA_plotlab=character(), RelYear=integer(),
                           PercentLive_pct=numeric(), LiveObs=logical())
for(i in 1:nrow(abap_p)){
  dat_i <- abap_p[i, c("ProgramID", "ProgramLocationID", "QuadIdentifier",
                       "Year", "ManagedAreaName", "PercentLiveMethod",
                       "UniversalReefID", "Region", "MA_plotlab", "RelYear",
                       "PercentLive_pct")]
  dat_l <- purrr::map_dfr(seq_len(round(dat_i$PercentLive_pct[1], digits=0)),
                          ~dat_i[, LiveObs := 1])
  dat_nl <- purrr::map_dfr(seq_len((100-round(dat_i$PercentLive_pct[1],
                                              digits=0))),
                           ~dat_i[, LiveObs := 0])
  dat <- rbind(dat_l, dat_nl)
  abap_p_binom <- rbind(abap_p_binom, dat)
}
saveRDS(abap_p_binom,
        paste0('output/model_results/data/abap_p_binom_',
               Sys.Date(), '.rds'))

abap_pct_glmm <- brm(formula=LiveObs ~ RelYear+(1 | UniversalReefID),
                     data=abap_p_binom, family=bernoulli, cores=ncores,
                     control= list(adapt_delta=0.995,
                                   max_treedepth=20),
                     iter=iter, warmup=warmup, chains=nchains, init=0,
                     thin=3,
                     seed=4331, backend="rstan",
                     threads=threading(threads),
                     file="output/model_results/GLMMs/abap_pct_glmm3.rds")

# abap_pct_glmm <- update(abap_pct_glmm, 
#                         newdata = abap_p_binom,
#                         family=bernoulli, cores=ncores,
#                         control= list(adapt_delta=0.995,
#                                       max_treedepth=20),
#                         iter=iter, warmup=warmup, chains=nchains, init=0,thin=3,
#                         seed=4331, backend="rstan", threads=threading(threads),
#                         file="output/model_results/GLMMs/abap_pct_glmm3.rds")

# Create model results tables and save diagnostic plots and marginal effects plots
data <- abap_p
models <- list(abap_pct_glmm) 
modresults(data, models, "Percent live", meplotzoom=FALSE)

## Apalachicola National Estuarine Research Reserve_Natural ----------------------------------------

anerr_p <- subset(oysterraw_pct,
                  oysterraw_pct$MA_plotlab==
                    "Apalachicola National Estuarine Research Reserve_Natural")
saveRDS(anerr_p, paste0('output/model_results/data/anerr_p_', Sys.Date(), '.rds'))

anerr_p_binom <- data.table(ProgramID=character(), ProgramLocationID=character(),
                            QuadIdentifier=character(), Year=integer(),
                            ManagedAreaName=character(),
                            PercentLiveMethod=character(),
                            UniversalReefID=factor(), Region=character(),
                            MA_plotlab=character(), RelYear=integer(),
                            PercentLive_pct=numeric(), LiveObs=logical())
for(i in 1:nrow(anerr_p)){
  dat_i <- anerr_p[i, c("ProgramID", "ProgramLocationID", "QuadIdentifier",
                        "Year", "ManagedAreaName", "PercentLiveMethod",
                        "UniversalReefID", "Region", "MA_plotlab", "RelYear",
                        "PercentLive_pct")]
  dat_l <- purrr::map_dfr(seq_len(round(dat_i$PercentLive_pct[1], digits=0)),
                          ~dat_i[, LiveObs := 1])
  dat_nl <- purrr::map_dfr(seq_len((100-round(dat_i$PercentLive_pct[1],
                                              digits=0))),
                           ~dat_i[, LiveObs := 0])
  dat <- rbind(dat_l, dat_nl)
  anerr_p_binom <- rbind(anerr_p_binom, dat)
}
saveRDS(anerr_p_binom,
        paste0('output/model_results/data/anerr_p_binom_',
               Sys.Date(), '.rds'))

anerr_pct_glmm <- brm(formula=LiveObs ~ RelYear+(1 | UniversalReefID),
                      data=anerr_p_binom, family=bernoulli, cores=ncores,
                      control= list(adapt_delta=0.995,
                                    max_treedepth=20),
                      iter=iter, warmup=warmup, chains=nchains, init=0,
                      thin=3,
                      seed=4331, backend="rstan",
                      threads=threading(threads),
                      file="output/model_results/GLMMs/anerr_pct_glmm3.rds")

# anerr_pct_glmm <- update(anerr_pct_glmm, 
#                          newdata = anerr_p_binom,
#                          family=bernoulli, cores=ncores,
#                          control= list(adapt_delta=0.995,
#                                        max_treedepth=20),
#                          iter=iter, warmup=warmup, chains=nchains, init=0, thin=3,
#                          seed=4331, backend="rstan", threads=threading(threads),
#                          file="output/model_results/GLMMs/anerr_pct_glmm3.rds")

# Create model results tables and save diagnostic plots and marginal effects plots
data <- anerr_p
models <- list(anerr_pct_glmm) 
modresults(data, models, "Percent live", meplotzoom=FALSE)

## Guana River Marsh Aquatic Preserve_Natural ----------------------------------------

grm_p <- subset(oysterraw_pct,
                oysterraw_pct$MA_plotlab==
                  "Guana River Marsh Aquatic Preserve_Natural")
saveRDS(grm_p, paste0('output/model_results/data/grm_p_', Sys.Date(), '.rds'))

grm_p_binom <- data.table(ProgramID=character(), ProgramLocationID=character(),
                          QuadIdentifier=character(), Year=integer(),
                          ManagedAreaName=character(),
                          PercentLiveMethod=character(),
                          UniversalReefID=factor(), Region=character(),
                          MA_plotlab=character(), RelYear=integer(),
                          PercentLive_pct=numeric(), LiveObs=logical())
for(i in 1:nrow(grm_p)){
  dat_i <- grm_p[i, c("ProgramID", "ProgramLocationID", "QuadIdentifier",
                      "Year", "ManagedAreaName", "PercentLiveMethod",
                      "UniversalReefID", "Region", "MA_plotlab", "RelYear",
                      "PercentLive_pct")]
  dat_l <- purrr::map_dfr(seq_len(round(dat_i$PercentLive_pct[1], digits=0)),
                          ~dat_i[, LiveObs := 1])
  dat_nl <- purrr::map_dfr(seq_len((100-round(dat_i$PercentLive_pct[1],
                                              digits=0))),
                           ~dat_i[, LiveObs := 0])
  dat <- rbind(dat_l, dat_nl)
  grm_p_binom <- rbind(grm_p_binom, dat)
}
saveRDS(grm_p_binom, paste0('output/model_results/data/grm_p_binom_',
                            Sys.Date(), '.rds'))

grm_pct_glmm <- brm(formula=LiveObs ~ RelYear+(1 | UniversalReefID),
                    data=grm_p_binom, family=bernoulli, cores=ncores,
                    control= list(adapt_delta=0.995, max_treedepth=20),
                    iter=iter, warmup=warmup, chains=nchains, init=0, thin=3,
                    seed=4331, backend="rstan", threads=threading(threads),
                    file="output/model_results/GLMMs/grm_pct_glmm3.rds")

# grm_pct_glmm <- update(grm_pct_glmm, 
#                        newdata = grm_p_binom,
#                        family=bernoulli, cores=ncores,
#                        control= list(adapt_delta=0.995, max_treedepth=20),
#                        iter=iter, warmup=warmup, chains=nchains, init=0, thin=3,
#                        seed=4331, backend="rstan", threads=threading(threads),
#                        file="output/model_results/GLMMs/grm_pct_glmm3.rds")

# Create model results tables and save diagnostic plots and marginal effects plots
data <- grm_p
models <- list(grm_pct_glmm) 
modresults(data, models, "Percent live", meplotzoom=FALSE)

## Guana Tolomato Matanzas National Estuarine Research
## Reserve_Natural ----------------------------------------

gtm_p <- subset(oysterraw_pct,
                oysterraw_pct$MA_plotlab==
                  "Guana Tolomato Matanzas National Estuarine Research Reserve_Natural")
saveRDS(gtm_p, paste0('output/model_results/data/gtm_p_', Sys.Date(), '.rds'))

gtm_p_binom <- data.table(ProgramID=character(), ProgramLocationID=character(),
                          QuadIdentifier=character(), Year=integer(),
                          ManagedAreaName=character(),
                          PercentLiveMethod=character(),
                          UniversalReefID=factor(), Region=character(),
                          MA_plotlab=character(), RelYear=integer(),
                          PercentLive_pct=numeric(), LiveObs=logical())
for(i in 1:nrow(gtm_p)){
  dat_i <- gtm_p[i, c("ProgramID", "ProgramLocationID", "QuadIdentifier",
                      "Year", "ManagedAreaName", "PercentLiveMethod",
                      "UniversalReefID", "Region", "MA_plotlab", "RelYear",
                      "PercentLive_pct")]
  dat_l <- purrr::map_dfr(seq_len(round(dat_i$PercentLive_pct[1], digits=0)),
                          ~dat_i[, LiveObs := 1])
  dat_nl <- purrr::map_dfr(seq_len((100-round(dat_i$PercentLive_pct[1],
                                              digits=0))),
                           ~dat_i[, LiveObs := 0])
  dat <- rbind(dat_l, dat_nl)
  gtm_p_binom <- rbind(gtm_p_binom, dat)
}
saveRDS(gtm_p_binom,
        paste0('output/model_results/data/gtm_p_binom_',
               Sys.Date(), '.rds'))

gtm_pct_glmm <- brm(formula=LiveObs ~ RelYear+(1 | UniversalReefID),
                    data=gtm_p_binom, family=bernoulli, cores=ncores,
                    control= list(adapt_delta=0.995,
                                  max_treedepth=20),
                    iter=iter, warmup=warmup, chains=nchains, init=0,
                    thin=3,
                    seed=4331, backend="rstan",
                    threads=threading(threads),
                    file="output/model_results/GLMMs/gtm_pct_glmm3.rds")

# gtm_pct_glmm <- update(gtm_pct_glmm, 
#                        newdata = gtm_p_binom,
#                        family=bernoulli, cores=ncores,
#                        control= list(adapt_delta=0.995,
#                                      max_treedepth=20),
#                        iter=iter, warmup=warmup, chains=nchains, init=0, thin=3,
#                        seed=4331, backend="rstan", threads=threading(threads),
#                        file="output/model_results/GLMMs/gtm_pct_glmm3.rds")

# Create model results tables and save diagnostic plots and marginal effects plots
data <- gtm_p
models <- list(gtm_pct_glmm) 
modresults(data, models, "Percent live", meplotzoom=FALSE)


## Indian River-Vero Beach to Ft. Pierce Aquatic Preserve_Natural ----------------------------------------

irvb_p <- subset(oysterraw_pct,
                 oysterraw_pct$MA_plotlab==
                   "Indian River-Vero Beach to Ft. Pierce Aquatic Preserve_Natural")
saveRDS(irvb_p, paste0('output/model_results/data/irvb_p_', Sys.Date(), '.rds'))

irvb_p_binom <- data.table(ProgramID=character(), ProgramLocationID=character(),
                           QuadIdentifier=character(), Year=integer(),
                           ManagedAreaName=character(),
                           PercentLiveMethod=character(),
                           UniversalReefID=factor(), Region=character(),
                           MA_plotlab=character(), RelYear=integer(),
                           PercentLive_pct=numeric(), LiveObs=logical())
for(i in 1:nrow(irvb_p)){
  dat_i <- irvb_p[i, c("ProgramID", "ProgramLocationID", "QuadIdentifier",
                       "Year", "ManagedAreaName", "PercentLiveMethod",
                       "UniversalReefID", "Region", "MA_plotlab", "RelYear",
                       "PercentLive_pct")]
  dat_l <- purrr::map_dfr(seq_len(round(dat_i$PercentLive_pct[1], digits=0)),
                          ~dat_i[, LiveObs := 1])
  dat_nl <- purrr::map_dfr(seq_len((100-round(dat_i$PercentLive_pct[1],
                                              digits=0))),
                           ~dat_i[, LiveObs := 0])
  dat <- rbind(dat_l, dat_nl)
  irvb_p_binom <- rbind(irvb_p_binom, dat)
}
saveRDS(irvb_p_binom, paste0('output/model_results/data/irvb_p_binom_',
                             Sys.Date(), '.rds'))

irvb_pct_glmm <- brm(formula=LiveObs ~ RelYear+(1 | UniversalReefID),
                     data=irvb_p_binom, family=bernoulli, cores=ncores,
                     control= list(adapt_delta=0.995, max_treedepth=20),
                     iter=iter, warmup=warmup, chains=nchains, init=0, thin=3,
                     seed=4331, backend="rstan", threads=threading(threads),
                     file="output/model_results/GLMMs/irvb_pct_glmm3.rds")

# irvb_pct_glmm <- update(irvb_pct_glmm, 
#                         newdata = irvb_p_binom,
#                         family=bernoulli, cores=ncores,
#                         control= list(adapt_delta=0.995, max_treedepth=20),
#                         iter=iter, warmup=warmup, chains=nchains, init=0, thin=3,
#                         seed=4331, backend="rstan", threads=threading(threads),
#                         file="output/model_results/GLMMs/irvb_pct_glmm3.rds")

# Create model results tables and save diagnostic plots and marginal effects plots
data <- irvb_p
models <- list(irvb_pct_glmm) 
modresults(data, models, "Percent live", meplotzoom=FALSE)


## Jensen Beach to Jupiter Inlet Aquatic Preserve_Natural ----------------------------------------

jbji_p <- subset(oysterraw_pct,
                 oysterraw_pct$MA_plotlab==
                   "Jensen Beach to Jupiter Inlet Aquatic Preserve_Natural")
saveRDS(jbji_p, paste0('output/model_results/data/jbji_p_', Sys.Date(), '.rds'))

jbji_p_binom <- data.table(ProgramID=character(), ProgramLocationID=character(),
                           QuadIdentifier=character(), Year=integer(),
                           ManagedAreaName=character(),
                           PercentLiveMethod=character(),
                           UniversalReefID=factor(), Region=character(),
                           MA_plotlab=character(), RelYear=integer(),
                           PercentLive_pct=numeric(), LiveObs=logical())
for(i in 1:nrow(jbji_p)){
  dat_i <- jbji_p[i, c("ProgramID", "ProgramLocationID", "QuadIdentifier",
                       "Year", "ManagedAreaName", "PercentLiveMethod",
                       "UniversalReefID", "Region", "MA_plotlab", "RelYear",
                       "PercentLive_pct")]
  dat_l <- purrr::map_dfr(seq_len(round(dat_i$PercentLive_pct[1], digits=0)),
                          ~dat_i[, LiveObs := 1])
  dat_nl <- purrr::map_dfr(seq_len((100-round(dat_i$PercentLive_pct[1],
                                              digits=0))),
                           ~dat_i[, LiveObs := 0])
  dat <- rbind(dat_l, dat_nl)
  jbji_p_binom <- rbind(jbji_p_binom, dat)
}
saveRDS(jbji_p_binom, paste0('output/model_results/data/jbji_p_binom_',
                             Sys.Date(), '.rds'))

jbji_pct_glmm <- brm(formula=LiveObs ~ RelYear+(1 | UniversalReefID),
                     data=jbji_p_binom, family=bernoulli, cores=ncores,
                     control= list(adapt_delta=0.995, max_treedepth=20),
                     iter=iter, warmup=warmup, chains=nchains, init=0, thin=3,
                     seed=4331, backend="rstan", threads=threading(threads),
                     file="output/model_results/GLMMs/jbji_pct_glmm3.rds")

# jbji_pct_glmm <- update(jbji_pct_glmm, 
#                         newdata = jbji_p_binom,
#                         family=bernoulli, cores=ncores,
#                         control= list(adapt_delta=0.995, max_treedepth=20),
#                         iter=iter, warmup=warmup, chains=nchains, init=0, thin=3,
#                         seed=4331, backend="rstan", threads=threading(threads),
#                         file="output/model_results/GLMMs/jbji_pct_glmm3.rds")

# Create model results tables and save diagnostic plots and marginal effects plots
data <- jbji_p
models <- list(jbji_pct_glmm) 
modresults(data, models, "Percent live", meplotzoom=FALSE)

## Lemon Bay Aquatic Preserve_Natural ----------------------------------------

lb_p <- subset(oysterraw_pct,
               oysterraw_pct$MA_plotlab=="Lemon Bay Aquatic Preserve_Natural")
lb_p[, PercentLive_dec := PercentLive_pct/100]
#PercentLiveMethod=="Percent" for Lemon Bay program(s) with sufficient data,
#so cannot be modeled as binomial
saveRDS(lb_p, paste0('output/model_results/data/lb_p_', Sys.Date(), '.rds'))

lb_pct_glmm <- brm(formula=PercentLive_dec ~
                     RelYear+(0+RelYear | ReefIdentifier),
                   data=subset(lb_p, lb_p$PercentLive_dec > 0 & lb_p$PercentLive_dec < 1),family=Beta,
                   cores=ncores, control= list(adapt_delta=0.995, max_treedepth=20),
                   iter=iter, warmup=warmup, chains=nchains, init=0, thin=3, seed=8465,
                   backend="rstan", threads=threading(threads),
                   file="output/model_results/GLMMs/lb_pct_glmm6.rds")

# lb_pct_glmm <- update(lb_pct_glmm, 
#                       newdata = subset(lb_p, lb_p$PercentLive_dec > 0),
#                       family=Beta,
#                       cores=ncores, control= list(adapt_delta=0.995, max_treedepth=20),
#                       iter=iter, warmup=warmup, chains=nchains, init=0, thin=3, seed=8465,
#                       backend="rstan", threads=threading(threads),
#                       file="output/model_results/GLMMs/lb_pct_glmm6.rds")

# Create model results tables and save diagnostic plots and marginal effects plots
data <- lb_p
models <- list(lb_pct_glmm) 
modresults(data, models, "Percent live", meplotzoom=FALSE)

##### Summarization--------------------------

fwrite(oysterresults, paste0("output/GLMM_AllDates_ModelResults.csv"), sep=",")
saveRDS(oysterresults, paste0("output/GLMM_AllDates_ModelResults.rds"))

#Get Rhat values for all models to check which ones may need to be reparameterized
model_list <- unique(oysterresults$filename)
rhats_all <- data.table(filename=character(),
                        term=character(),
                        rhat=numeric())
rhats_sum <- data.table(filename=character(),
                        rhat=numeric())

for(mod in model_list){
  mod_i <- readRDS(mod)
  allrhat_i <- rhat(mod_i)
  sumrhat_i <- c(summary(mod_i)$fixed$Rhat, summary(mod_i)$spec_pars$Rhat)
  allrhat_model_i <- data.table(filename=mod,
                                term=names(allrhat_i),
                                rhat=allrhat_i)
  sumrhat_model_i <- data.table(filename=mod,
                                rhat=sumrhat_i)
  rhats_all <- rbind(rhats_all, allrhat_model_i)
  rhats_sum <- rbind(rhats_sum, sumrhat_model_i)
}

rhats_all[, rhat_r := round(rhat, 2)]
rhats_sum[, rhat_r := round(rhat, 2)]

saveRDS(rhats_all, paste0("output/rhats_all_", Sys.Date(), ".rds"))
saveRDS(rhats_sum, paste0("output/rhats_sum_", Sys.Date(), ".rds"))

models_to_check_allrhat <- unique(rhats_all[rhat_r > 1.05, filename])
models_to_check_sumrhat <- unique(rhats_sum[rhat_r > 1.05, filename])

# Zip all figures
for(p in c("Density", "Percent_Live", "Shell_Height")){
  out_dir <- paste0("output/", p)
  fig_list <- list.files(paste0(out_dir, "/Figures"), pattern=".png", full=FALSE)
  filename <- paste0("Oyster", gsub("_", "", p), "Figures")
  setwd(paste0(out_dir, "/Figures"))
  zip(filename, files=fig_list)
  setwd(wd)
}
