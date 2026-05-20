Sys.setenv(CMDSTAN = cmdstanr::cmdstan_default_path()) # Set to your local configuration of cmdstan
library(brms)
library(Rmisc)
library(stringr)
library(data.table)
library(sf)
library(tidyverse)
library(doFuture)
library(tictoc)
library(doRNG)
library(rstudioapi)
library(ggpubr)
library(SEACAR)
library(cmdstanr)

##### Which analysis to run? Select one, can only be run individually (by ManagedAreaName: "ma" or by OIMMP: "oimmp")
# analysis <- "oimmp"
# analysis <- "ma"
####

source("../SEACAR_data_location.R")

# Read in ManagedAreas reference file
MA_All <- SEACAR::ManagedAreas

# Determine settings for brms, number of warmup samples, chains, cores, iterations etc.
warmup <- 1000
iter <- 3000
nchains <- 4
ncores <- 4
nthreads <- 4

###### parallel plan set up
plan(multisession, workers = 4) #to run 4 models at a time
options(future.globals.maxSize = 2 * 1024 * 1024 * 1024)

##### If QAQCPlots is TRUE, plots will be created without running models
# Set to FALSE to run models
QAQCPlots <- FALSE
#####

# Set column name variable to differentiate between MA and OIMMP
col_name <- ifelse(analysis=="ma", "ManagedAreaName", "OIMMP")

# Set working directory to current file location (SEACAR_Trend_Analyses/Oyster)
wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

# Set output directory
out_dir <- "output"

# Set total output directory for MA and OIMMP
output_path <- paste0(out_dir, "/", col_name, "/")

# Create paths for model and figure outputs for each analysis type
for(subfolder in c("model_results", "QAQC", "tmp", "model_results/data", "model_results/GLMMs", "model_results/GLMMs/archive")){
  if(!file.exists(paste0(output_path, subfolder, "/"))) dir.create(paste0(output_path, subfolder, "/"))
}

file_in <- str_subset(list.files(seacar_data_location, full.names = TRUE),"OYSTER")
oysterraw <- fread(file_in, sep="|", na.strings=c("NULL"))
# Apply Managed Area transformation - de-concatenate MA names
oysterraw <- setDT(SEACAR::clean_managed_areas(oysterraw, "ma"))
# New OIMMP updates include samples without MA associations, exclude for these analyses currently
if(analysis=="ma"){
  oysterraw <- oysterraw[!is.na(AreaID), ]
}

oyster <- copy(oysterraw)
oysterraw2 <- tidyr::pivot_wider(oysterraw, names_from="ParameterName",
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
                  OIMMP=as.character(OIMMP),
                  SurveyMethod=as.character(SurveyMethod),
                  PercentLiveMethod=as.character(PercentLiveMethod),
                  HabitatClassification=as.character(HabitatClassification),
                  MinimumSizeMeasured_mm=as.character(MinimumSizeMeasured_mm),
                  NumberMeasured_n=as.character(NumberMeasured_n),
                  QuadSize_m2=as.numeric(QuadSize_m2),
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
oysterraw[ProgramID==4016, Density_m2_2 :=
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

# Plot labels for both MA and OIMMP
oysterraw[, MA_plotlab := paste0(ManagedAreaName, "_", HabitatClassification)]
oysterraw[, OIMMP_plotlab := paste0(OIMMP, "_", HabitatClassification)]

# Set `plotlab` variable to store plot label column name
plotlab_col <- ifelse(analysis=="ma", "MA_plotlab", "OIMMP_plotlab")

subtidal <- c(4044, 5007, 5071, 5073)
oysterraw[, Subtidal := ifelse(ProgramID %in% subtidal, 1, 0)][, Subtidal := as.logical(Subtidal)]

#Create variables for relative year and size class category for data that
#should be included in analyses and counts of live oysters measured
for(i in oysterraw[, unique(get(col_name))]){
  oysterraw[get(col_name)==i & !is.na(LiveDate), `:=`
            (RelYear=(LiveDate-min(LiveDate))+1,
              YearDiff=min(LiveDate)-1,
              #adding 1 to each RelYear to avoid min(RelYear)==0,
              #because it is used later as an index for plotting years so
              #it needs to start from 1
              SizeClass=fcase(ShellHeight_mm >= 25 &
                                ShellHeight_mm < 75, "25to75mm",
                              ShellHeight_mm >= 75, "o75mm",
                              default=NA))]
  
  oysterraw[get(col_name)==i & !is.na(LiveDate),
            counts := length(ShellHeight_mm), by=c("QuadIdentifier")]
}

# Ensure RelYear column is listed as "years"
# oysterraw$RelYear <- time_length(oysterraw$RelYear, "years")

#Remove unrealistically high shell heights from ID_5017
oysterraw <- setdiff(oysterraw, oysterraw[ProgramID==5017 & ShellHeight_mm >= 165, ])

#Create data table to save model results
oysterresults <- data.table(indicator=character(),
                            areaName=character(),
                            areaClass = col_name,
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
          by=get(plotlab_col)]
oysterraw[!is.na(PercentLive_pct), `:=` (nyrpar="PercentLive_pct",
                                         nyears=length(unique(Year))),
          by=get(plotlab_col)]
oysterraw[!is.na(ShellHeight_mm), `:=` (nyrpar="ShellHeight_mm",
                                        nyears=length(unique(Year))),
          by=get(plotlab_col)]
MAinclude <- distinct(oysterraw[, .(get(plotlab_col), nyrpar, nyears)])
# View(MAinclude[!is.na(nyrpar) & nyears >= 5, ])

### Managed Area Statistics -----
# Create a directory to store all MA stats files for each indicator
# Will be used to ensure models are run for all necessary MA/Indicator combinations
ma_stats <- list()
## Density -----
oysterraw$SizeClass[oysterraw$SizeClass=="25to75mm"] <- "25-75mm"
oysterraw$SizeClass[oysterraw$SizeClass=="35to75mm"] <- "35-75mm"
oysterraw$SizeClass[oysterraw$SizeClass=="o75mm"] <- ">75mm"

# Create summary statistics for each managed area based on Year and Month
# intervals.
MA_YM_Stats <- oysterraw[oysterraw$nyrpar=="Density_m2",] %>%
  group_by(AreaID, !!sym(col_name), Year, Month, nyrpar,
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
MA_YM_Stats <- as.data.table(MA_YM_Stats[order(MA_YM_Stats[[col_name]],
                                               MA_YM_Stats$Year,
                                               MA_YM_Stats$Month,
                                               MA_YM_Stats$ShellType,
                                               MA_YM_Stats$SizeClass,
                                               MA_YM_Stats$HabitatType), ])
# Writes summary statistics to file
fwrite(MA_YM_Stats, paste0(output_path,"Oyster_Dens_", toupper(analysis), "_MMYY_Stats.txt"), sep="|")
# Save stats file to directory
ma_stats[[analysis]][["Density"]][["MA_YM_Stats"]] <- MA_YM_Stats
# Removes variable storing data to improve computer memory
rm(MA_YM_Stats)

# Create summary statistics for each managed area based on Year intervals
MA_Y_Stats <- oysterraw[oysterraw$nyrpar=="Density_m2",] %>%
  group_by(AreaID, !!sym(col_name), Year, nyrpar, LiveDate_Qualifier,
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
# Puts the data in order based on ManagedAreaName/OIMMP then Year
MA_Y_Stats <- as.data.table(MA_Y_Stats[order(MA_Y_Stats[[col_name]],
                                             MA_Y_Stats$Year,
                                             MA_Y_Stats$ShellType,
                                             MA_Y_Stats$SizeClass,
                                             MA_Y_Stats$HabitatType), ])
# Writes summary statistics to file
fwrite(MA_Y_Stats, paste0(output_path,"Oyster_Dens_",toupper(analysis),"_Yr_Stats.txt"), sep="|")
# Save stats file to directory
ma_stats[[analysis]][["Density"]][["MA_Y_Stats"]] <- MA_Y_Stats
# Removes variable storing data to improve computer memory
rm(MA_Y_Stats)

# Create summary statistics for each managed area based on Month intervals.
MA_M_Stats <- oysterraw[oysterraw$nyrpar=="Density_m2",] %>%
  group_by(AreaID, !!sym(col_name), Month, nyrpar,
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
# Puts the data in order based on ManagedAreaName/OIMMP then Month
MA_M_Stats <- as.data.table(MA_M_Stats[order(MA_M_Stats[[col_name]],
                                             MA_M_Stats$Month,
                                             MA_M_Stats$ShellType,
                                             MA_M_Stats$SizeClass,
                                             MA_M_Stats$HabitatType), ])
# Writes summary statistics to file
fwrite(MA_M_Stats, paste0(output_path,"/Oyster_Dens_",toupper(analysis),"_Mo_Stats.txt"), sep="|")
# Save stats file to directory
ma_stats[[analysis]][["Density"]][["MA_M_Stats"]] <- MA_M_Stats
# Removes variable storing data to improve computer memory
rm(MA_M_Stats)

# Create summary overall statistics for each managed area.
MA_Ov_Stats <- oysterraw[oysterraw$nyrpar=="Density_m2",] %>%
  group_by(!!sym(col_name), nyrpar,
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
if(analysis=="ma"){
  MA_Ov_Stats <- MA_Ov_Stats %>% merge(SEACAR::ManagedAreas[, c("AreaID", "ManagedAreaName")], all.x=T) %>%
    select(AreaID, everything())
}
setnames(MA_Ov_Stats, c("nyrpar", "LiveDate_Qualifier",
                        "HabitatClassification"),
         c("ParameterName", "ShellType", "HabitatType"))
MA_Ov_Stats$ShellType[MA_Ov_Stats$ShellType=="Exact"] <- "Live Oyster Shells"
MA_Ov_Stats$ShellType[MA_Ov_Stats$ShellType=="Estimate"] <- "Dead Oyster Shells"
# Puts the data in order based on ManagedAreaName/OIMMP
MA_Ov_Stats <- as.data.table(MA_Ov_Stats[order(MA_Ov_Stats[[col_name]],
                                               MA_Ov_Stats$ShellType,
                                               MA_Ov_Stats$SizeClass,
                                               MA_Ov_Stats$HabitatType), ])

# Replaces blank ProgramIDs with NA (missing values)
MA_Ov_Stats$ProgramIDs <- replace(MA_Ov_Stats$ProgramIDs,
                                  MA_Ov_Stats$ProgramIDs=="", NA)
MA_Ov_Stats$Programs <- replace(MA_Ov_Stats$Programs,
                                MA_Ov_Stats$Programs=="", NA)
# Write overall statistics to file
fwrite(MA_Ov_Stats, paste0(output_path,"/Oyster_Dens_",toupper(analysis),"_Overall_Stats.txt"), sep="|")
# Save stats file to directory
ma_stats[[analysis]][["Density"]][["MA_Ov_Stats"]] <- MA_Ov_Stats
# Removes variable storing data to improve computer memory
rm(MA_Ov_Stats)

## Shell Height -----
# Create summary statistics for each managed area based on Year and Month
# intervals.
MA_YM_Stats <- oysterraw[oysterraw$nyrpar=="ShellHeight_mm",] %>%
  group_by(AreaID, !!sym(col_name), Year, Month, nyrpar,
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
# Puts the data in order based on ManagedAreaName/OIMMP, Year, then Month
MA_YM_Stats <- as.data.table(MA_YM_Stats[order(MA_YM_Stats[[col_name]],
                                               MA_YM_Stats$Year,
                                               MA_YM_Stats$Month,
                                               MA_YM_Stats$ShellType,
                                               MA_YM_Stats$SizeClass,
                                               MA_YM_Stats$HabitatType), ])
# Writes summary statistics to file
fwrite(MA_YM_Stats, paste0(output_path,"/Oyster_SH_",toupper(analysis),"_MMYY_Stats.txt"), sep="|")
# Save stats file to directory
ma_stats[[analysis]][["Shell Height"]][["MA_YM_Stats"]] <- MA_YM_Stats
# Removes variable storing data to improve computer memory
rm(MA_YM_Stats)

# Create summary statistics for each managed area based on Year intervals
MA_Y_Stats <- oysterraw[oysterraw$nyrpar=="ShellHeight_mm",] %>%
  group_by(AreaID, !!sym(col_name), Year, nyrpar, LiveDate_Qualifier,
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
# Puts the data in order based on ManagedAreaName/OIMMP then Year
MA_Y_Stats <- as.data.table(MA_Y_Stats[order(MA_Y_Stats[[col_name]],
                                             MA_Y_Stats$Year,
                                             MA_Y_Stats$ShellType,
                                             MA_Y_Stats$SizeClass,
                                             MA_Y_Stats$HabitatType), ])
# Writes summary statistics to file
fwrite(MA_Y_Stats, paste0(output_path,"/Oyster_SH_",toupper(analysis),"_Yr_Stats.txt"), sep="|")
# Save stats file to directory
ma_stats[[analysis]][["Shell Height"]][["MA_Y_Stats"]] <- MA_Y_Stats
# Removes variable storing data to improve computer memory
rm(MA_Y_Stats)

# Create summary statistics for each managed area based on Month intervals.
MA_M_Stats <- oysterraw[oysterraw$nyrpar=="ShellHeight_mm",] %>%
  group_by(AreaID, !!sym(col_name), Month, nyrpar,
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
# Puts the data in order based on ManagedAreaName/OIMMP then Month
MA_M_Stats <- as.data.table(MA_M_Stats[order(MA_M_Stats[[col_name]],
                                             MA_M_Stats$Month,
                                             MA_M_Stats$ShellType,
                                             MA_M_Stats$SizeClass,
                                             MA_M_Stats$HabitatType), ])
# Writes summary statistics to file
fwrite(MA_M_Stats, paste0(output_path,"/Oyster_SH_",toupper(analysis),"_Mo_Stats.txt"), sep="|")
# Save stats file to directory
ma_stats[[analysis]][["Shell Height"]][["MA_M_Stats"]] <- MA_M_Stats
# Removes variable storing data to improve computer memory
rm(MA_M_Stats)

# Create summary overall statistics for each managed area.
MA_Ov_Stats <- oysterraw[oysterraw$nyrpar=="ShellHeight_mm",] %>%
  group_by(!!sym(col_name), nyrpar,
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
if(analysis=="ma"){
  MA_Ov_Stats <- MA_Ov_Stats %>% merge(SEACAR::ManagedAreas[, c("AreaID", "ManagedAreaName")], all.x=T) %>%
    select(AreaID, everything())
}
setnames(MA_Ov_Stats, c("nyrpar", "LiveDate_Qualifier",
                        "HabitatClassification"),
         c("ParameterName", "ShellType", "HabitatType"))
MA_Ov_Stats$ShellType[MA_Ov_Stats$ShellType=="Exact"] <- "Live Oyster Shells"
MA_Ov_Stats$ShellType[MA_Ov_Stats$ShellType=="Estimate"] <- "Dead Oyster Shells"
# Puts the data in order based on ManagedAreaName/OIMMP
MA_Ov_Stats <- as.data.table(MA_Ov_Stats[order(MA_Ov_Stats[[col_name]],
                                               MA_Ov_Stats$ShellType,
                                               MA_Ov_Stats$SizeClass,
                                               MA_Ov_Stats$HabitatType), ])

# Replaces blank ProgramIDs with NA (missing values)
MA_Ov_Stats$ProgramIDs <- replace(MA_Ov_Stats$ProgramIDs,
                                  MA_Ov_Stats$ProgramIDs=="", NA)
MA_Ov_Stats$Programs <- replace(MA_Ov_Stats$Programs,
                                MA_Ov_Stats$Programs=="", NA)
# Write overall statistics to file
fwrite(MA_Ov_Stats, paste0(output_path,"/Oyster_SH_",toupper(analysis),"_Overall_Stats.txt"), sep="|")
# Save stats file to directory
ma_stats[[analysis]][["Shell Height"]][["MA_Ov_Stats"]] <- MA_Ov_Stats
# Removes variable storing data to improve computer memory
rm(MA_Ov_Stats)

## Percent Live -----
# Create summary statistics for each managed area based on Year and Month
# intervals.
MA_YM_Stats <- oysterraw[oysterraw$nyrpar=="PercentLive_pct",] %>%
  group_by(AreaID, !!sym(col_name), Year, Month, nyrpar,
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
# Puts the data in order based on ManagedAreaName/OIMMP, Year, then Month
MA_YM_Stats <- as.data.table(MA_YM_Stats[order(MA_YM_Stats[[col_name]],
                                               MA_YM_Stats$Year,
                                               MA_YM_Stats$Month,
                                               MA_YM_Stats$ShellType,
                                               MA_YM_Stats$SizeClass,
                                               MA_YM_Stats$HabitatType), ])
# Writes summary statistics to file
fwrite(MA_YM_Stats, paste0(output_path,"/Oyster_PrcLive_",toupper(analysis),"_MMYY_Stats.txt"), sep="|")
# Save stats file to directory
ma_stats[[analysis]][["Percent Live"]][["MA_YM_Stats"]] <- MA_YM_Stats
# Removes variable storing data to improve computer memory
rm(MA_YM_Stats)

# Create summary statistics for each managed area based on Year intervals
MA_Y_Stats <- oysterraw[oysterraw$nyrpar=="PercentLive_pct",] %>%
  group_by(AreaID, !!sym(col_name), Year, nyrpar, LiveDate_Qualifier,
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
# Puts the data in order based on ManagedAreaName/OIMMP then Year
MA_Y_Stats <- as.data.table(MA_Y_Stats[order(MA_Y_Stats[[col_name]],
                                             MA_Y_Stats$Year,
                                             MA_Y_Stats$ShellType,
                                             MA_Y_Stats$SizeClass,
                                             MA_Y_Stats$HabitatType), ])
# Writes summary statistics to file
fwrite(MA_Y_Stats, paste0(output_path,"/Oyster_PrcLive_",toupper(analysis),"_Yr_Stats.txt"), sep="|")
# Save stats file to directory
ma_stats[[analysis]][["Percent Live"]][["MA_Y_Stats"]] <- MA_Y_Stats
# Removes variable storing data to improve computer memory
rm(MA_Y_Stats)

# Create summary statistics for each managed area based on Month intervals.
MA_M_Stats <- oysterraw[oysterraw$nyrpar=="PercentLive_pct",] %>%
  group_by(AreaID, !!sym(col_name), Month, nyrpar,
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
# Puts the data in order based on ManagedAreaName/OIMMP then Month
MA_M_Stats <- as.data.table(MA_M_Stats[order(MA_M_Stats[[col_name]],
                                             MA_M_Stats$Month,
                                             MA_M_Stats$ShellType,
                                             MA_M_Stats$SizeClass,
                                             MA_M_Stats$HabitatType), ])
# Writes summary statistics to file
fwrite(MA_M_Stats, paste0(output_path,"/Oyster_PrcLive_",toupper(analysis),"_Mo_Stats.txt"), sep="|")
# Save stats file to directory
ma_stats[[analysis]][["Percent Live"]][["MA_M_Stats"]] <- MA_M_Stats
# Removes variable storing data to improve computer memory
rm(MA_M_Stats)

# Create summary overall statistics for each managed area.
MA_Ov_Stats <- oysterraw[oysterraw$nyrpar=="PercentLive_pct",] %>%
  group_by(!!sym(col_name), nyrpar,
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
if(analysis=="ma"){
  MA_Ov_Stats <- MA_Ov_Stats %>% merge(SEACAR::ManagedAreas[, c("AreaID", "ManagedAreaName")], all.x=T) %>%
    select(AreaID, everything())
}
setnames(MA_Ov_Stats, c("nyrpar", "LiveDate_Qualifier",
                        "HabitatClassification"),
         c("ParameterName", "ShellType", "HabitatType"))
MA_Ov_Stats$ShellType[MA_Ov_Stats$ShellType=="Exact"] <- "Live Oyster Shells"
MA_Ov_Stats$ShellType[MA_Ov_Stats$ShellType=="Estimate"] <- "Dead Oyster Shells"
# Puts the data in order based on ManagedAreaName/OIMMP
MA_Ov_Stats <- as.data.table(MA_Ov_Stats[order(MA_Ov_Stats[[col_name]],
                                               MA_Ov_Stats$ShellType,
                                               MA_Ov_Stats$SizeClass,
                                               MA_Ov_Stats$HabitatType), ])

# Replaces blank ProgramIDs with NA (missing values)
MA_Ov_Stats$ProgramIDs <- replace(MA_Ov_Stats$ProgramIDs,
                                  MA_Ov_Stats$ProgramIDs=="", NA)
MA_Ov_Stats$Programs <- replace(MA_Ov_Stats$Programs,
                                MA_Ov_Stats$Programs=="", NA)
# Write overall statistics to file
fwrite(MA_Ov_Stats, paste0(output_path,"Oyster_PrcLive_",toupper(analysis),"_Overall_Stats.txt"), sep="|")
# Save stats file to directory
ma_stats[[analysis]][["Percent Live"]][["MA_Ov_Stats"]] <- MA_Ov_Stats
# Removes variable storing data to improve computer memory
rm(MA_Ov_Stats)

#Plotting ----
# LiveDate Threshold -----------------------------------------------------
oysterraw <- oysterraw[oysterraw$LiveDate>=1960,]
for(i in unique(oysterraw[[col_name]])){
  oysterraw[get(col_name)==i & !is.na(LiveDate), `:=`
            (RelYear=(LiveDate-min(LiveDate))+1,
              YearDiff=min(LiveDate)-1)]
}

# Plot theme and setup -----
plot_theme <- SEACAR::SEACAR_plot_theme()
plot_jitter <- position_jitter(width = 0.1, height = 0.1, seed=42)

library(future)
library(future.apply)
######################
#### Shell Height ####
######################

# Find out which MAs should receive models for SH
sh_stats <- ma_stats[[analysis]][["Shell Height"]][["MA_Ov_Stats"]] %>% 
  filter(!is.na(SizeClass)) %>% 
  select(!!sym(col_name), ParameterName, ShellType, SizeClass, HabitatType) %>%
  as.data.table()

task_list <- sh_stats[, .(HabitatType = unique(HabitatType)), by = eval(col_name)]
task_list <- as.data.frame(task_list)

# Function to subset data and run models where possible
shell_height_models_par <- function(loc, habitat_type, oysterraw){
  library(future)
  library(future.apply)
  library(tidyverse)
  library(data.table)
  library(ggplot2)
  library(brms)
  library(Rmisc)
  library(cmdstanr)
  library(rstantools)
  library(stringr)
  library(sf)
  library(tictoc)
  library(rstudioapi)
  library(ggpubr)
  
  if(analysis=="ma"){
    abrev <- MA_All[ManagedAreaName==loc, Abbreviation]
  } else {
    abrev <- loc
  }
  
  # Combined MA/OIMMP name with habitat type
  plotlabel <- paste0(loc, "_", str_to_title(habitat_type))
  
  # At least 5 years of data are required in order to run model analyses
  # Function checks N years of data, returns T or F
  suff_years <- function(data){length(unique(data$Year))>=5}
  
  if(abrev %in% c("ABAP", "ANERR", "Apalachicola Bay")){
    #Exclude the five samples that don't have counts less than the "NumberMeasured"
    #value for the corresponding program (see variable exploration graphs in the
    #25to75mm section for the rationale and graphs for this step.)
    numValves <- unique(oysterraw[, c("ProgramID", "RelYear", "counts",
                                      "QuadIdentifier", "Subtidal", "QuadSize_m2",
                                      "LiveDate_Qualifier", "NumberMeasured_n")])
    
    exclude_samps <- subset(numValves, numValves$NumberMeasured_n=="20" &
                              numValves$counts > 19)$QuadIdentifier
    
    sho25 <- oysterraw[!is.na(ShellHeight_mm) & ShellHeight_mm >= 25 & 
                         get(plotlab_col)==plotlabel & QuadIdentifier %in% setdiff(
                           oysterraw[!is.na(ShellHeight_mm) & get(col_name)==loc, QuadIdentifier], exclude_samps), ]
  } else {
    sho25 <- oysterraw[!is.na(ShellHeight_mm) & ShellHeight_mm >= 25 & get(plotlab_col)==plotlabel, ]
  }
  # Save shell height data > 25mm
  saveRDS(sho25, paste0(output_path, "model_results/data/", abrev, "_sho25_", Sys.Date(), "_", habitat_type, ".rds"))
  
  # Subset and save for shell height data >25 & <75
  sh25to75 <- sho25[ShellHeight_mm < 75, ]
  saveRDS(sh25to75, paste0(output_path, "model_results/data/", abrev, "_sh25to75_", Sys.Date(), "_", habitat_type, ".rds"))
  # Subset for model data (where LiveDate_Qualifier is "Exact" NOT "Estimate")
  sh25to75_mod_data <- subset(sh25to75, sh25to75$LiveDate_Qualifier!="Estimate")
  
  # run 25to75 model?
  # Load in previous model (if available) to determine if new data has been added
  # If new data has been added, run model again.
  model_loc <- paste0(output_path, "model_results/GLMMs/", abrev, "_sh25to75_glmm_", habitat_type, ".rds")
  prevMod <- tryCatch({
    readRDS(model_loc)
  }, error = function(e){
    message("Error reading in previous model file (sh25to75): ", conditionMessage(e))
    NULL
  })
  
  if(is.null(prevMod)){
    run25to75model <- TRUE
  } else if(nrow(sh25to75_mod_data)!=nrow(prevMod$data)){ #Check if amount of data has changed
    run25to75model <- TRUE
  } else {
    run25to75model <- FALSE
  }
  
  print(paste0("Sufficient years of data?: ", suff_years(sh25to75_mod_data)))
  
  # Don't run model if not enough years of data (5)
  if(!suff_years(sh25to75_mod_data) & run25to75model){
    run25to75model <- FALSE
  }
  
  # Model unable to run in AB Restored
  if(abrev %in% c("ABAP", "ANERR", "Apalachicola Bay") & habitat_type=="Restored"){
    run25to75model <- FALSE
  }
  
  print(paste0("Run new model?: ", run25to75model))
  
  # If the above is TRUE, then delete the old model so a new one can be run
  if(run25to75model & !is.null(prevMod)){
    print("Archive old model")
    file.rename(
      from = model_loc,
      to = paste0(output_path, "model_results/GLMMs/archive/", abrev, "_sh25to75_glmm_", habitat_type, "_", Sys.Date(), ".rds")
    )
  }
  cat(paste0("N_Row previous (sh25to75): ", nrow(prevMod$data), "\n N_Row current (sh25to75): ", nrow(sh25to75_mod_data), "\n"))
  
  if(suff_years(sh25to75_mod_data) & !QAQCPlots){
    cat("---- Sufficient years of data for SH 25mm to 75mm. \n")
    # Set formula to account for multiple quadsizes
    if(length(unique(sh25to75_mod_data$QuadSize_m2))>1){
      f <- brms::brmsformula(ShellHeight_mm | trunc(lb=25, ub=75) ~ RelYear + QuadSize_m2 + (1 | UniversalReefID))
    } else {
      f <- brms::brmsformula(ShellHeight_mm | trunc(lb=25, ub=75) ~ RelYear + (1 | UniversalReefID))
    }
    # Failed convergence in PISAP due to low number of UniversalReefID, try simpler model
    if(abrev=="PISAP"){
      f <- brms::brmsformula(ShellHeight_mm | trunc(lb=25, ub=75) ~ RelYear)
    }
    
    # Add ProgramID for Apalach Restored?
    if(abrev %in% c("ABAP", "ANERR") & habitat_type=="Restored"){
      f <- brms::brmsformula(ShellHeight_mm | trunc(lb=25, ub=75) ~ RelYear + (1 | UniversalReefID) + ProgramID)
    }
    # Run model if needed
    if(run25to75model){
      cat("---- Running model 25to75. \n")
      sh25to75_glmm <- brm(
        # formula=ShellHeight_mm | trunc(lb=25, ub=75) ~ RelYear+QuadSize_m2+(1 | UniversalReefID),
        formula = f,
        data=sh25to75_mod_data,
        family=gaussian, cores=ncores,
        control=list(adapt_delta=0.995, max_treedepth=20),
        iter=iter, warmup=warmup, chains=nchains, thin=3, seed=5699,
        backend="cmdstanr",
        file=model_loc,
        threads = threading(nthreads)
      )
    } else {
      if(abrev %in% c("ABAP", "ANERR") & habitat_type=="Restored"){
        sh25to75_glmm <- NULL
      } else {
        sh25to75_glmm <- readRDS(model_loc)
        sh25to75_glmm$file <- model_loc      
      }
    }
    models1 <- list(sh25to75_glmm)
  } else {models1 <- NULL}
  
  # Set variables for use within plots
  data1 <- sh25to75
  
  # Subset and save for shell height data >=75
  sho75 <- sho25[ShellHeight_mm >= 75, ]
  # Remove any large values to avoid truncation error
  sho75 <- sho75[ShellHeight_mm<=250, ]
  saveRDS(sho75, paste0(output_path, "model_results/data/", abrev, "_sho75_", Sys.Date(), "_", habitat_type, ".rds"))
  # Subset for model data (where LiveDate_Qualifier is "Exact" NOT "Estimate")
  sho75_mod_data <- subset(sho75, sho75$LiveDate_Qualifier!="Estimate")
  
  # run sho75 model?
  # Load in previous model (if available) to determine if new data has been added
  # If new data has been added, run model again.
  model_loc <- paste0(output_path, "model_results/GLMMs/", abrev, "_sho75_glmm_", habitat_type, ".rds")
  prevMod <- tryCatch({
    readRDS(model_loc)
  }, error = function(e){
    message("Error reading in previous model file (sho75): ", conditionMessage(e))
    NULL
  })
  
  if(is.null(prevMod)){
    runsho75model <- TRUE
  } else if(nrow(sho75_mod_data)!=nrow(prevMod$data)){ #Check if amount of data has changed
    runsho75model <- TRUE
  } else {
    runsho75model <- FALSE
  }
  
  print(paste0("Sufficient years of data?: ", suff_years(sho75_mod_data)))
  
  # Don't run model if not enough years of data (5)
  if(!suff_years(sho75_mod_data) & runsho75model){
    runsho75model <- FALSE
  }
  
  # Model unable to run in AB Restored
  if(abrev %in% c("ABAP", "ANERR", "Apalachicola Bay") & habitat_type=="Restored"){
    runsho75model <- FALSE
  }
  
  print(paste0("Run new model?: ", runsho75model))
  
  # If the above is TRUE, then delete the old model so a new one can be run
  if(runsho75model & !is.null(prevMod)){
    print("Archive old model")
    file.rename(
      from = model_loc,
      to = paste0(output_path, "model_results/GLMMs/archive/", abrev, "_pct_glmm_", habitat_type, "_", Sys.Date(), ".rds")
    )
  }
  cat(paste0("N_Row previous (sho75): ", nrow(prevMod$data), "\n N_Row current (sho75): ", nrow(sho75_mod_data), "\n"))
  
  if(suff_years(sho75_mod_data) & !QAQCPlots){
    cat("---- Sufficient years of data for SH over 75mm. \n")
    # Set formula to account for multiple quadsizes
    if(length(unique(sh25to75_mod_data$QuadSize_m2))>1){
      f <- brms::brmsformula(ShellHeight_mm | trunc(lb=75, ub=250) ~ RelYear + QuadSize_m2 + (1 | UniversalReefID))
    } else {
      f <- brms::brmsformula(ShellHeight_mm | trunc(lb=75, ub=250) ~ RelYear + (1 | UniversalReefID))
    }
    # Failed convergence in PISAP due to low number of UniversalReefID, try simpler model
    if(abrev=="PISAP"){
      f <- brms::brmsformula(ShellHeight_mm | trunc(lb=75, ub=250) ~ RelYear)
    }
    # Run model if needed
    if(runsho75model){
      cat("---- Running model >75. \n")
      sho75_glmm <- brm(
        formula = f,
        data=sho75_mod_data,
        family=gaussian, cores=ncores,
        control= list(adapt_delta=0.995, max_treedepth=20),
        iter=iter, warmup=warmup, chains=nchains, thin=3, seed=3639,
        backend="cmdstanr",
        file=model_loc,
        threads = threading(nthreads)
      )
    } else {
      if(abrev %in% c("ABAP", "ANERR") & habitat_type=="Restored"){
        sho75_glmm <- NULL
      } else {
        sho75_glmm <- readRDS(model_loc)
        sho75_glmm$file <- model_loc      
      }
    }
    models2 <- list(sho75_glmm)
  } else {models2 <- NULL}
  # Set variables for use within plots
  data2 <- sho75
  
  #### modresultssh_par function ####
  datafile1 <- data1
  datafile2 <- data2
  indicator <- "Size class"
  meplotzoom <- FALSE
  
  oysterresults_temp <- data.frame()
  datafile1$SizeClass[datafile1$SizeClass=="25to75mm" &
                        datafile1$MA_plotlab==
                        "St. Martins Marsh Aquatic Preserve_Natural"] <-
    "35-75mm"
  sizeclass1 <- unique(datafile1$SizeClass)
  for(m in seq_along(models1)){
    modelobj <- models1[[m]]
    if(is.null(modelobj)) next
    oyres_i <- setDT(broom.mixed::tidy(modelobj))
    #tidy() does not like that parameter values have underscores
    #for some reason, so the resulting table is incomplete
    
    if(nrow(oyres_i[effect=="fixed", ])-nrow(summary(modelobj)$fixed)==-1){
      missingrow <- data.table(effect="fixed",
                               component="cond",
                               #not sure what "cond" means in the tidy summary.
                               group=NA,
                               term=rownames(summary(modelobj)$fixed)[2],
                               estimate=summary(modelobj)$fixed$Estimate[2],
                               std.error=summary(modelobj)$fixed$Est.Error[2],
                               conf.low=summary(modelobj)$fixed$`l-95% CI`[2],
                               conf.high=summary(modelobj)$fixed$`u-95% CI`[2])
      oyres_i <- rbind(oyres_i, missingrow) %>% arrange(effect, group)
    }
    
    setDT(oyres_i)
    oyres_i[, `:=` (indicator=indicator,
                    areaName=unique(datafile1[[col_name]]),
                    habitat_class=unique(datafile1$HabitatClassification),
                    size_class=sizeclass1,
                    live_date_qual=ifelse(
                      str_detect(
                        modelobj$file, "_hist"), "Estimate",
                      "Exact"),
                    n_programs=if(class(
                      try(datafile1$LiveDate_Qualifier))!="try-error"){
                      length(unique(
                        datafile1[LiveDate_Qualifier==
                                    ifelse(str_detect(
                                      modelobj$file, "_hist"),
                                      "Estimate", "Exact"),
                                  ProgramID]))
                    } else{length(unique(datafile1[, ProgramID]))},
                    programs=if(class(try(
                      datafile1$LiveDate_Qualifier)) != "try-error"){
                      list(unique(
                        datafile1[LiveDate_Qualifier==
                                    ifelse(
                                      str_detect(
                                        modelobj$file,
                                        "_hist"),
                                      "Estimate",
                                      "Exact"),
                                  ProgramID]))
                    } else{list(unique(datafile1[, ProgramID]))},
                    filename=modelobj$file)]
    
    oysterresults_temp <- rbind(oysterresults_temp, oyres_i)
  }
  
  datafile2$SizeClass[datafile2$SizeClass=="25to75mm" &
                        datafile2$MA_plotlab==
                        "St. Martins Marsh Aquatic Preserve_Natural"] <- "35-75mm"
  sizeclass2 <- unique(datafile2$SizeClass)
  
  for(m in seq_along(models2)){
    modelobj <- models2[[m]]
    if(is.null(modelobj)) next
    oyres_i <- setDT(broom.mixed::tidy(modelobj))
    #tidy() does not like that parameter values have underscores for
    #some reason, so the resulting table is incomplete
    
    if(nrow(oyres_i[effect=="fixed", ])-nrow(summary(modelobj)$fixed)==-1){
      missingrow <- data.table(effect="fixed",
                               component="cond",
                               #not sure what "cond" means in the tidy summary.
                               group=NA,
                               term=rownames(summary(modelobj)$fixed)[2],
                               estimate=summary(modelobj)$fixed$Estimate[2],
                               std.error=summary(modelobj)$fixed$Est.Error[2],
                               conf.low=summary(modelobj)$fixed$`l-95% CI`[2],
                               conf.high=summary(modelobj)$fixed$`u-95% CI`[2])
      oyres_i <- rbind(oyres_i, missingrow) %>% arrange(effect, group)
    }
    
    oyres_i <- oyres_i %>%
      mutate(
        indicator = indicator,
        areaName = unique(datafile2[[col_name]]),
        habitat_class = unique(datafile2$HabitatClassification),
        size_class = sizeclass2,
        live_date_qual = if_else(
          str_detect(modelobj$file, "_hist"), "Estimate", "Exact"
        ),
        n_programs = if (class(try(datafile2$LiveDate_Qualifier)) != "try-error") {
          datafile2 %>%
            filter(LiveDate_Qualifier == if_else(str_detect(modelobj$file, "_hist"), "Estimate", "Exact")) %>%
            pull(ProgramID) %>%
            unique() %>%
            length()
        } else {
          datafile2 %>%
            pull(ProgramID) %>%
            unique() %>%
            length()
        },
        programs = if (class(try(datafile2$LiveDate_Qualifier)) != "try-error") {
          list(datafile2 %>%
                 filter(LiveDate_Qualifier == if_else(str_detect(modelobj$file, "_hist"), "Estimate", "Exact")) %>%
                 pull(ProgramID) %>%
                 unique())
        } else {
          list(datafile2 %>% pull(ProgramID) %>% unique())
        },
        filename = modelobj$file
      )
    oysterresults_temp <- rbind(oysterresults_temp, oyres_i)
  }
  
  ind <- case_when(str_detect(indicator, "ercent") ~ "Pct",
                   str_detect(indicator, "ensity") ~ "Den",
                   str_detect(indicator, "^S|^s") ~ "SH")
  
  if(nrow(data1)>0){
    sizeclass1 <- unique(data1$SizeClass)
  } else {
    sizeclass1 <- ""
  }
  if(nrow(data2)>0){
    sizeclass2 <- unique(data2$SizeClass)
  } else {
    sizeclass2 <- ""
  }
  
  # Set size labels
  if(sizeclass1 != ""){
    size1 <- case_when(
      str_detect(sizeclass1, "25") & str_detect(sizeclass1, "75") ~ "25to75",
      str_detect(sizeclass1, "35") & str_detect(sizeclass1, "75") ~ "35to75",
      str_detect(sizeclass1, "25")==FALSE & str_detect(sizeclass1, "75") ~ "o75",
      TRUE ~ "raw")
    sizelab1 <- case_when(
      str_detect(sizeclass1, "25") & str_detect(sizeclass1, "75") ~ "25-75mm",
      str_detect(sizeclass1, "35") & str_detect(sizeclass1, "75") ~ "35-75mm",
      str_detect(sizeclass1, "25")==FALSE & str_detect(sizeclass1, "75") ~ "\u2265 75mm",
      TRUE ~ "raw")
  }
  if(sizeclass2 != ""){
    size2 <- case_when(
      str_detect(sizeclass2, "25") & str_detect(sizeclass2, "75") ~ "25to75",
      str_detect(sizeclass2, "35") & str_detect(sizeclass2, "75") ~ "35to75",
      str_detect(sizeclass2, "25")==FALSE & str_detect(sizeclass2, "75") ~ "o75",
      TRUE ~ "raw")
    sizelab2 <- case_when(
      str_detect(sizeclass2, "25") & str_detect(sizeclass2, "75") ~ "25-75mm",
      str_detect(sizeclass2, "35") & str_detect(sizeclass2, "75") ~ "35-75mm",
      str_detect(sizeclass2, "25")==FALSE & str_detect(sizeclass2, "75") ~ "\u2265 75mm",
      TRUE ~ "raw")
  } else {
    size2 <- "o75"
    sizelab2 <- "\u2265 75mm"
  }
  # Remove space from between >= and 75mm
  sizelab1 <- gsub(" ", "", sizelab1)
  sizelab2 <- gsub(" ", "", sizelab2)
  
  #Marginal effects plot including random effects
  ## Hist plot settings
  if(nrow(data2)>0){
    y_max <- round(max(data2[!is.na(ShellHeight_mm), ShellHeight_mm]), -0)+1
  } else {
    y_max <- round(max(data1[!is.na(ShellHeight_mm), ShellHeight_mm]), -0)+1
  }
  y_breaks <- seq(25, 300, 50)
  y_labs <- seq(25, 300, 50)
  y_minor <- seq(0, 300, 25)
  ylim_upper <- ceiling(y_max/25)*25
  
  yrdiff1 <- unique(data1$YearDiff)
  yrdiff2 <- unique(data2$YearDiff)
  
  # function to set year breaks, type == "hist" or "live"
  set_breaks <- function(type, data1, data2){
    ldq <- ifelse(type=="hist", "Estimate", "Exact")
    
    maxyr <- max(data1[!is.na(LiveDate) & LiveDate_Qualifier==ldq, LiveDate],
                 data2[!is.na(LiveDate) & LiveDate_Qualifier==ldq, LiveDate])
    minyr <- min(data1[!is.na(LiveDate) & LiveDate_Qualifier==ldq, LiveDate],
                 data2[!is.na(LiveDate) & LiveDate_Qualifier==ldq, LiveDate])
    nyrs <- maxyr - minyr + 1
    
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    
    # Creates break intervals for plots based on number of years of data
    if(nyrs>=40){
      # Set breaks to every 10 years if more than 40 years of data
      brk <- 10
    } else if(nyrs>=20){
      # Set breaks to every 5 years if between 40 and 20 years of data
      brk <- 5
    } else if(nyrs>=12){
      # Set breaks to every 3 years if between 20 and 12 years of data
      brk <- 3
    } else if(nyrs>=8){
      # Set breaks to every 2 years if between 12 and 8 years of data
      brk <- 2
    } else if(nyrs>=5){
      # Set breaks to every year if between 8 and 5 years of data
      brk <- 1
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
    return(list("seq" = seq(minyr,maxyr,brk),"maxyr" = maxyr, "minyr" = minyr))
  }
  
  ## Check data for Exact and Estimate
  n_hist1 <- nrow(data1[data1$LiveDate_Qualifier=="Estimate" &
                          !is.na(data1$ShellHeight_mm),])
  n_live1 <- nrow(data1[data1$LiveDate_Qualifier=="Exact" &
                          !is.na(data1$ShellHeight_mm),])
  n_hist2 <- nrow(data2[data2$LiveDate_Qualifier=="Estimate" &
                          !is.na(data2$ShellHeight_mm),])
  n_live2 <- nrow(data2[data2$LiveDate_Qualifier=="Exact" &
                          !is.na(data2$ShellHeight_mm),])
  
  # Plot variable to record which plots to show (dead, live, or both)
  available_plots <- c()
  # If "Estimate" data exists, set y-axis (years)
  if(n_hist1>0 | n_hist2>0){
    yrlist_hist <- set_breaks(type = "hist", data1 = data1, data2 = data2)[["seq"]]
    maxyr_hist <- set_breaks(type = "hist", data1 = data1, data2 = data2)[["maxyr"]]
    minyr_hist <- set_breaks(type = "hist", data1 = data1, data2 = data2)[["minyr"]]
    available_plots <- c(available_plots, "dead")
  }
  # If "Exact" data exists, set y-axis (years)
  if(n_live1>0 | n_live2>0){
    yrlist_live <- set_breaks(type = "live", data1 = data1, data2 = data2)[["seq"]]
    maxyr_live <- set_breaks(type = "live", data1 = data1, data2 = data2)[["maxyr"]]
    minyr_live <- set_breaks(type = "live", data1 = data1, data2 = data2)[["minyr"]]
    available_plots <- c(available_plots, "live")
  }
  
  set.seed(987)
  if(!is.null(models1[[1]]) & !QAQCPlots){
    liveplot_1 <- plot(conditional_effects(models1[[1]], re_formula=NULL), plot=FALSE)
  }
  
  if(!is.null(models2[[1]]) & !QAQCPlots){
    liveplot_2 <- plot(conditional_effects(models2[[1]], re_formula=NULL), plot=FALSE)
  }
  
  # Set boolean values for whether liveplot1&2 are available
  liveplot1_avail <- class(try(liveplot_1, silent=TRUE)) != "try-error"
  liveplot2_avail <- class(try(liveplot_2, silent=TRUE)) != "try-error"
  
  # Set ribbon transparency value
  a_ribb <- 0.2
  # Set size and shapes for plots
  p_shape <- c("size2"=24, "size1"=21)
  sizelab <- c("size2"=sizelab2, "size1"=sizelab1)
  
  col1 <- NA
  col2 <- NA
  
  # "transparent" allows for dummy values to be plotted. Ensures proper legend display
  if(liveplot1_avail){
    col1 <- c(size1="#00374f")
  } else{
    col1 <- c(size1="transparent")
  }
  
  if(liveplot2_avail){
    col2 <- c(size2="#0094b0")
  } else{
    col2 <- c(size2="transparent")
  }
  
  p_color <- c(col2, col1)
  
  # Initial plots to set legends
  plot_leg <- ggplot() +
    {if(liveplot1_avail){
      list(geom_ribbon(data=liveplot_1$RelYear$data,
                       aes(x=RelYear+yrdiff1, y=ShellHeight_mm,
                           ymin=lower__, ymax=upper__,
                           fill="size1"), 
                       alpha=a_ribb,
                       show.legend = TRUE),
           geom_line(data=liveplot_1$RelYear$data,
                     aes(x=RelYear+yrdiff1, y=estimate__, 
                         color="size1"),
                     lwd=0.75,
                     show.legend = TRUE),
           # Dummy values
           geom_ribbon(data=liveplot_1$RelYear$data,
                       aes(x=RelYear+yrdiff1, y=ShellHeight_mm,
                           ymin=lower__, ymax=upper__,
                           fill="size2"), 
                       alpha=a_ribb,
                       show.legend = TRUE),
           geom_line(data=liveplot_1$RelYear$data,
                     aes(x=RelYear+yrdiff1, y=estimate__, 
                         color="size2"),
                     lwd=0.75,
                     show.legend = TRUE))
    }} +
    {if(liveplot2_avail){
      list(geom_ribbon(data=liveplot_2$RelYear$data,
                       aes(x=RelYear+yrdiff2, y=ShellHeight_mm,
                           ymin=lower__, ymax=upper__, 
                           fill="size2"), 
                       alpha=a_ribb,
                       show.legend = TRUE),
           geom_line(data=liveplot_2$RelYear$data,
                     aes(x=RelYear+yrdiff2, y=estimate__, 
                         color="size2"),
                     lwd=0.75,
                     show.legend = TRUE),
           # Dummy values
           geom_ribbon(data=liveplot_2$RelYear$data,
                       aes(x=RelYear+yrdiff2, y=ShellHeight_mm,
                           ymin=lower__, ymax=upper__, 
                           fill="size1"), 
                       alpha=a_ribb,
                       show.legend = TRUE),
           geom_line(data=liveplot_2$RelYear$data,
                     aes(x=RelYear+yrdiff2, y=estimate__, 
                         color="size1"),
                     lwd=0.75,
                     show.legend = TRUE))
    }} +
    # Dummy points
    geom_point(data=data1[!is.na(RelYear) & !is.na(LiveDate), ],
               aes(x=LiveDate, y=ShellHeight_mm, shape="size2"),
               position=plot_jitter, size=2, color="transparent", fill = "transparent",
               alpha=0.8, show.legend = TRUE) +
    geom_point(data=data1[!is.na(RelYear) & !is.na(LiveDate), ],
               aes(x=LiveDate, y=ShellHeight_mm, shape="size1"),
               position=plot_jitter, size=2, color="#333333", fill = "#cccccc",
               alpha=0.8, show.legend = TRUE) +
    # Dummy points
    geom_point(data=data2[!is.na(RelYear) & !is.na(LiveDate), ],
               aes(x=LiveDate, y=ShellHeight_mm, shape="size1"),
               position=plot_jitter, size=2, color="transparent", fill = "transparent",
               alpha=0.8, show.legend = TRUE) +
    geom_point(data=data2[!is.na(RelYear) & !is.na(LiveDate), ],
               aes(x=LiveDate, y=ShellHeight_mm, shape="size2"),
               position=plot_jitter, size=2, color="#333333", fill = "#cccccc",
               alpha=0.8, show.legend = TRUE) +
    plot_theme +
    theme(legend.position="right") +
    scale_shape_manual(name="Size class",
                       breaks = c("size2", "size1"),
                       values=p_shape,
                       labels=sizelab) +
    scale_color_manual(name="Size class",
                       breaks = c("size2", "size1"),
                       values=p_color,
                       labels=sizelab, 
                       aesthetics = c("color", "fill"))
  
  leg <- get_legend(plot_leg)
  rm(plot_leg)
  
  # Dead oyster shell plot
  if("dead" %in% available_plots){
    plot1 <- ggplot() +
      geom_hline(yintercept=75, linewidth=1, color="grey") +
      {if(n_hist1>0){
        geom_point(data=data1[!is.na(RelYear) &
                                !is.na(LiveDate) &
                                LiveDate_Qualifier=="Estimate", ],
                   aes(x=LiveDate, y=ShellHeight_mm, shape="size1"),
                   position=plot_jitter, size=2, color="#333333", fill="#cccccc",
                   alpha=0.8, inherit.aes=FALSE) 
      }} +
      {if(n_hist2>0){
        geom_point(data=data2[!is.na(RelYear) & !is.na(LiveDate) &
                                LiveDate_Qualifier=="Estimate", ],
                   aes(x=LiveDate, y=ShellHeight_mm, shape="size2"),
                   position=plot_jitter, size=2, color="#333333", fill="#cccccc",
                   alpha=0.8, inherit.aes=FALSE)
      }} +
      scale_x_continuous(limits=c(minyr_hist-0.25, maxyr_hist+0.25),
                         breaks=yrlist_hist) +
      scale_y_continuous(breaks=y_breaks,
                         labels=y_labs, minor_breaks=y_minor) +
      plot_theme +
      theme(plot.subtitle=element_text(hjust=0, size=10, color="#314963"),
            legend.position="none") +
      labs(subtitle="Dead Oyster Shells",
           x="Estimated year",
           y="Shell height (mm)") +
      scale_shape_manual(name="Shell heights",
                         values=c("size1"=21, "size2"=24),
                         labels=c(sizelab1, sizelab2)) +
      scale_color_manual(name="Shell heights",
                         values=c("size1"="#00374f", "size2"="#0094b0"),
                         labels=c(sizelab1, sizelab2)) +
      scale_fill_manual(name="Shell heights",
                        values=c("size1"="#00374f", "size2"="#0094b0"),
                        labels=c(sizelab1, sizelab2)) +
      coord_cartesian(ylim=c(25, ylim_upper))  
  }
  
  # Live oyster shell plot
  if("live" %in% available_plots){
    plot2 <- ggplot() +
      geom_hline(yintercept=75, linewidth=1, color="grey") +
      {if(n_live1>0){
        geom_point(data=data1[!is.na(RelYear) & !is.na(LiveDate) &
                                LiveDate_Qualifier=="Exact", ],
                   aes(x=LiveDate, y=ShellHeight_mm, shape="size1"),
                   position=plot_jitter, size=2, color="#333333", fill="#cccccc",
                   alpha=0.8, inherit.aes=FALSE) 
      }} +
      {if(n_live2>0){
        geom_point(data=data2[!is.na(RelYear) & !is.na(LiveDate) &
                                LiveDate_Qualifier=="Exact", ],
                   aes(x=LiveDate, y=ShellHeight_mm, shape="size2"),
                   position=plot_jitter, size=2, color="#333333", fill="#cccccc",
                   alpha=0.8, inherit.aes=FALSE)
      }} +
      {if(liveplot1_avail){
        list(geom_ribbon(data=liveplot_1$RelYear$data,
                         aes(x=RelYear+yrdiff1, y=ShellHeight_mm,
                             ymin=lower__, ymax=upper__, fill="size1"),
                         alpha=a_ribb),
             geom_line(data=liveplot_1$RelYear$data,
                       aes(x=RelYear+yrdiff1, y=estimate__, color="size1"),
                       lwd=0.75))
      }} +
      {if(liveplot2_avail){
        list(geom_ribbon(data=liveplot_2$RelYear$data,
                         aes(x=RelYear+yrdiff2, y=ShellHeight_mm,
                             ymin=lower__, ymax=upper__, fill="size2"),
                         alpha=a_ribb),
             geom_line(data=liveplot_2$RelYear$data,
                       aes(x=RelYear+yrdiff2, y=estimate__, color="size2"),
                       lwd=0.75))
      }} +
      scale_x_continuous(limits=c(minyr_live-0.25, maxyr_live+0.25),
                         breaks=yrlist_live) +
      scale_y_continuous(breaks=y_breaks,
                         labels=y_labs, minor_breaks=y_minor) +
      plot_theme +
      theme(plot.subtitle=element_text(hjust=0, size=10, color="#314963"),
            legend.position="none") +
      labs(subtitle="Live Oysters",
           x="Year",
           y="Shell height (mm)") +
      scale_shape_manual(name="Shell heights",
                         values=c("size1"=21, "size2"=24),
                         labels=c(sizelab1, sizelab2)) +
      scale_color_manual(name="Shell heights",
                         values=c("size1"="#00374f", "size2"="#0094b0"),
                         labels=c(sizelab1, sizelab2)) +
      scale_fill_manual(name="Shell heights",
                        values=c("size1"="#00374f", "size2"="#0094b0"),
                        labels=c(sizelab1, sizelab2)) +
      coord_cartesian(ylim=c(25, ylim_upper))  
  }
  
  location_subtitle <- ifelse(analysis=="oimmp", paste0(loc, " OIMMP Region"), loc)
  
  # Set plot title
  plot_title <- ggplot() +
    labs(title=paste0("Oyster Size Class (", habitat_type, ")"), subtitle=location_subtitle) +
    plot_theme +
    theme(plot.subtitle=element_text(hjust=0.5, size=10, color="#314963"),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_blank())
  
  if("live" %in% available_plots & "dead" %in% available_plots){
    # Remove y-axis labels, ticks, title before combining both plots
    plot2 <- plot2 +
      theme(legend.position="none",
            axis.text.y=element_blank(),  #remove y-axis labels
            axis.ticks.y=element_blank(),  #remove y-axis ticks
            axis.title.y=element_blank())   #removes y-axis title
    # Combine live and dead plots + legend
    plot_comb <- ggarrange(plot1, plot2, leg, nrow=1,
                           widths=c(0.46, 0.39, 0.15))
  } else if("live" %in% available_plots & !"dead" %in% available_plots){
    # Combine live plots with legend
    plot_comb <- ggarrange(plot2, leg, nrow=1,
                           widths=c(0.85, 0.15))
  } else if("dead" %in% available_plots & !"live" %in% available_plots){
    # Combine dead plots with legend
    plot_comb <- ggarrange(plot1, leg, nrow=1,
                           widths=c(0.85, 0.15))
  }
  
  plot_comb <- ggarrange(plot_title, plot_comb, ncol=1,
                         heights=c(0.125, 0.875))
  
  # Specify save location (QAQC Plots saved elsewhere)
  if(QAQCPlots){
    file_name <- paste0(output_path, "QAQC/Oyster_SH_GLMM_", abrev, "_", habitat_type, ".png")
  } else {
    file_name <- paste0(output_path, "Figures/Shell_Height/Oyster_SH_GLMM_", abrev, "_", habitat_type, ".png")
  }
  
  ggsave(file_name,
         plot_comb,
         width=8,
         height=4,
         units="in",
         dpi=200,
         bg="white")
  
  return(oysterresults_temp)
  
  cat("---- Shell Height plot created for", loc, "-", habitat_type, "\n")
}

split_tasks <- split(task_list, ceiling(seq_along(1:nrow(task_list)) / 4))
oyster_sh <- oysterraw %>% filter(!is.na(ShellHeight_mm))
# Subset and save temp .rds objects for each MA (breaks up oysterraw)
for(ma in unique(oyster_sh[[col_name]])){
  saveRDS(oyster_sh[oyster_sh[[col_name]] == ma, ],
          file = paste0(output_path, "tmp/oystersh_", make.names(ma), ".rds"))
}

results_all <- list()
for(b in seq_along(split_tasks)){
  batch <- split_tasks[[b]]
  
  results_list <- future_lapply(seq_len(nrow(batch)), function(i) {
    task <- batch[i, ]
    oysterraw_path <- paste0(output_path, "tmp/oystersh_", make.names(task[[col_name]]), ".rds")
    oysterraw_sub <- readRDS(oysterraw_path)
    cat("\n-- Analyzing ", task[[col_name]], "\n")
    shell_height_models_par(
      loc = task[[col_name]],
      habitat_type = task$HabitatType,
      oysterraw = oysterraw_sub
    )
  }, future.seed = TRUE)
  results_all[[b]] <- data.table::rbindlist(results_list, fill = TRUE)
  gc()
}

if(!QAQCPlots){
  oysterresults_sh <- data.table::rbindlist(results_all, fill = TRUE)
  fwrite(oysterresults_sh, paste0(output_path, "model_results/oysterresults_sh.csv"))
}

############################
######### DENSITY ##########
############################

oysterraw$YearDiff <- oysterraw$LiveDate-oysterraw$RelYear
oysterraw_den <- oysterraw[!is.na(Density_m2),]
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
                  !!sym(col_name), SurveyMethod,
                  HabitatClassification, QuadSize_m2, UniversalReefID, 
                  !!sym(plotlab_col), Subtidal) %>%
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

#Summarize density data by managed area
den_all_sum <- summarySE(oysterraw_den, measurevar='Density_m2',
                         groupvars=c(eval(col_name), 'Year'))

# Find out which MAs should receive models for Density
den_stats <- ma_stats[[analysis]][["Density"]][["MA_Ov_Stats"]] %>% 
  select(eval(col_name), ParameterName, HabitatType) %>%
  as.data.table()

task_list <- den_stats[, .(HabitatType = unique(HabitatType)), by = eval(col_name)]
task_list <- as.data.frame(task_list)

# Density modelling function
density_models_par <- function(loc, habitat_type, oysterraw_den){
  library(future)
  library(future.apply)
  library(tidyverse)
  library(data.table)
  library(ggplot2)
  library(brms)
  library(Rmisc)
  library(cmdstanr)
  library(rstantools)
  library(stringr)
  library(sf)
  library(tictoc)
  library(rstudioapi)
  library(ggpubr)
  
  plot_density <- function(mod){
    df <- as.data.table(mod$data)
    response_col <- "Density_m2"
    rel_year_col <- "RelYear"
    rel_seq <- round(seq(from = min(df[[rel_year_col]], na.rm = TRUE),
                         to = max(df[[rel_year_col]], na.rm = TRUE),
                         by = 0.1), 1)
    cols_to_group <- setdiff(names(df), c(response_col, rel_year_col))
    
    newdata_w <- df[, .(weight_n = .N), by = cols_to_group]
    
    newdata_w <- newdata_w[,.(RelYear = rel_seq,
                              weight_n = rep(weight_n[1], length(rel_seq))),
                           by = cols_to_group]
    
    setnames(newdata_w, "RelYear", rel_year_col)
    
    newdata <- as.data.frame(
      newdata_w[, setdiff(names(newdata_w), "weight_n"), with = FALSE]
    )
    
    weights <- newdata_w$weight_n
    epred <- posterior_epred(
      mod,
      newdata = newdata,
      re_formula = NULL,
      ndraws = 2668
    )
    
    summarise_epred_by_rel_year <- function(
    epred,
    newdata,
    weights = NULL,
    rel_year_col = "RelYear",
    probs = c(0.025, 0.975)
    ) {
      
      newdata <- as.data.table(newdata)
      
      if (is.null(weights)) {
        weights <- rep(1, nrow(newdata))
      }
      
      rel_values <- sort(unique(newdata[[rel_year_col]]))
      
      # Matrix: posterior draws × RelYear values
      trend_draws <- sapply(rel_values, function(yr) {
        
        idx <- which(newdata[[rel_year_col]] == yr)
        w <- weights[idx] / sum(weights[idx])
        
        as.numeric(epred[, idx, drop = FALSE] %*% w)
      })
      
      trend_summary <- data.table(
        RelYear = rel_values,
        estimate__ = colMeans(trend_draws, na.rm = TRUE),
        se__ = apply(trend_draws, 2, sd, na.rm = TRUE),
        lower__ = apply(trend_draws, 2, quantile, probs = probs[1], na.rm = TRUE),
        upper__ = apply(trend_draws, 2, quantile, probs = probs[2], na.rm = TRUE)
      )
      
      list(
        summary = trend_summary,
        draws = trend_draws,
        rel_values = rel_values
      )
    }
    
    overall_trend <- summarise_epred_by_rel_year(
      epred = epred,
      newdata = newdata,
      weights = weights,
      rel_year_col = "RelYear"
    )
    return(overall_trend)
  }
  
  if(analysis=="ma"){
    abrev <- MA_All[ManagedAreaName==loc, Abbreviation]
  } else {
    abrev <- loc
  }
  
  cat("----", paste0("Habitat type: ", habitat_type, "\n"))
  # Combined MA name with habitat type
  plotlabel <- paste0(loc, "_", str_to_title(habitat_type))
  
  # At least 5 years of data are required in order to run model analyses
  # Function checks N years of data, returns T or F
  suff_years <- function(data){length(unique(data$Year))>=5}
  # Create subset for each MA
  ma_subset <- subset(oysterraw_den, oysterraw_den[[plotlab_col]]==plotlabel)
  # Ensure Density_m2 is numberic & rounded
  ma_subset[, Density_m2 := as.integer(round(Density_m2))]
  # Save data used in model
  saveRDS(ma_subset, paste0(output_path, "model_results/data/", abrev, "_density_", Sys.Date(), "_", habitat_type, ".rds"))
  
  # run new model?
  # Load in previous model (if available) to determine if new data has been added
  # If new data has been added, run model again.
  model_loc <- paste0(output_path, "model_results/GLMMs/", abrev, "_den_glmm9_", habitat_type, ".rds")
  prevMod <- tryCatch({
    readRDS(model_loc)
  }, error = function(e){
    message("Error reading in previous model file: ", conditionMessage(e))
    NULL
  })
  # Determine whether to run new model using runDenModel
  if(is.null(prevMod)){
    runDenModel <- TRUE
  } else if(nrow(ma_subset)!=nrow(prevMod$data)){ #Check if amount of data has changed
    runDenModel <- TRUE
  } else {
    runDenModel <- FALSE
  }
  
  print(paste0("Sufficient years of data?: ", suff_years(ma_subset)))
  # Don't run model if not enough years of data (5)
  if(!suff_years(ma_subset) & runDenModel){
    runDenModel <- FALSE
  }
  
  print(paste0("Run new model?: ", runDenModel))
  
  # If the above is TRUE, then delete the old model so a new one can be run
  if(runDenModel & !is.null(prevMod)){
    print("Archive old model")
    file.rename(
      from = model_loc,
      to = paste0(output_path, "model_results/GLMMs/archive/", abrev, "_den_glmm9_", habitat_type, "_", Sys.Date(), ".rds")
    )
  }
  cat(paste0("N_Row previous (PctLive): ", nrow(prevMod$data), "\n N_Row current (PctLive): ", nrow(ma_subset), "\n"))
  
  if(suff_years(ma_subset) & !QAQCPlots){
    cat("---- Sufficient years of data for Density. \n")
    # Determine model family
    # When to use negbinomial or zero-inflated-negbinomial 
    # If zeroes make up >30% of dataset, use zero-inflated
    if(mean(ma_subset$Density_m2 == 0)>0.3){
      fam <- zero_inflated_negbinomial()
    } else {
      fam <- negbinomial()
    }
    # Determine formula (if to add subtidal or quadsize factor)
    # If more than 1 values for either Subtidal or QuadSize_m2, include as fixed effect
    num_subtidals <- length(unique(ma_subset$Subtidal))
    num_quads <- length(unique(ma_subset$QuadSize_m2))
    if(num_subtidals>1 & num_quads>1){
      f <- brms::brmsformula(Density_m2 ~ RelYear + Subtidal + QuadSize_m2 + (1 + RelYear | UniversalReefID))
    } else if(num_subtidals>1 & !num_quads>1) {
      f <- brms::brmsformula(Density_m2 ~ RelYear + Subtidal + (1 + RelYear | UniversalReefID))
    } else if(!num_subtidals>1 & num_quads>1){
      f <- brms::brmsformula(Density_m2 ~ RelYear + QuadSize_m2 + (1 + RelYear | UniversalReefID))
    } else {
      f <- brms::brmsformula(Density_m2 ~ RelYear + (1 + RelYear | UniversalReefID))
    }
    if(abrev=="PISAP"){
      f <- brms::brmsformula(Density_m2 ~ RelYear + (0 + RelYear | UniversalReefID))
    }
    if(abrev=="ANERR" & habitat_type=="Natural"){
      f <- brms::brmsformula(Density_m2 ~ RelYear + Subtidal + QuadSize_m2 + (0 + RelYear | UniversalReefID))
    }
    
    cat("------ Using Formula: ", paste(f[1]), "\n")
    cat("------ Using Family: ", paste(fam[1]), "\n")
    
    if(runDenModel){
      cat("---- Running model. \n")
      den_glmm <- brm(formula=f, data=ma_subset,
                      family=fam, cores=ncores,
                      control= list(adapt_delta=0.995, max_treedepth=20),
                      iter=iter, warmup=warmup, chains=nchains,
                      init=0, thin=3, seed=sample.int(.Machine$integer.max, 1),
                      backend="cmdstanr",
                      file=model_loc,
                      threads = threading(nthreads))
    } else {
      den_glmm <- readRDS(model_loc)
      den_glmm$file <- model_loc
    }
  } else {
    den_glmm <- NA
  }
  
  # Create model results tables and save diagnostic plots and marginal effects plots
  datafile <- ma_subset
  models <- list(den_glmm)
  indicator <- "Density"
  meplotzoom <- FALSE
  oysterresults_temp <- data.frame()
  if(class(den_glmm)=="brmsfit"){
    for(m in seq_along(models)){
      modelobj <- models[[m]]
      sizeclass <- ifelse(str_detect(modelobj$file, "25to75|seed"),
                          "25-75mm", 
                          ifelse(str_detect(modelobj$file, "35to75|seed"),
                                 "35-75mm",
                                 ifelse(str_detect(modelobj$file,
                                                   "o75|market"),
                                        ">75mm", "NA")))
      oyres_i <- setDT(broom.mixed::tidy(modelobj))
      #tidy() does not like that parameter values have underscores for
      #some reason, so the resulting table is incomplete
      
      if(nrow(oyres_i[effect=="fixed", ])-nrow(summary(modelobj)$fixed)==-1){
        missingrow <- data.table(effect="fixed",
                                 component="cond",
                                 #not sure what "cond" means in the tidy summary.
                                 group=NA,
                                 term=rownames(summary(modelobj)$fixed)[2],
                                 estimate=summary(modelobj)$fixed$Estimate[2],
                                 std.error=summary(modelobj)$fixed$Est.Error[2],
                                 conf.low=summary(modelobj)$fixed$`l-95% CI`[2],
                                 conf.high=summary(modelobj)$fixed$`u-95% CI`[2])
        oyres_i <- rbind(oyres_i, missingrow) %>% arrange(effect, group)
      }
      
      oyres_i[, `:=` (indicator=indicator,
                      areaName=unique(datafile[[col_name]]),
                      habitat_class=unique(datafile$HabitatClassification),
                      size_class=sizeclass,
                      live_date_qual=ifelse(
                        str_detect(modelobj$file, "_hist"),
                        "Estimate", "Exact"),
                      n_programs=if(
                        class(try(datafile$LiveDate_Qualifier)) !=
                        "try-error"){
                        length(
                          unique(
                            datafile[LiveDate_Qualifier==
                                       ifelse(
                                         str_detect(
                                           modelobj$file,
                                           "_hist"),
                                         "Estimate",
                                         "Exact"),
                                     ProgramID]))
                      } else{length(unique(datafile[, ProgramID]))},
                      programs=if(class(try(
                        datafile$LiveDate_Qualifier)) != "try-error"){
                        list(unique(datafile[LiveDate_Qualifier==
                                               ifelse(
                                                 str_detect(
                                                   modelobj$file,
                                                   "_hist"),
                                                 "Estimate",
                                                 "Exact"),
                                             ProgramID]))
                      } else{list(unique(datafile[, ProgramID]))},
                      filename=modelobj$file)]
      oysterresults_temp <- rbind(oysterresults_temp, oyres_i)
    }    
  } else {
    sizeclass <- ""
  }
  
  data <- datafile
  
  if(sizeclass != ""){
    size <- case_when(str_detect(sizeclass, "25") &
                        str_detect(sizeclass, "75") ~ "25to75",
                      str_detect(sizeclass, "35") &
                        str_detect(sizeclass, "75") ~ "35to75",
                      str_detect(sizeclass, "25")==FALSE &
                        str_detect(sizeclass, "75") ~ "o75", TRUE ~ "raw")
    sizelab <- case_when(str_detect(sizeclass, "25") &
                           str_detect(sizeclass, "75") ~ "25-75mm",
                         str_detect(sizeclass, "35") &
                           str_detect(sizeclass, "75") ~ "35-75mm",
                         str_detect(sizeclass, "25")==FALSE &
                           str_detect(sizeclass, "75") ~ "\u2265 75mm",
                         TRUE ~ "raw")
  }
  
  nyrs <- max(data$LiveDate)-min(data$LiveDate)+1
  maxyr <- max(data$LiveDate)
  minyr <- min(data$LiveDate)
  yrdiff <- unique(data$YearDiff)
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  # Creates break intervals for plots based on number of years of data
  # Creates break intervals for plots based on number of years of data
  if(nyrs>=40){
    # Set breaks to every 10 years if more than 40 years of data
    brk <- 10
  } else if(nyrs>=20){
    # Set breaks to every 5 years if between 40 and 20 years of data
    brk <- 5
  } else if(nyrs>=12){
    # Set breaks to every 3 years if between 20 and 12 years of data
    brk <- 3
  } else if(nyrs>=8){
    # Set breaks to every 2 years if between 12 and 8 years of data
    brk <- 2
  } else if(nyrs>=5){
    # Set breaks to every year if between 8 and 5 years of data
    brk <- 1
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
  yrlist <- seq(minyr,maxyr,brk)
  
  if(class(den_glmm)=="brmsfit"){
    den_results <- plot_density(den_glmm)
    saveRDS(den_results, paste0(output_path, "model_results/den_mods/Oyster_Dens_GLMM_", abrev, "_", habitat_type, ".rds"))
  }
  
  location_subtitle <- ifelse(analysis=="oimmp", paste0(loc, " OIMMP Region"), loc)
  
  plot1 <- ggplot() +
    {if("meanDen_int" %in% colnames(data)){
      geom_point(data=data, aes(x=LiveDate,
                                y=meanDen_int), position=plot_jitter,
                 shape=21, size=2, color="#333333", fill="#cccccc",
                 alpha=0.8, inherit.aes=FALSE)
    } else{
      geom_point(data=data, aes(x=LiveDate,
                                y=Density_m2), position=plot_jitter,
                 shape=21, size=2, color="#333333", fill="#cccccc",
                 alpha=0.8, inherit.aes=FALSE)
    }} +
    {if(class(den_glmm)=="brmsfit"){
      list(geom_ribbon(data=den_results$summary,
                       aes(x=RelYear+yrdiff,
                           ymin=lower__, ymax=upper__),
                       fill="#000099", alpha=0.1, inherit.aes=FALSE),
           geom_line(data=den_results$summary,
                     aes(x=RelYear+yrdiff,
                         y=estimate__),
                     color="#000099", lwd=0.75, inherit.aes=FALSE))   
    }} +
    scale_x_continuous(limits=c(minyr-0.25, maxyr+0.25), breaks=yrlist) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
    plot_theme +
    {if("meanDen_int" %in% colnames(data)){
      labs(title=paste0("Oyster Density (", habitat_type, ")"),
           subtitle=location_subtitle,
           x="Year",
           y=bquote('Estimated density ('*~m^{-2}*')'))
    }else{
      labs(title=paste0("Oyster Density (", habitat_type, ")"),
           subtitle=location_subtitle,
           x="Year",
           y=bquote('Density ('*~m^{-2}*')'))
    }}
  
  # Specify save location (QAQC Plots saved elsewhere)
  if(QAQCPlots){
    file_name <- paste0(output_path, "QAQC/Oyster_Dens_GLMM_", abrev, "_", habitat_type,
                        ifelse(sizeclass != "", paste0("_",size), "_raw"), ".png")
  } else {
    file_name <- paste0(output_path, "Figures/Density/Oyster_Dens_GLMM_", abrev, "_", habitat_type,
                        ifelse(sizeclass != "", paste0("_",size), "_raw"), ".png")
  }
  
  ggsave(file_name,
         plot1,
         width=8,
         height=4,
         units="in",
         dpi=200)
  
  cat("---- Density plot created for", loc, "-", habitat_type, "\n")
  return(oysterresults_temp)
}

split_tasks <- split(task_list, ceiling(seq_along(1:nrow(task_list)) / 4))
# Subset and save temp .rds objects for each MA (breaks up oysterraw)
for(ma in unique(oysterraw_den[[col_name]])){
  saveRDS(oysterraw_den[oysterraw_den[[col_name]] == ma, ],
          file = paste0(output_path, "tmp/oysterden_", make.names(ma), ".rds"))
}

results_all <- list()
for(b in seq_along(split_tasks)){
  batch <- split_tasks[[b]]
  results_list <- future_lapply(seq_len(nrow(batch)), function(i) {
    task <- batch[i, ]
    oysterraw_path <- paste0(output_path, "tmp/oysterden_", make.names(task[[col_name]]), ".rds")
    oysterraw_sub <- readRDS(oysterraw_path)
    
    cat(paste0("Now starting ", task[[col_name]], "\n"))
    
    density_models_par(
      loc = task[[col_name]],
      habitat_type = task$HabitatType,
      oysterraw_den = oysterraw_sub
    )
    
  }, future.seed = TRUE)
  results_all[[b]] <- data.table::rbindlist(results_list, fill = TRUE)
  gc()
}

if(!QAQCPlots){
  oysterresults_den <- data.table::rbindlist(results_all, fill = TRUE)
  fwrite(oysterresults_den, paste0(output_path, "model_results/oysterresults_den.csv"))
}

#############################
####### Percent Live ########
#############################
oysterraw_pct <- oysterraw[!is.na(PercentLive_pct)]

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
                  Year, Month, !!sym(col_name), SurveyMethod,
                  PercentLiveMethod, HabitatClassification, QuadSize_m2,
                  UniversalReefID, !!sym(plotlab_col), Subtidal,
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

#Add column of decimal versions of percent live values
oysterraw_pct[, PercentLive_dec := PercentLive_pct/100]

#Summarize percent live values
pct_all_sum <- summarySE(oysterraw_pct, measurevar='PercentLive_pct',
                         groupvars=c(eval(col_name), 'Year', 'PercentLiveMethod', 'HabitatClassification'))

# Find out which MAs should receive models for Percent Live
pct_stats <- ma_stats[[analysis]][["Percent Live"]][["MA_Ov_Stats"]] %>% 
  select(eval(col_name), ParameterName, HabitatType) %>%
  as.data.table()

task_list <- pct_stats[, .(HabitatType = unique(HabitatType)), by = eval(col_name)]
task_list <- as.data.frame(task_list)

pctlive_models_par <- function(loc, habitat_type, oysterraw_pct){
  library(data.table)
  library(cmdstanr)
  # Set abbreviation name
  if(analysis=="ma"){
    abrev <- MA_All[ManagedAreaName==loc, Abbreviation]
  } else {
    abrev <- loc
  }
  # Combined MA/loc name with habitat type
  plotlabel <- paste0(loc, "_", str_to_title(habitat_type))
  # At least 5 years of data are required in order to run model analyses
  # Function checks N years of data, returns T or F
  suff_years <- function(data){length(unique(data$Year))>=5}
  # Create subset for each MA
  ma_subset <- subset(oysterraw_pct, oysterraw_pct[[plotlab_col]]==plotlabel)
  # ma_subset <- ma_subset[str_detect(PercentLiveMethod,"Percent"),]
  # Save data used in model
  saveRDS(ma_subset, paste0(output_path, "model_results/data/", abrev, "_PrcLive_", Sys.Date(), "_", habitat_type, ".rds"))
  
  # run new model?
  # Load in previous model (if available) to determine if new data has been added
  # If new data has been added, run model again.
  model_loc <- paste0(output_path, "model_results/GLMMs/", abrev, "_pct_glmm_", habitat_type, ".rds")
  prevMod <- tryCatch({
    readRDS(model_loc)
  }, error = function(e){
    message("Error reading in previous model file: ", conditionMessage(e))
    NULL
  })
  
  if(is.null(prevMod)){
    runPctModel <- TRUE
  } else if(nrow(ma_subset)!=nrow(prevMod$data)){ #Check if amount of data has changed
    runPctModel <- TRUE
  } else {
    runPctModel <- FALSE
  }
  
  print(paste0("Sufficient years of data?: ", suff_years(ma_subset)))
  
  # Don't run model if not enough years of data (5)
  if(!suff_years(ma_subset) & runPctModel){
    runPctModel <- FALSE
  }
  
  print(paste0("Run new model?: ", runPctModel))
  
  # If the above is TRUE, then delete the old model so a new one can be run
  if(runPctModel & !is.null(prevMod)){
    print("Archive old model")
    file.rename(
      from = model_loc,
      to = paste0(output_path, "model_results/GLMMs/archive/", abrev, "_pct_glmm_", habitat_type, "_", Sys.Date(), ".rds")
    )
  }
  cat(paste0("N_Row previous (PctLive): ", nrow(prevMod$data), "\n N_Row current (PctLive): ", nrow(ma_subset), "\n"))
  
  # If enough years of data, perform modelling. If not, plot data points only
  if(suff_years(ma_subset) & !QAQCPlots){
    cat("---- Sufficient years of data. \n")
    # Check to see if previous model already exists
    if(!file.exists(model_loc)){
      
      ma_subset <- as.data.frame(ma_subset)
      ma_subset$LiveSuccess <- round(ma_subset$PercentLive_pct)
      ma_subset$Trials <- 100
      
      # Save data (used in model)
      saveRDS(ma_subset, paste0(output_path, "model_results/data/", abrev, "_PrcLive_binom_", Sys.Date(), "_", habitat_type, ".rds"))
      # Run model
      cat("------ Running model \n")
      # Determine whether to include PercentLiveMethod as a contrast within formula
      if(length(unique(ma_subset$PercentLiveMethod))>1){
        # ManagedAreaName
        # f <- brms::brmsformula(LiveSuccess | trials(Trials) ~ RelYear * PercentLiveMethod + (1 | UniversalReefID)) # binomial
        f <- brms::brmsformula(PercentLive_pct | trunc(lb = 0) ~ RelYear + (1 | PercentLiveMethod) + (1 | UniversalReefID)) # gaussian
        # For OIMMP only
        # f <- brms::brmsformula(LiveSuccess | trials(Trials) ~ RelYear + (1 | PercentLiveMethod) + (1 || ManagedAreaName) + (1 | UniversalReefID))
        # f <- brms::brmsformula(PercentLive_pct ~ RelYear + (1 | PercentLiveMethod) + (1 || ManagedAreaName) + (1 || OIMMP) + (1 | UniversalReefID)) # with guassian
      } else {
        # f <- brms::brmsformula(LiveSuccess | trials(Trials) ~ RelYear + (1 | UniversalReefID)) # binomial
        f <- brms::brmsformula(PercentLive_pct | trunc(lb = 0) ~ RelYear + (1 | UniversalReefID)) # gaussian
      }
      if(runPctModel){
        print(paste0("RUNNING MODEL FOR PERCENT LIVE: ", loc, " - ", habitat_type))
        pct_glmm <- brm(
          formula=f,
          data=ma_subset, family=gaussian, cores=ncores, # family=gaussian
          control= list(adapt_delta=0.995, max_treedepth=20),
          iter=iter, warmup=warmup, chains=nchains, init=0, thin=3,
          seed=4331, backend="cmdstanr", save_pars = save_pars(all = TRUE),
          file=model_loc,
          threads = threading(nthreads)
        )
        pct_glmm <- NA
      }
    } else {
      pct_glmm <- readRDS(model_loc)
      pct_glmm$file <- model_loc
    }
  } else {
    pct_glmm <- NA
  }
  # Create model results tables and save diagnostic plots and marginal effects plots
  datafile <- setDT(ma_subset)
  models <- list(pct_glmm)
  indicator <- "Percent live"
  meplotzoom <- FALSE
  oysterresults_temp <- data.frame()
  
  if(class(pct_glmm)=="brmsfit"){
    for(m in seq_along(models)){
      modelobj <- models[[m]]
      sizeclass <- ifelse(str_detect(modelobj$file, "25to75|seed"),
                          "25-75mm", 
                          ifelse(str_detect(modelobj$file, "35to75|seed"),
                                 "35-75mm",
                                 ifelse(str_detect(modelobj$file,
                                                   "o75|market"),
                                        ">75mm", "NA")))
      oyres_i <- setDT(broom.mixed::tidy(modelobj))
      #tidy() does not like that parameter values have underscores for
      #some reason, so the resulting table is incomplete
      
      if(nrow(oyres_i[effect=="fixed", ])-nrow(summary(modelobj)$fixed)==-1){
        missingrow <- data.table(effect="fixed",
                                 component="cond",
                                 #not sure what "cond" means in the tidy summary.
                                 group=NA,
                                 term=rownames(summary(modelobj)$fixed)[2],
                                 estimate=summary(modelobj)$fixed$Estimate[2],
                                 std.error=summary(modelobj)$fixed$Est.Error[2],
                                 conf.low=summary(modelobj)$fixed$`l-95% CI`[2],
                                 conf.high=summary(modelobj)$fixed$`u-95% CI`[2])
        oyres_i <- rbind(oyres_i, missingrow) %>% arrange(effect, group)
      }
      
      oyres_i[, `:=` (indicator=indicator,
                      areaName=unique(datafile[[col_name]]),
                      habitat_class=unique(datafile$HabitatClassification),
                      size_class=sizeclass,
                      live_date_qual=ifelse(
                        str_detect(modelobj$file, "_hist"),
                        "Estimate", "Exact"),
                      n_programs=if(
                        class(try(datafile$LiveDate_Qualifier)) !=
                        "try-error"){
                        length(
                          unique(
                            datafile[LiveDate_Qualifier==
                                       ifelse(
                                         str_detect(
                                           modelobj$file,
                                           "_hist"),
                                         "Estimate",
                                         "Exact"),
                                     ProgramID]))
                      } else{length(unique(datafile[, ProgramID]))},
                      programs=if(class(try(
                        datafile$LiveDate_Qualifier)) != "try-error"){
                        list(unique(datafile[LiveDate_Qualifier==
                                               ifelse(
                                                 str_detect(
                                                   modelobj$file,
                                                   "_hist"),
                                                 "Estimate",
                                                 "Exact"),
                                             ProgramID]))
                      } else{list(unique(datafile[, ProgramID]))},
                      filename=modelobj$file)]
      oysterresults_temp <- rbind(oysterresults_temp, oyres_i)
    }    
  } else {
    sizeclass <- ""
  }
  
  data <- datafile
  
  ind <- case_when(str_detect(indicator, "ercent") ~ "Pct",
                   str_detect(indicator, "ensity") ~ "Den",
                   str_detect(indicator, "^S|^s") ~ "SH")
  
  if(sizeclass != ""){
    size <- case_when(str_detect(sizeclass, "25") &
                        str_detect(sizeclass, "75") ~ "25to75",
                      str_detect(sizeclass, "35") &
                        str_detect(sizeclass, "75") ~ "35to75",
                      str_detect(sizeclass, "25")==FALSE &
                        str_detect(sizeclass, "75") ~ "o75", TRUE ~ "raw")
    sizelab <- case_when(str_detect(sizeclass, "25") &
                           str_detect(sizeclass, "75") ~ "25-75mm",
                         str_detect(sizeclass, "35") &
                           str_detect(sizeclass, "75") ~ "35-75mm",
                         str_detect(sizeclass, "25")==FALSE &
                           str_detect(sizeclass, "75") ~ "\u2265 75mm",
                         TRUE ~ "raw")
  }
  
  if(ind=="Pct"){
    nyrs <- max(data$LiveDate)-min(data$LiveDate)+1
    maxyr <- max(data$LiveDate)
    minyr <- min(data$LiveDate)
    yrdiff <- unique(data$YearDiff)
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    # Creates break intervals for plots based on number of years of data
    if(nyrs>=40){
      # Set breaks to every 10 years if more than 40 years of data
      brk <- 10
    } else if(nyrs>=20){
      # Set breaks to every 5 years if between 40 and 20 years of data
      brk <- 5
    } else if(nyrs>=12){
      # Set breaks to every 3 years if between 20 and 12 years of data
      brk <- 3
    } else if(nyrs>=8){
      # Set breaks to every 2 years if between 12 and 8 years of data
      brk <- 2
    } else if(nyrs>=5){
      # Set breaks to every year if between 8 and 5 years of data
      brk <- 1
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
    yrlist <- seq(minyr,maxyr,brk)
    
    # Setup shape and color legends as factor in data
    method_levels <- c("Percent", "Point-intercept", "Estimated percent")
    data <- data %>% mutate(PercentLiveMethod = factor(PercentLiveMethod, levels = method_levels))
    cols <- c("Percent" = "#00374f",
              "Point-intercept" = "#0094b0",
              "Estimated percent" = "#4FC3D9")
    shapes <- c("Percent" = 21,
                "Point-intercept" = 24,
                "Estimated percent" = 22)
    # Dummy layer to show all legend values
    legend_seed <- data.frame(LiveDate = min(data$LiveDate, na.rm = TRUE),
                              PercentLive_pct = 0,
                              PercentLiveMethod = factor(method_levels, levels = method_levels))
    
    set.seed(987)
    # Empty list to store necessary plot layers
    plot_layers <- list()
    if(class(pct_glmm)=="brmsfit"){
      pctplots <- plot(conditional_effects(models[[1]], re_formula=NULL), plot=FALSE)
      
      plot_layers <- c(
        geom_ribbon(data = pctplots$RelYear$data,
                    aes(x = RelYear + yrdiff,
                        y = estimate__,
                        ymin = lower__,
                        ymax = upper__),
                    fill = "#000099", alpha = 0.1, inherit.aes = FALSE),
        geom_line(data = pctplots$RelYear$data,
                  aes(x = RelYear + yrdiff,
                      y = estimate__),
                  color = "#000099", lwd = 0.75, inherit.aes = FALSE))
    }
    
    plot_layers <- c(
      plot_layers,
      geom_point(data = legend_seed, 
                 aes(x = LiveDate,
                     y = PercentLive_pct,
                     fill = PercentLiveMethod,
                     shape = PercentLiveMethod),
                 size = 2,
                 color = "#333333",
                 alpha = 0,
                 inherit.aes = FALSE,
                 show.legend = TRUE),
      scale_fill_manual(name = "Percent Live Method", 
                        limits = method_levels,
                        breaks = method_levels,
                        values = cols,
                        drop = FALSE),
      scale_color_manual(name = "Percent Live Method", 
                         limits = method_levels,
                         breaks = method_levels,
                         values = cols,
                         drop = FALSE),
      geom_point(data=data, aes(x=LiveDate,
                                y=PercentLive_pct,
                                fill = PercentLiveMethod,
                                shape = PercentLiveMethod), 
                 position=plot_jitter, size=2, color="#333333",
                 alpha=0.4, inherit.aes=FALSE),
      scale_shape_manual(name = "Percent Live Method", 
                         limits = method_levels,
                         breaks = method_levels,
                         values = shapes,
                         drop = FALSE)
    )
    
    location_subtitle <- ifelse(analysis=="oimmp", paste0(loc, " OIMMP Region"), loc)
    
    plot1 <- ggplot() +
      plot_layers +
      scale_x_continuous(limits=c(minyr-0.25, maxyr+0.25),
                         breaks=yrlist) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
      plot_theme +
      theme(legend.text=element_text(size=10), 
            legend.title=element_text(size=10)) +
      guides(fill = guide_legend(
        override.aes = list(
          shape = unname(shapes[method_levels]),
          fill = unname(cols[method_levels]),
          color = "#333333",
          alpha = 0.4)), shape = "none") +
      {
        if(length(unique(ma_subset$PercentLiveMethod))>1){
          # More than 1 PercentLiveMethod
          location_title <- paste0("Percent Live Oysters (", habitat_type, ")")
          y_axis_label <- "Percent live (%)"
        } else {
          if(unique(ma_subset$PercentLiveMethod)=="Percent"){
            # PercentLiveMethod == Percent
            location_title <- paste0("Percent Live Oysters (", habitat_type, ")")
            y_axis_label <- "Percent live (%)"
          } else if(unique(ma_subset$PercentLiveMethod)=="Estimated percent"){
            # PercentLiveMethod == Estimated percent
            location_title <- paste0("Percent Live Oysters (", habitat_type, ")")
            y_axis_label <- "Estimated percent live (%)"
          } else {
            # PercentLiveMethod == Point-intercept
            location_title <- paste0("Oyster Percent Live Cover (", habitat_type, ")")
            y_axis_label <- "Live cover (%)"
          }
        }
        labs(title = location_title,
             subtitle = location_subtitle,
             x = "Year",
             y = y_axis_label)
      }
    
    # Specify save location (QAQC Plots saved elsewhere)
    if(QAQCPlots){
      file_name <- paste0(output_path, "QAQC/Oyster_PrcLive_GLMM_", 
                          abrev, "_", habitat_type, "_raw.png")
    } else {
      file_name <- paste0(output_path, "Figures/Percent_Live/Oyster_PrcLive_GLMM_", 
                          abrev, "_", habitat_type, "_raw.png")
    }
    
    ggsave(file_name,
           plot1,
           width=8,
           height=4,
           units="in",
           dpi=200)
  }
  cat("---- Percent Live plot created for", loc, "-", habitat_type, "\n")
  return(oysterresults_temp)
}

split_tasks <- split(task_list, ceiling(seq_along(1:nrow(task_list)) / 4))
# Subset and save temp .rds objects for each MA (breaks up oysterraw)
for(ma in unique(oysterraw_pct[[col_name]])){
  saveRDS(oysterraw_pct[oysterraw_pct[[col_name]] == ma, ],
          file = paste0(output_path, "tmp/oysterpct_", make.names(ma), ".rds"))
}

results_all <- list()
for(b in seq_along(split_tasks)){
  batch <- split_tasks[[b]]
  results_list <- future_lapply(seq_len(nrow(batch)), function(i){
    task <- batch[i, ]
    oysterraw_path <- paste0(output_path, "tmp/oysterpct_", make.names(task[[col_name]]), ".rds")
    oysterraw_sub <- readRDS(oysterraw_path)
    
    cat(paste0("Now starting ", task[[col_name]], "\n"))
    
    pctlive_models_par(
      loc = task[[col_name]],
      habitat_type = task$HabitatType,
      oysterraw_pct = oysterraw_sub
    )
    
  }, future.seed = TRUE)
  results_all[[b]] <- data.table::rbindlist(results_list, fill = TRUE)
  gc()
}

if(!QAQCPlots){
  oysterresults_pct <- data.table::rbindlist(results_all, fill = TRUE)
  fwrite(oysterresults_pct, paste0(output_path, "model_results/oysterresults_pct.csv"))
  
  # Combine all results into a single file for processing (Oyster_ResultsCompile.R)
  all_oysterresults <- bind_rows(oysterresults_sh, oysterresults_den, oysterresults_pct)
  
  fwrite(all_oysterresults, paste0(output_path, "GLMM_AllDates_ModelResults.csv"), sep=",")
  saveRDS(all_oysterresults, paste0(output_path, "GLMM_AllDates_ModelResults.rds"))
  
  #Get Rhat values for all models to check which ones may need to be reparameterized
  model_list <- unique(all_oysterresults$filename)
  
  rhats_all <- data.table(filename=character(),
                          term=character(),
                          rhat=numeric())
  rhats_sum <- data.table(filename=character(),
                          rhat=numeric())
  fam_overview <- data.table(filename=character(),
                             family=character(),
                             formula=character())
  
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
    sum <- summary(mod_i)
    familyType <- sum$formula$family$family
    formula <- as.character(mod_i$formula)
    fam_overview <- rbind(fam_overview, data.table(filename=mod,family=familyType,formula=formula))
  }
  
  rhats_all[, rhat_r := round(rhat, 2)]
  rhats_sum[, rhat_r := round(rhat, 2)]
  
  saveRDS(rhats_all, paste0(output_path, "model_results/rhats_all_", Sys.Date(), ".rds"))
  saveRDS(rhats_sum, paste0(output_path, "model_results/rhats_sum_", Sys.Date(), ".rds"))
  fwrite(fam_overview, paste0(output_path, "model_results/model_family_overview_", Sys.Date(), ".csv"))
  
  models_to_check_allrhat <- unique(rhats_all[rhat_r > 1.05, filename])
  models_to_check_sumrhat <- unique(rhats_sum[rhat_r > 1.05, filename])  
}

if(!QAQCPlots){
  # Zip all figures
  out_dir <- paste0(output_path, "Figures")
  fig_list <- list.files(paste0(output_path, "Figures"), recursive = T)
  filename <- paste0("AllOysterFigures_", toupper(analysis))
  setwd(out_dir)
  zip(filename, files=fig_list)
  setwd(wd)  
}
