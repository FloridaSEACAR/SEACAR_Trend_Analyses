#The purpose of this script is to read the file names for the LME results of the BBpct analysis,
#import each one, extract the intercept, slope, and p values, then write them to a pipe-delimited file
library(data.table)
library(dplyr)

#List all of the files in the "tables" directory that are LME results
file_list <- list.files("output", pattern="ModelResults", full.names=TRUE)

#Include only those that are txt
file_in <- file_list[grep("csv", file_list)]

#Set output directory
out_dir <- "output"

#Read in file
data <- fread(file_in, sep = ",", header = TRUE, stringsAsFactors = FALSE, na.strings = "")

#Keep only rows that are values with "fixed" in the effect column
data <- data[data$effect=="fixed" & !is.na(data$effect),]

#For each managed area and species, get the LME intercept, slope, and p values
table <- data %>%
      group_by(managed_area, indicator, live_date_qual, size_class, habitat_class) %>%
      dplyr::reframe(
        Intercept = estimate[term == "(Intercept)"],
        ModelEstimate = estimate[term == "RelYear" | 
                                   term == "meRelYearSampleAge_StdevgrEQQuadIdentifier"],
        StandardError = std.error[term == "RelYear" | 
                                    term == "meRelYearSampleAge_StdevgrEQQuadIdentifier"],
        LowerConfidence = conf.low[term == "RelYear" | 
                                     term == "meRelYearSampleAge_StdevgrEQQuadIdentifier"],
        UpperConfidence = conf.high[term == "RelYear" | 
                                      term == "meRelYearSampleAge_StdevgrEQQuadIdentifier"])

#Change column names to better match other outputs
setnames(table, c("managed_area", "indicator", "size_class", "live_date_qual", "habitat_class"),
         c("ManagedAreaName", "ParameterName", "SizeClass", "ShellType", "HabitatType"))

table$ManagedAreaName[table$ManagedAreaName=="Fort Pickens Aquatic Preserve"] <-
      "Fort Pickens State Park Aquatic Preserve"

table$ManagedAreaName[table$ManagedAreaName=="St. Andrews Aquatic Preserve"] <-
      "St. Andrews State Park Aquatic Preserve"

table$ShellType[table$ShellType=="Exact"] <- "Live Oyster Shells"
table$ShellType[table$ShellType=="Estimate"] <- "Dead Oyster Shells"

table$ParameterName[table$ParameterName=="Size class"] <- "Shell Height"
table$ParameterName[table$ParameterName=="Percent live"] <- "Percent Live"

#Load data summaries
# Gets list of all directories in output
param_dirs <- list.dirs(out_dir, full.name=FALSE)
# Only keeps root directory for parameters
param_dirs <- param_dirs[ !grepl("Figures", param_dirs) ]
param_dirs <- param_dirs[param_dirs!=""]
# Loops through each parameter directory and gets list of Overall stats files
for(i in 1:length(param_dirs)){
  param <- paste0("output/",param_dirs[i])
  if(i==1){
    files<-list.files(param, pattern="Overall", full.name=TRUE)
  } else{
    temp <- list.files(param, pattern="Overall", full.name=TRUE)
    files <- append(files, temp)
  }
}


for(i in 1:length(files)){
  if(i==1){
    data_summ <- fread(files[i], sep="|", header=TRUE, stringsAsFactors=FALSE,
                  na.strings="")
  } else{
    data_summ <- bind_rows(data_summ,
                      fread(files[i], sep="|", header=TRUE,
                            stringsAsFactors=FALSE, na.strings=""))
  }
}

data_summ$ParameterName[data_summ$ParameterName=="ShellHeight_mm"] <- "Shell Height"
data_summ$ParameterName[data_summ$ParameterName=="Density_m2"] <- "Density"
data_summ$ParameterName[data_summ$ParameterName=="PercentLive_pct"] <- "Percent Live"


table <- merge.data.frame(data_summ, table, by=c("ManagedAreaName", "ParameterName",
                                                 "ShellType", "SizeClass", "HabitatType"),
                             all=TRUE)

table <- as.data.table(table[order(table$ManagedAreaName, table$ParameterName,
                                   table$ShellType, table$SizeClass,
                                   table$HabitatType), ])
table <- table %>% select(AreaID, everything())

#Write output table to a csv and pipe-delimited txt file
fwrite(table, "output/Oyster_All_GLMM_Stats.txt", sep="|")
fwrite(table, "output/Oyster_All_GLMM_Stats.csv", sep=",")

###### Compile data used for plots

#List all of the files in the "Tables" directory that are Shell Heights
file_list <- list.files("data/GLMMs/AllDates/Data", pattern="_sh", full.names=TRUE)

for(i in 1:length(file_list)){
  if(i==1){
    data <- readRDS(file_list[i])
    data$ProgramID <- as.character(data$ProgramID)
  } else{
    temp_data <- readRDS(file_list[i])
    temp_data$ProgramID <- as.character(temp_data$ProgramID)
    data <- bind_rows(data, temp_data)
  }
}

#Write output table to a csv and pipe-delimited txt file
fwrite(data, "output/Shell_Height/Oyster_SH_plotdata.txt", sep="|")
fwrite(data, "output/Shell_Height/Oyster_SH_plotdata.csv", sep=",")


#List all of the files in the "tables" directory that are Density
file_list <- list.files("data/GLMMs/AllDates/Data", pattern="_n_", full.names=TRUE)

for(i in 1:length(file_list)){
  if(i==1){
    data <- readRDS(file_list[i])
    data$ProgramID <- as.character(data$ProgramID)
  } else{
    temp_data <- readRDS(file_list[i])
    temp_data$ProgramID <- as.character(temp_data$ProgramID)
    data <- bind_rows(data, temp_data)
  }
}

#Write output table to a csv and pipe-delimited txt file
fwrite(data, "output/Density/Oyster_Den_plotdata.txt", sep="|")
fwrite(data, "output/Density/Oyster_Den_plotdata.csv", sep=",")


#List all of the files in the "tables" directory that are Density
file_list <- list.files("data/GLMMs/AllDates/Data", pattern="_p_", full.names=TRUE)

for(i in 1:length(file_list)){
  if(i==1){
    data <- readRDS(file_list[i])
    data$ProgramID <- as.character(data$ProgramID)
  } else{
    temp_data <- readRDS(file_list[i])
    temp_data$ProgramID <- as.character(temp_data$ProgramID)
    data <- bind_rows(data, temp_data)
  }
}

#Write output table to a csv and pipe-delimited txt file
fwrite(data, "output/Percent_Live/Oyster_Pct_plotdata.txt", sep="|")
fwrite(data, "output/Percent_Live/Oyster_Pct_plotdata.csv", sep=",")
