# This script is designed to read the file names for the Kendall Tau results,
# import each one, combine into one data table, arrange by managed area, and write to file

## WHEN RUNNING IN RSTUDIO:
## Set working directory to "Source File Location" in "Session" menu


library(data.table)
library(dplyr)
library(here)

out_dir <- here::here("WQ_Discrete/output/by_parameter/Combined")

#List all of the files in the "tables" directory that are KendallTau results
files <- list.files("WQ_Discrete/output/by_parameter", pattern="KendallTau", full.names=TRUE)

for(i in 1:length(files)){
      if(i==1){
            data <- fread(files[i], sep="|", header=TRUE, stringsAsFactors=FALSE,
                         na.strings="")
      } else{
            data <- bind_rows(data,
                              fread(files[i], sep="|", header=TRUE,
                                    stringsAsFactors=FALSE, na.strings=""))
      }
}
data <- as.data.table(data[order(data$ManagedAreaName, data$ParameterName,
                                 data$RelativeDepth, data$ActivityType), ])

fwrite(data, paste0(out_dir,"/", "WQ_Discrete_All_KendallTau_Stats.txt"), sep="|")
fwrite(data, paste0(out_dir,"/", "WQ_Discrete_All_KendallTau_Stats.csv"), sep=",")
