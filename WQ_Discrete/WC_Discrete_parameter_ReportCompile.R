#This script is created to automate the production of Rmd documents for each relevant combination of
#parameter, relative depth, and activity type for WC data.


## WHEN RUNNING IN RSTUDIO:
## Set working directory to "Source File Location" in "Session" menu


#Load libraries
library(data.table)
library(knitr)
library(readr)
library(plyr)
library(dplyr)
library(here)

#Sets whether to run documents with plots or not (APP_Plots==TRUE to include plots)
APP_Plots <- TRUE
#Set output directory
out_dir <- here::here("WQ_Discrete/output/by_parameter/")

#Sets the list of parameter names to cycle through. This can be edited to limit the number of parameters.
#Easiest way to edit is to comment out undesired parameters.
#If only one parameter is desired, comment out all other parameters and delete comma after remaining parameter
all_params <- c(
   # "Chlorophyll_a_uncorrected_for_pheophytin",
   # "Colored_dissolved_organic_matter_CDOM",
   # "Dissolved_Oxygen",
   "Dissolved_Oxygen_Saturation",
   "pH",
   "Salinity",
   "Secchi_Depth",
   "Total_Nitrogen",
   "Total_Phosphorus",
   "Total_Suspended_Solids_TSS",
   "Turbidity",
   "Water_Temperature"
)

#Sets the list of relative depths to cycle through. This can be edited to limit the number of depths.
#If only one depth is desired, comment out the other depth and delete comma after remaining depth
all_depths <- c(
   "Surface",
   "Bottom"
)

#Sets the list of activity types to cycle through. This can be edited to limit the number of types.
#If only one type is desired, comment out the other type and delete comma after remaining type
all_activity <- c(
   "Field",
   "Sample"
)

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread(here::here("WQ_Discrete/data/ManagedArea.csv"), sep = ",", header = TRUE, stringsAsFactors = FALSE,
                na.strings = "")

#Creates data.table to store all Kendall test results
KT.Stats_all <- data.table(AreaID = integer(),
                           ManagedAreaName = character(),
                           ParameterName = character(),
                           Units = character(),
                           ActivityType = character(),
                           RelativeDepth = character(),
                           N_Data = integer(),
                           N_Years = integer(),
                           EarliestYear = integer(),
                           LatestYear = integer(),
                           SufficientData = logical(),
                           Season = factor(),
                           Median = numeric(),
                           Independent = logical(),
                           tau = numeric(),
                           z = numeric(),
                           p_z = numeric(),
                           chi_sq = numeric(),
                           p_chi_sq = numeric(),
                           SennSlope = numeric(),
                           SennIntercept = numeric(),
                           Trend = integer())

#Starts for loop that cycles through each parameter
for (param_name in all_params){
   #Gets the file with the filename containing the desired parameter
   file_in <- list.files(here::here("WQ_Discrete/data"), pattern=param_name, full=TRUE)
   
   #Since Dissolved_Oxygen will return both Dissolved_Oxygen and Dissolved_Oxygen_Saturation,
   #the if statement removes the entry for Dissolved_Oxygen_Saturation when trying to get Dissolved_Oxygen
   if(param_name=="Dissolved_Oxygen" & length(grep("Saturation", file_in))>0){
      file_in <- file_in[-grep("Saturation", file_in)]
   }
   
   #Starts for loop that cycles through each depth
   for (depth in all_depths){
      #Because secchi depth is does not have a bottom measurement, this statement skips
      #Secchi depth for bottom
      if (param_name=="Secchi_Depth" & depth=="Bottom"){
         next
      }
      
      #Starts for loop that cycles through activity types.
      for (activity in all_activity){
         #Skips Field loops for parameters that only have Sample measurements
         if ((param_name=="Chlorophyll_a_uncorrected_for_pheophytin" |
              param_name=="Colored_dissolved_organic_matter_CDOM" | param_name=="Total_Nitrogen" | 
              param_name=="Total_Phosphorus" | param_name=="Total_Suspended_Solids_TSS") & activity=="Field") {
            next
         #Skips Sample loops for parameters that only have Field measurements
         } else if ((param_name=="Dissolved_Oxygen" | param_name=="Dissolved_Oxygen_Saturation" |
                     param_name=="pH" | param_name=="Secchi_Depth" |
                     param_name=="Water_Temperature") & activity=="Sample") {
            next
         }
         
         #Renders WC_discrete_parameter.Rmd for each parameter combination and writes the report to a pdf
         #Stored in reports/by_parameter directory
         rmarkdown::render(input = here::here("WQ_Discrete/WC_discrete_parameter.Rmd"), 
                           output_format = "pdf_document",
                           output_file = paste0("WC_Discrete_", param_name, "_",
                                                activity, "_", depth,".pdf"),
                           output_dir = here::here("WQ_Discrete/reports/by_parameter"),
                           clean=TRUE)
      }
   }
}

write.csv(KT.Stats_all, here::here("WQ_Discrete/output/WQ_Discrete_All_KTstats.csv"))
