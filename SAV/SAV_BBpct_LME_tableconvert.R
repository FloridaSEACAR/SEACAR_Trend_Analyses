#This script is designed to read the file names for the LME results of the BBpct analysis,
#import each one, extract the intercept, slope, and p values, then write them to a pipe-delimited file
library(data.table)
library(dplyr)

#List all of the files in the "tables" directory that are LME results
files <- list.files("output/tables/SAV", pattern="lmeresults", full.names=TRUE)

#Include only those that are BBpct
files <- files[grep("BBpct", files)]

# Empty output file
output <- data.table()
#For loop cycles through each file name
for (i in 1:length(files)) {
   #Get filename from list
   filename <- files[i]
   
   #Read in file
   table <- readRDS(filename)
   
   #Keep only rows that are values with "fixed" in the effect column
   table <- table[table$effect=="fixed" & !is.na(table$effect),]
   
   #For each managed area and species, get the LME intercept, slope, and p values
   table <- table %>%
      group_by(managed_area, species) %>%
      summarise(LME_Intercept = estimate[term == "(Intercept)"],
                LME_Slope = estimate[term == "relyear"],
                p = p.value[term == "relyear"],
                .groups = "keep")
   
   #If this is the first file, the table from above is stored as the output table
   #If not the first file, the table is added to the end of the output table
   if(i==1) {
      output <- table
   } else {
      output <- bind_rows(output, table)
   }
}

#Add statistical trend column to denote where p<=0.05 and whether LME_slope increase or decreasing
output$StatisticalTrend <- ifelse(output$p <= 0.05 & output$LME_Slope > 0, "Significantly increasing trend",
                                  ifelse(output$p <= 0.05 & output$LME_Slope <0, "Significantly decreasing trend", "No significant trend"))

#Change column names to better match other outputs
setnames(output, c("managed_area", "species"), c("ManagedAreaName", "Species"))

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
                na.strings = "")

stats <- fread("output/SAV_BBpct_Stats.txt", sep = "|", header = TRUE, stringsAsFactors = FALSE,
               na.strings = "")
setnames(stats, c("analysisunit"), c("Species"))

stats <- merge.data.frame(MA_All[,c("AreaID", "ManagedAreaName")],
                          stats, by="ManagedAreaName", all=TRUE)

stats <-  merge.data.frame(stats, output,
                              by=c("ManagedAreaName", "Species"), all=TRUE)

stats <- as.data.table(stats[order(stats$ManagedAreaName, stats$Species), ])
stats <- stats %>% select(AreaID, everything())

stats$EarliestYear[stats$EarliestYear=="Inf"] <- NA
stats$LatestYear[stats$LatestYear=="-Inf"] <- NA

#filling remaining values in StatisticalTrend column
stats$StatisticalTrend[stats$SufficientData==FALSE] <- "Insufficient data to calculate trend"
stats$StatisticalTrend[stats$SufficientData==TRUE & is.na(stats$LME_Slope)] <- "Model did not fit the available data"

#drop rows where ManagedArea does not contain data
stats <- stats[!apply(stats[, -c(1, 2), drop = FALSE], 1, function(row) all(is.na(row))), ]

# Convert to common names
stats$Species[stats$Species=="Thalassia testudinum"] <- "Turtle grass"
stats$Species[stats$Species=="Syringodium filiforme"] <- "Manatee grass"
stats$Species[stats$Species=="Halodule wrightii"] <- "Shoal grass"
stats$Species[stats$Species=="Ruppia maritima"] <- "Widgeon grass"
stats$Species[stats$Species=="Halophila engelmannii"] <- "Star grass"
stats$Species[stats$Species=="Halophila decipiens"] <- "Paddle grass"

# Change Unidentified Halophila to Halophila, unk.
stats[Species=="Unidentified Halophila", Species := "Halophila, unk."]

#Write output table to a pipe-delimited txt file
fwrite(stats, "output/website/SAV_BBpct_LMEresults_All.txt", sep="|")

#excel format
openxlsx::write.xlsx(stats, "output/website/SAV_BBpct_LMEresults_All.xlsx", colNames = c(TRUE, TRUE), colWidths = c("auto", "auto"), firstRow = c(TRUE, TRUE))
