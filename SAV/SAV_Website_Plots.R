#This script is designed to read the file names for the trendplots and bar plots
#Plot files are either copied to the website directory or the rds file is import
#and converted to a png
library(data.table)
library(dplyr)
library(ggplot2)
library(colorspace)
library(here)
windowsFonts(`Segoe UI` = windowsFont('Segoe UI'))

###Bar Plot
#List all of the trendplot jpg files
files <- list.files(here::here("SAV/output/Figures/BB/"), pattern="barplot")

#Set the directory with the input rds files and directory to save to
in_dir <- here::here("SAV/output/Figures/BB/")
out_dir <- here::here("SAV/output/website/images/barplots/")

#For loop the cycles through each barplot file
for(i in 1:length(files)){
      #Imports the barplot object
      barplot <- readRDS(paste0(in_dir, "/", files[i]))
      barplot <- barplot + theme_bw() +
         theme(text=element_text(family="Segoe UI"),
               title=element_text(face="bold"),
               plot.title=element_text(hjust=0.5, size=14, color="#314963"),
               plot.subtitle=element_text(hjust=0.5, size=10, color="#314963"),
               axis.title.x = element_text(margin = margin(t = 5, r = 0,
                                                           b = 10, l = 0)),
               axis.title.y = element_text(margin = margin(t = 0, r = 10,
                                                           b = 0, l = 0)),
               axis.text=element_text(size=10),
               axis.text.x=element_text(face="bold", angle = 60, hjust = 1),
               axis.text.y=element_text(face="bold"))
      
      #Changes output name from ending in rds to png
      outname <- substr(files[i], 1, nchar(files[i])-3)
      outname <- paste0(outname, "png")
      
      #Creates and saves png file of plot
      png(paste0(out_dir,outname),
          width = 8,
          height = 4,
          units = "in",
          res = 200)
      
      print(barplot)
      dev.off()
}