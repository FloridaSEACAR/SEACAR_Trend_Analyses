library(stringr)
library(dplyr)
library(data.table)
library(ggplot2)
library(grid)
library(kableExtra)
library(cowplot)

all_depths <- c("Surface","Bottom","All")
all_activities <- c("Field","Lab","All")
all_params_short <- c(
  "ChlaC",
  "Chla",
  "CDOM",
  "DO",
  "DOS",
  "pH",
  "Sal",
  "Secchi",
  "TN",
  "TP",
  "TSS",
  "Turb",
  "TempW"
)

############################
######## FUNCTIONS #########
############################

# function of parameter, activity type, depth, with specified filetype
# retrieves RDS filepath to be loaded
get_files <- function(p, a, d, filetype) {
  
  # Declaring RDS file list of respective tables
  files <- list.files(here::here("output/tables/disc"),pattern = "\\.rds$")
  
  # "data" contains overall data for each param, regardless of depth/activity
  if (filetype == "data") {
    pattern <- paste0(p,"_",filetype)
    
  } else {
    pattern <- paste0(p,"_",a,"_",d,"_",filetype)
  }
  # subset directory files for given pattern
  file_return <- str_subset(files, pattern)
  return(file_return)
}

#function to check the number of managed areas for each p,a,d combination
n_managedareas <- function(p, a, d) {
  # Declaring n value as count of managed areas
  # return 0 if unable to load file (activity/depth combo not available for that param)
  n <- tryCatch(
    {
      ma_file <- get_files(p, a, d, "MA_Include")
      ma_inclusion <- readRDS(paste0("output/tables/disc/", ma_file))
      n <- length(ma_inclusion)
      rm(ma_inclusion)
      n
    },
    error = function(e) {
      0
    },
    warning = function(w) {
      0
    }
  )
  return(n)
}

#function to make a list of managed area names
get_managed_area_names <- function(p, a, d) {
  ma_list <- with(
    readRDS(paste0("output/tables/disc/",get_files(p, a, d, "MA_MMYY"))),
    {
      unique(ManagedAreaName)
    }
  )
  return(list(ma_list))
}

#results list to record managed areas for each combination
results_list <- list()

for (param in all_params_short) {
  if (param == "Secchi"){
    depth <- "Surface"
  } else {
    depth <- "All"
  }
  
  # Choosing which analyses to plot, when to combine 
  if (param == "ChlaC" |
      param == "Chla" |
      param == "CDOM" |
      param == "TN" |
      param == "TP") {activity = "Lab"} else if (
        param == "DO" |
        param == "DOS" |
        param == "pH" |
        param == "Secchi" |
        param == "TempW") {activity = "Field"} else if (
          param == "Sal" |
          param == "TSS" |
          param == "Turb") {activity = "All"}
  
  n <- n_managedareas(param, activity, depth)
  
  if (n > 0) {
    # print(n)
    managed_area_names <- get_managed_area_names(param, activity, depth)
    
    # Concatenate the managed area names into a single character vector
    concatenated_names <- unlist(managed_area_names)
    
    # Create a data frame for the current combination
    result_df <- data.frame(Parameter = param,
                            Depth = depth,
                            Activity = activity,
                            ManagedAreaName = paste(concatenated_names))
    
    # Append the result data frame to the list
    results_list <- c(results_list, list(result_df))
    rm(result_df, concatenated_names, managed_area_names, n)
    
  } else {
    print(0)
  }
}

# Bind the list of data frames using bind_rows()
managed_area_df <- bind_rows(results_list)

disc_managed_areas <- unique(managed_area_df$ManagedAreaName)

## Setting plot theme for plots
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


## Load Data Table Function
## For loading discrete data
load_data_table <- function(p, a="All", d="All", table) {
  
  # Declaring RDS file list of respective tables
  files <- list.files(here::here("output/tables/disc"),pattern = "\\.rds$")
  
  if (table == "data") {
    filename_string <- paste0(p,"_",table)
  } else {
    filename_string <- paste0(p,"_",a,"_",d,"_",table)
  }
  
  # subset file list to select desired table RDS file
  table_file <- paste0("output/tables/disc/",str_subset(files, filename_string))
  
  # importing RDS files
  df <- lapply(table_file, readRDS)
  
  return(df)
}

# Pie chart to show Program proportions of VQ data
vq_piechart <- function(ma, data){
  # list of programs with VQ data
  vq <- data %>% 
    filter(ManagedAreaName==ma, !is.na(ValueQualifier)) %>%
    group_by(ProgramID, ProgramName) %>%
    summarise(N_VQ = n())
  
  myPalette <- brewer.pal(nrow(vq), "Set2")
  cat("  \n")
  pie(vq$N_VQ, labels = vq$ProgramID, border="white", col=myPalette, radius=0.4)
  cat("  \n")
}

### Discrete sample location maps
plot_discrete_maps <- function(ma, data, param_label, ma_abrev){
  map_output <- "output/maps/discrete/"
  
  # Grab a list of programs within {discrete parameter} data for each MA
  disc_programs <- data %>% filter(ManagedAreaName == ma) %>% distinct(ProgramID, ProgramName)
  
  # grab sample coordinates from those programs
  coord_df <- locs_pts_rcp %>% filter(ProgramID %in% disc_programs$ProgramID)
  
  # frame to plot coordinates, allows for bubble size display of n_samples
  ma_data <- data %>% filter(ManagedAreaName == ma, ProgramID %in% disc_programs$ProgramID) %>%
    group_by(ProgramLocationID) %>%
    summarise(n_data = n()) %>%
    rename(ProgramLoc = ProgramLocationID)
  
  # merge frames together prior to plotting
  discrete_df <- merge(ma_data, coord_df)
  discrete_df <- discrete_df[order(discrete_df$n_data, decreasing=TRUE), ]
  
  # locate shape file for a given MA
  ma_shape <- find_shape(ma)
  
  # get coordinates to set zoom level
  shape_coordinates <- get_shape_coordinates(ma_shape)
  
  # setting color palette
  # pal <- colorFactor(seacar_palette, discrete_df$ProgramID)
  pal <- colorFactor("plasma", discrete_df$ProgramID)
  
  # leaflet map
  map <- leaflet(discrete_df, options = leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
    addPolygons(data=ma_shape, color="black", weight = 1, smoothFactor = 0.5, opacity = 0.8, fillOpacity = 0.1) %>%
    addCircleMarkers(lat=~Latitude_D, lng=~Longitude_, color=~pal(ProgramID), weight=0.5, radius=sqrt(discrete_df$n_data), fillOpacity=0.3) %>%
    addLegend(pal=pal, values=~ProgramID, labFormat=labelFormat(prefix="Program "), title="") %>%
    fitBounds(lng1=shape_coordinates$xmin,
              lat1=shape_coordinates$ymin,
              lng2=shape_coordinates$xmax,
              lat2=shape_coordinates$ymax)
  
  # map output filepath
  map_out <- paste0(map_output, ma_abrev, "_wc_discrete.png")
  
  # save file as png
  mapshot(map, file = map_out)
  
  # draw .png with ggplot
  p1 <- ggdraw() + draw_image(map_out, scale = 1)
  
  # captions / label
  cat("\\newpage")
  caption = paste0("Map showing location of Discrete sampling sites for ", param_label, "  \n")
  
  cat("  \n")
  cat(caption)
  print(p1)
  cat("  \n")
  cat("The bubble size on the above plots reflects the amount of data available at each sampling site")
  cat("  \n")
}

## Kendall-Tau Trendlines Plot function ##
plot_trendlines <- function(p, a, d, activity_label, depth_label, y_labels, parameter, data, include_map=TRUE, ma, ma_abrev) {
  cat("  \n")
  cat(glue("**Seasonal Kendall-Tau Trend Analysis**"), "  \n")
  
  MA_YM_Stats <- as.data.frame(load_data_table(p, a, d, "MA_MMYY_Stats"))
  skt_stats <- as.data.frame(load_data_table(p, a, d, "skt_stats"))
  
  setDT(skt_stats)
  setDT(MA_YM_Stats)
  
  ### SKT STATS ###
  # Gets x and y values for starting point for trendline
  KT.Plot <- skt_stats %>%
    dplyr::group_by(ManagedAreaName) %>%
    dplyr::summarize(x=decimal_date(EarliestSampleDate),
              y=(x-EarliestYear)*SennSlope+SennIntercept)
  # Gets x and y values for ending point for trendline
  KT.Plot2 <- skt_stats %>%
    dplyr::group_by(ManagedAreaName) %>%
    dplyr::summarize(x=decimal_date(LastSampleDate),
              y=(x-EarliestYear)*SennSlope+SennIntercept)
  # Combines the starting and endpoints for plotting the trendline
  KT.Plot <- bind_rows(KT.Plot, KT.Plot2)
  rm(KT.Plot2)
  KT.Plot <- as.data.table(KT.Plot[order(KT.Plot$ManagedAreaName), ])
  KT.Plot <- KT.Plot[!is.na(KT.Plot$y),]
  setDT(KT.Plot)
  
  # Checking for missing values
  check_ym <- MA_YM_Stats %>%
    dplyr::filter(ManagedAreaName == ma)
  
  if (nrow(check_ym) == 0) {
    invisible()
    # print("error")
  } else {
    # Gets data to be used in plot for managed area
    plot_data <- MA_YM_Stats[ManagedAreaName==ma,]
    
    # Gets trendline data for managed area
    KT.plot_data <- KT.Plot[ManagedAreaName==ma,]
    
    #Determine max and min time (Year) for plot x-axis
    t_min <- min(plot_data$Year)
    t_max <- max(plot_data$YearMonthDec)
    t_max_brk <- as.integer(round(t_max, 0))
    t <- t_max-t_min
    min_RV <- min(plot_data$Mean)
    
    # Sets break intervals based on the number of years spanned by data
    if(t>=30){
      brk <- -10
    }else if(t<30 & t>=10){
      brk <- -4
    }else if(t<10 & t>=4){
      brk <- -2
    }else if(t<4){
      brk <- -1
    }
    
    # Create plot object with data and trendline
    p1 <- ggplot(data=plot_data,
                 aes(x=YearMonthDec, y=Mean)) +
      # geom_line(size=0.75, color="#333333", alpha=0.6) +
      geom_point(shape=21, size=3, color="#333333", fill="#cccccc",
                 alpha=0.75) +
      geom_line(data=KT.plot_data, aes(x=x, y=y),
                color="#000099", size=1.2, alpha=0.7) +
      labs(title=paste0(parameter,", ",activity_label, ", ",depth_label),
           subtitle=ma,
           x="Year", y=y_labels) +
      scale_x_continuous(limits=c(t_min-0.25, t_max+0.25),
                         breaks=seq(t_max_brk, t_min, brk)) +
      plot_theme
    # Creates ResultTable to display statistics below plot
    ResultTable <- skt_stats[ManagedAreaName==ma, ] %>%
      select(RelativeDepth, N_Data, N_Years, Median, Independent, tau, p,
             SennSlope, SennIntercept, ChiSquared, pChiSquared, Trend)
    # Create table object
    t1 <- ggtexttable(ResultTable, rows=NULL,
                      theme=ttheme(base_size=10)) %>%
      tab_add_footnote(text="p < 0.00005 appear as 0 due to rounding.\n
              SennIntercept is intercept value at beginning of
              record for monitoring location",
                       size=10, face="italic")
    
    # Arrange and display plot and statistic table
    print(ggarrange(p1, t1, ncol=1, heights=c(0.85, 0.15)))
    cat("  \n")
    
    #####################
    ### Discrete Maps ###
    #####################
    
    if (include_map==TRUE){
      plot_discrete_maps(ma, data, param_label = parameter, ma_abrev = ma_abrev)
    }
    
    #####################
    #####################
    
    # Included Programs
    program_table <- data %>%
      filter(ManagedAreaName == ma) %>%
      group_by(ProgramID) %>%
      mutate(YearMin = min(Year),
             YearMax = max(Year),
             N_Data = length(ResultValue)) %>%
      distinct(ProgramID, ProgramName, N_Data, YearMin, YearMax) %>%
      select(ProgramID, ProgramName, N_Data, YearMin, YearMax) %>%
      arrange(desc(N_Data))
    
    program_kable <- kable(program_table %>% select(-ProgramName),
                           format="simple",
                           caption=paste0("Programs contributing data for ", parameter),
                           col.names = c("*ProgramID*","*N_Data*","*YearMin*","*YearMax*"))
    
    print(program_kable)
    cat("  \n")
    
    # program names listed below (accounting for long names)
    program_ids <- unique(program_table$ProgramID)
    
    cat("\n **Program names:** \n \n")
    
    # Display ProgramName below data table
    for (p_id in program_ids) {
      p_name <- program_table %>% filter(ProgramID == p_id) %>% pull(ProgramName)
      cat(paste0("*",p_id,"*", " - ",p_name, "  \n"))
    }
    
    cat("  \n")
    
    rm(plot_data, program_kable, program_table)
    rm(MA_YM_Stats)
    # rm(KT.Plot)
    rm(skt_stats)
  }
}

## Boxplots function ##
plot_boxplots <- function(p, a, d, activity_label, depth_label, y_labels, parameter, data) {
  # data <- as.data.frame(load_data_table(p, a, d, "data"))
  
  plot_title <- paste0(parameter,", ",activity_label, ", ",depth_label)
  
  # Determine upper and lower bounds of time for x-axis
  plot_data <- data[data$Include==TRUE &
                      data$ManagedAreaName==ma,]
  # plot_data <- data[data$ManagedAreaName==ma,]
  year_lower <- min(plot_data$Year)
  year_upper <- max(plot_data$Year)
  
  # Determine upper and lower bounds of ResultValue for y-axis
  min_RV <- min(plot_data$ResultValue)
  mn_RV <- mean(plot_data$ResultValue[plot_data$ResultValue <
                                        quantile(data$ResultValue, 0.98)])
  sd_RV <- sd(plot_data$ResultValue[plot_data$ResultValue <
                                      quantile(data$ResultValue, 0.98)])
  # Sets x- and y-axis scale
  x_scale <- ifelse(year_upper - year_lower > 30, 10, 5)
  y_scale <- mn_RV + 4 * sd_RV
  
  ##Year plots
  # Create plot object for auto-scaled y-axis plot
  p1 <- ggplot(data=plot_data,
               aes(x=Year, y=ResultValue, group=Year)) +
    geom_boxplot(color="#333333", fill="#cccccc", outlier.shape=21,
                 outlier.size=3, outlier.color="#333333",
                 outlier.fill="#cccccc", outlier.alpha=0.75) +
    labs(subtitle="By Year",
         x="Year", y=y_labels) +
    scale_x_continuous(limits=c(year_lower - 1, year_upper + 1),
                       breaks=rev(seq(year_upper,
                                      year_lower, -x_scale))) +
    plot_theme
  
  p4 <- ggplot(data=plot_data,
               aes(x=YearMonthDec, y=ResultValue,
                   group=YearMonth, color=as.factor(Month))) +
    geom_boxplot(fill="#cccccc", outlier.size=1.5, outlier.alpha=0.75) +
    labs(subtitle="By Year and Month",
         x="Year", y=y_labels, color="Month") +
    scale_x_continuous(limits=c(year_lower - 1, year_upper + 1),
                       breaks=rev(seq(year_upper,
                                      year_lower, -x_scale))) +
    plot_theme +
    theme(legend.position="none")
  
  # Month Plots
  # Create plot object for auto-scaled y-axis plot
  p7 <- ggplot(data=plot_data,
               aes(x=Month, y=ResultValue,
                   group=Month, fill=as.factor(Month))) +
    geom_boxplot(color="#333333", outlier.shape=21, outlier.size=3,
                 outlier.color="#333333", outlier.alpha=0.75) +
    labs(subtitle="By Month",
         x="Month", y=y_labels, fill="Month") +
    scale_x_continuous(limits=c(0, 13), breaks=seq(3, 12, 3)) +
    plot_theme +
    theme(legend.position="none",
          axis.text.x=element_text(angle = 0, hjust = 1))
  
  set <- ggarrange(p1 + rremove("ylab"), p4 + rremove("ylab"), p7 + rremove("ylab"), ncol=1)
  
  p0 <- ggplot() + labs(title=plot_title, 
                        subtitle=ma) + 
    plot_theme +
    theme(panel.border=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_blank())
  
  annotate_figure(p0, left = textGrob(y_labels, rot = 90, vjust = 1, gp = gpar(cex = 1.3)))
  
  # Arrange title on plots
  Yset <- ggarrange(p0, set, ncol=1, heights=c(0.07, 1))
  Yset_annotated <- annotate_figure(Yset,
                                    left = text_grob(y_labels, rot = 90, family = "Arial", size = 10))
  
  print(Yset_annotated)
  
  rm(plot_data)
  rm(p1, p4, p7, p0, Yset, Yset_annotated)
}

## VQ Summary Barplot ##
plot_vq_barplot <- function(p, a, d, activity_label, depth_label, y_labels, parameter, data, include_plot, pie_chart, ma) {
  
  VQ_Summary <- as.data.frame(load_data_table(p, a, d, "VQSummary"))
  
  # Filter and subset dataframe for managed area
  ma_vq_summary <- VQ_Summary %>% filter(ManagedAreaName == ma)
  
  # VQSummary conditions for qualifying VQ values
  vq_condition <- ma_vq_summary$N_H !=0 | ma_vq_summary$N_I != 0 | ma_vq_summary$N_Q != 0 | ma_vq_summary$N_S != 0 | ma_vq_summary$N_U != 0
  
  # apply VQ_conditions to subset dataframe
  filtered_vq <- ma_vq_summary[vq_condition, ]
  
  # check to see if there are any qualifying VQ values, if not, skip
  if (nrow(filtered_vq) != 0) {
    
    # select respective perc_vq columns
    plot_data <- filtered_vq %>% 
      dplyr::select(Year, N_Total, N_H, perc_H, N_I, perc_I, N_Q, perc_Q, N_S, perc_S, N_U, perc_U) %>%
      dplyr::mutate_if(is.numeric, round, 2)
    
    # show only relevant columns for table display
    plot_data <- plot_data %>% 
      dplyr::select(-where(~ all(. == 0)))
    
    # convert data format to "long" for plotting
    plot_data_long <- tidyr::pivot_longer(plot_data, 
                                          cols = starts_with("perc_"), 
                                          names_to = "Category", 
                                          values_to = "Percentage")
    
    # remove values when their VQ not included
    plot_data_long <- plot_data_long %>% 
      dplyr::filter(Percentage != 0)
    
    # set year bounds for upper and lower
    year_lower <- min(plot_data_long$Year)
    year_upper <- max(plot_data_long$Year)
    
    # Use similar x-scaling to previous charts # may change
    x_scale <- ifelse(year_upper - year_lower > 30, 10, 
                      ifelse(year_upper == year_lower, 1, 3))
    
    # set title label
    lab_title <- paste0("Percentage Distribution of Value Qualifiers by year for ", d," Depths -  ", parameter)
    
    # plot results
    vq_plot <- ggplot(plot_data_long, aes(x=Year, y=Percentage, fill=Category)) + 
      geom_bar(stat = "identity", position="stack") +
      labs(title = lab_title,
           subtitle = paste(ma),
           x = "Year",
           y = "Percentage") +
      ylim(0, 100) +
      scale_x_continuous(limits=c(year_lower - 1, year_upper + 1),
                         breaks=rev(seq(year_upper,
                                        year_lower, -x_scale))) +
      scale_fill_manual(values=c("#00ADAE","#65CCB3","#AEE4C1","#FDE8A8","#F8CD6D"),
                        breaks=c("perc_H","perc_I","perc_Q","perc_S","perc_U"),
                        labels=c("H", "I", "Q", "S", "U")) +
      plot_theme
    
    # print plots if include=TRUE
    if (include_plot==TRUE){
      print(vq_plot)
      cat("  \n")
    }
    
    if (pie_chart==TRUE){
      vq_piechart(ma, data)
    }
    
    
    # Replace 0 values with NA, to be modified to empty string with kable function
    plot_data[plot_data == 0] <- NA
    options(knitr.kable.NA = "")
    
    # italicized col_names determined dynamically
    col_names <- list()
    for (k in 1:length(names(plot_data))){
      col <- names(plot_data)[k]
      new_col <- paste0("*",col,"*")
      col_names <- c(col_names, new_col)
    }
    
    cat("  \n")
    cat("**Value Qualifiers**  \n \n")
    
    # add description for each VQ shown
    vq <- list("N_H","N_I","N_Q","N_S","N_U")
    vq_desc <- list("H - Value based on field kit determiniation; results may not be accurate. 
                This code shall be used if a field screening test (e.g., field gas chromatograph data, 
                immunoassay, or vendor-supplied field kit) was used to generate the value and the field 
                kit or method has not been recognized by the Department as equivalent to laboratory methods.",
                    
                    "I - The reported value is greater than or equal to the laboratory method detection 
                limit but less than the laboratory practical quantitation limit.",
                    
                    "Q - Sample held beyond the accepted holding time. This code shall be used if the value is derived 
                from a sample that was prepared or analyzed after the approved holding time restrictions for sample 
                preparation or analysis.",
                    
                    "S - Secchi disk visible to bottom of waterbody. The value reported is the depth of the waterbody 
                at the location of the Secchi disk measurement.",
                    
                    "U - Indicates that the compound was analyzed for but not detected. This symbol shall be used to indicate 
                that the specified component was not detected. The value associated with the
                qualifier shall be the laboratory method detection limit. Unless requested by the client, 
                less than the method detection limit values shall not be reported ")
    
    vq_list <- setNames(as.list(vq_desc), vq)
    
    vq_footnotes <- list()
    # add description for each VQ shown
    # loop to add description if the corresponding VQ is listed above
    for (vq in names(vq_list)) {
      if (vq %in% names(plot_data)) {
        vq_footnote <- unlist(vq_list[vq])
        vq_footnotes <- c(vq_footnotes, vq_footnote)
        cat("\n")
      }
    }
    
    vq_footnote_description <- list("*N_Total* is total amount of data for a given year", 
                                    "*N_* is the total amount of values flagged with the respective value qualifier in a given year",
                                    "*perc_* is the percent of data flagged with the respective value qualifier as a proportion of *N_Total*")
    
    for (desc in vq_footnote_description){
      cat(paste0("* ",desc, "\n"))
    }
    
    # add text table beneath plot
    vq_table <- kable(plot_data, 
                      format="simple",
                      digits = 1,
                      caption=paste0("Value Qualifiers for ", parameter),
                      col.names = col_names,
                      row.names = FALSE) %>%
      kable_styling(latex_options="scale_down",
                    position = "center")
    
    vq_table <- vq_table %>% add_footnote(label = vq_footnotes,
                                          notation = "number")
    
    print(vq_table)
    cat(" \n")
    
    # list of programs with VQ data
    vq <- data %>% 
      filter(Include==TRUE, ManagedAreaName==ma, ValueQualifier!="NA") %>%
      select(ProgramID, ProgramName)
    
    vq_program_id <- unique(vq$ProgramID)
    
    cat("\n **Programs containing Value Qualified data:** \n \n")
    
    # Display ProgramName below data table
    for (p_id in vq_program_id) {
      p_name <- unlist(unique(vq %>% filter(ProgramID == p_id) %>% select(ProgramName)))
      cat(paste0("*",p_id,"*", " - ",p_name, "  \n"))
    }
    
    cat("  \n")
    
    rm(VQ_Summary, filtered_vq, plot_data, plot_data_long, vq_plot)
  } else {
    cat(paste0("There are no qualifying Value Qualifiers for ", parameter, " in ", ma))
    cat("\n \n \n")
  }
}