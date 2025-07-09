library(stringr)
library(dplyr)
library(data.table)
library(ggplot2)
library(grid)
library(kableExtra)
library(cowplot)

# Function to generate trend text for model results
checkTrends <- function(p, Slope, SufficientData){
  if(SufficientData){
    if(is.na(Slope)){
      return("Model did not fit the available data")
    } else {
      increasing <- Slope > 0
      trendPresent <- p <= 0.05
      trendStatus <- "No significant trend"
      if(trendPresent){
        trendStatus <- ifelse(increasing, "Significantly increasing trend", 
                              "Significantly decreasing trend")
      }          
    }
  } else {
    trendStatus <- "Insufficient data to calculate trend"
  }
  return(trendStatus)
}

skt_stats_disc <- fread("../WQ_Cont_Discrete/output/WQ_Discrete_All_KendallTau_Stats.txt", sep='|')
skt_stats_disc$`Period of Record` <- paste0(skt_stats_disc$EarliestYear, " - ", skt_stats_disc$LatestYear)
skt_stats_disc <- skt_stats_disc %>% rowwise() %>% mutate(
  `Statistical Trend` = checkTrends(`p` = p, Slope = SennSlope, SufficientData = SufficientData)
) %>% as.data.table()

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

# Load in discrete snippets where possible
discSnippets <- setDT(openxlsx::read.xlsx("data/discreteSnippets.xlsx"))
snippetParams <- discSnippets[!is.na(Snippet), ParameterShort]

############################
######## FUNCTIONS #########
############################
disc_file_loc <- "../WQ_Cont_Discrete/output/tables/disc/"

# function of parameter, activity type, depth, with specified filetype
# retrieves RDS filepath to be loaded
get_files <- function(p, a, d, filetype) {
  # Declaring RDS file list of respective tables
  files <- list.files(disc_file_loc, pattern = "\\.rds$")
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
      ma_inclusion <- readRDS(paste0(disc_file_loc, ma_file))
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
    readRDS(paste0(disc_file_loc,get_files(p, a, d, "MA_MMYY"))),
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
managed_area_df$ManagedAreaName[managed_area_df$ManagedAreaName=="St. Andrews State Park Aquatic Preserve"] <- "St. Andrews Aquatic Preserve"
managed_area_df$ManagedAreaName[managed_area_df$ManagedAreaName=="Southeast Florida Coral Reef Ecosystem Conservation Area"] <- "Kristin Jacobs Coral Aquatic Preserve"

disc_managed_areas <- unique(managed_area_df$ManagedAreaName)

## Load Data Table Function
## For loading discrete data
load_data_table <- function(p, a="All", d="All", table) {
  
  # Declaring RDS file list of respective tables
  files <- list.files(disc_file_loc, pattern = "\\.rds$")
  
  if (table == "data") {
    filename_string <- paste0(p,"_",table)
  } else {
    filename_string <- paste0(p,"_",a,"_",d,"_",table)
  }
  
  # subset file list to select desired table RDS file
  table_file <- paste0(disc_file_loc, str_subset(files, filename_string))
  
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
plot_discrete_maps <- function(ma_abrev, param_short, param_label, map_files){
  # Locate map
  map_loc <- str_subset(map_files, paste0("_", param_short, "_", ma_abrev, "_map"))
  # captions / label
  cat("\\newpage")
  caption <- paste0("Map showing location of discrete water quality sampling locations within the boundaries of *", ma, 
                    "*. The bubble size on the maps above reflect the amount of data available at each sampling site.  \n")
  cat("  \n")
  # Print map
  subchunkify(cat("![", caption, "](", map_loc,")"))
  cat("  \n")
}

## Kendall-Tau Trendlines Plot function ##
plot_trendlines <- function(param, ma, ma_abrev, report_type){
  format_type <- ifelse(report_type=="HTML", "simple", "latex")
  cat("  \n")
  cat(glue("**Seasonal Kendall-Tau Trend Analysis**"), "  \n")
  
  skt_stats <- skt_stats_disc[ParameterName==param & ManagedAreaName==ma & Website==1, ]
  
  if (nrow(skt_stats) == 0) {
    invisible()
  } else {
    # Locate plot
    plot_loc <- get_plot(ma_abrev = ma_abrev, parameter = param, type = "Discrete", pid = "none")
    
    # Arrange and display plot and statistic table
    cat("  \n")
    
    # fig_caption <- paste0("Seasonal Kendall-Tau Results for ", param, " - Discrete")
    fig_caption <- FigureCaptions[ParameterName==param & SamplingFrequency=="Discrete", FigureCaptions]
    subchunkify(cat("![", fig_caption, "](", plot_loc,")"))
    
    # cat("![](", plot_loc,")")
    cat("  \n")
    
    # Grab relevant table description for a given plot
    desc <- TableDescriptions[ManagedAreaName==ma & ParameterName==param & SamplingFrequency=="Discrete", get(descriptionColumn)]
    # Table title
    table_title <- paste0("Seasonal Kendall-Tau Trend Analysis for ", param)
    
    # Creates ResultTable to display statistics below plot
    ResultTable <- skt_stats %>%
      select(ActivityType, `Statistical Trend`, N_Data, N_Years, 
             `Period of Record`, Median, tau, SennIntercept, SennSlope, p) %>%
      rename("Activity Type" = ActivityType, "Sample Count" = N_Data, 
             "Years with Data" = N_Years, "Sen Intercept" = SennIntercept, 
             "Sen Slope" = SennSlope)
    # Prep for latex-format
    names(ResultTable) <- gsub("_", "-", names(ResultTable))
    result_table <- kable(ResultTable, format = format_type,
                          caption = table_title,
                          row.names = FALSE, digits = 4,
                          booktabs = T, linesep = "", escape = F, longtable = F) %>%
      kableExtra::kable_styling(latex_options = c("scale_down", "HOLD_position"))
    cat("  \n")
    print(result_table)
    cat("  \n")
    cat(desc)
    cat("  \n")
  }
}

disc_program_tables <- function(ma, data){
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
  program_ids <- sort(unique(program_table$ProgramID))
  
  cat("\n **Program names:** \n \n")
  
  # Display ProgramName below data table
  for (p_id in program_ids) {
    p_name <- program_table %>% filter(ProgramID == p_id) %>% pull(ProgramName)
    cat(paste0("*",p_id,"*", " - ",p_name, knitcitations::citep(bib[[paste0("SEACARID", p_id)]]), "  \n"))
  }
  cat("  \n")
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