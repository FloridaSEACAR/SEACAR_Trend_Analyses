# # loading SAV4 from SAV4.txt output or below from script (SAV4-Creation) source
# data_dir <- here::here("output")
# file_in <- list.files(data_dir, pattern="SAV_DataUsed.txt", full=TRUE)
# SAV4 <- fread(file_in, sep = "|", header = TRUE, stringsAsFactors = FALSE,
#               na.strings=c("NULL","","NA"))

# importing SAV4-Creation directly by running script locally
source("SAV4-Creation.R", echo=TRUE)

# check for folder paths, creating them if they don't yet exist, clearing out previous models
folder_paths <- c("output/models", "output/tables", "output/Figures/BB", "output/website/images/barplots", "output/website/images/trendplots")
for (path in folder_paths) {
  if (file.exists(path)) {
    files <- list.files(path, full.names=TRUE)
    if (length(files) > 0) {
      file.remove(files)
      print(paste("Contents removed from folder:", path))
    } else {
      print(paste("Folder is empty", path))
    }
  } else {
    dir.create(path, recursive = TRUE)  # Create folders recursively
    print(paste("Folder created:", path))
  }
}

# declaring addfits function which plots Percent Cover models on a single plot
addfits <- function(models, plot_i, param) {
  # aucol determines whether analysisunit or analysisunit_halid is used
  aucol <- as.name(names(plot_i$data)[1])
  # empty data frame to fill with regression data
  regression_data <- data.frame()
  plot_data <- data.frame()
  
  for (i in seq_along(models)) {
    # finding model name, calling previously created model variable
    model_name <- models[[i]]
    model <- get(model_name)
    
    # selecting for Total SAV and Total Seagrass to apply aesthetic conditions later
    is_ToSa <- grepl("ToSa", model_name)
    is_ToSe <- grepl("ToSe", model_name)
    exclude <- c("DrAl")
    
    # declaring species & managed area of each model
    species <- unique(model$data[[aucol]])
    managed_area <- unique(model$data$ManagedAreaName)
    
    #extract p-value
    p_val <- summary(model)$tTab[2,5]
    
    # exclude Drift algae from model plots
    if(!grepl(paste(exclude, collapse='|'), model_name)) {
      
      linetypes <- "solid"
      size <- 1
      alpha <- 1
      #alpha <- if (p_val <= 0.05) 1 else 0.8
      
      # filter dataframe for managed_area & species
      species_data <- SAV4 %>%
        filter(ManagedAreaName == managed_area,
               !is.na({{p}}),
               {{ aucol }} == species)
      
      # plot_dat <- plotdat %>%
      #   filter({{ aucol }} == species)
      
      # create predicted values variable for each model
      predicted_values <- predict(model, level = 0, newdata = species_data)
      
      # separate significant values
      significant <- if (p_val <=0.05) TRUE else FALSE
      
      # Add predicted values to the regression_data dataframe, with species & relyear
      regression_data <- rbind(regression_data, data.frame(
        relyear = species_data$relyear,
        fit = predicted_values,
        species = unique(species_data[[aucol]]),
        significance = significant))
      
      # in case we separate Total SAV and Total seagrass and treat them differently
      #if (is_ToSa || is_ToSe) {} else {}
      # regression_data <- regression_data %>%
      #   filter(!species %in% c("Total SAV", "Total seagrass"))
      
      # Plot all other species
      plot_i <- plot_i +
        geom_line(data = regression_data,
                  aes(x = relyear, y = fit, color=species, linetype=factor(significance)),
                  size=size, alpha=alpha, inherit.aes = FALSE) +
        # geom_bar(data = plot_dat, aes(x=relyear, y=npt), stat = "identity") +
        scale_linetype_manual(name=NULL,
                              values=c("TRUE" = "solid", "FALSE" = "dotdash"),
                              labels=c("TRUE" = "Significant", "FALSE" = "Not significant")) +
        # setting order of the legends, color first
        guides(color = guide_legend(order=1),
               linetype = guide_legend(order=2))
    }
  }
  
  # creating color scale so names line up correctly in legend
  species_list <- c("")
  
  for (l in plot_i[["layers"]]) {
    new_species <- unique(l$data$species[!l$data$species %in% species_list])
    if (length(new_species) > 0) {
      species_list <- append(species_list, new_species)
    }
  }
  
  # ordering species list to match spcols, with Total SAV & Total seagrass at bottom, otherwise alphabetical (Hal spp. at top)
  species_list <- species_list[order(match(species_list, names(spcols)))]
  
  # determining if scientific or common names
  species_labels <- modify_species_labels(species_list)
  
  plot_i <- plot_i + scale_color_manual(values = subset(spcols, names(spcols) %in% species_list),
                                        breaks = species_list,
                                        labels = species_labels)
  
  return(plot_i)
}

# function to modify species labels prior to plotting (sci vs common names)
modify_species_labels <- function(species_list) {
  if(usenames == "common") {
    lab <- str_replace(species_list, "Unidentified Halophila", "Halophila, unk.")
  } else {
    lab <- sapply(species_list, function(name) {
      match_idx <- match(name, spp_common)
      if (!is.na(match_idx)) {
        return(spp[match_idx])
      }
      return(name)
    })
    lab <- str_replace(lab, "Unidentified Halophila", "Halophila, unk.")
  }
  return(lab)
}

# Which analyses to run? c("BB_all," "BB_pct", "PC", "PO", and/or "PA") or c("none") for just EDA plotting
# Analyses <- c("BB_pct", "PC", "PA")

# Which plot output? "trendplot", "barplot" or both
plot_type <- c("trendplot","barplot")
# plot_type <- "barplot"

if ("trendplot" %in% plot_type && "barplot" %in% plot_type) {
  Analyses <- c("BB_pct", "PC", "PA")
} else if ("trendplot" %in% plot_type) {
  Analyses <- c("BB_pct", "PC")
} else if ("barplot" %in% plot_type) {
  Analyses <- c("PA")
}

#Empty data.table to house names of any failed models generated below.
failedmods <- data.table(model = character(),
                         error = character())

#Create a table of the proportion of present SAV types by managed area and year
props_halid <- SAV4 %>% filter(str_detect(analysisunit_halid, "Total|Drift|spp\\.", negate = TRUE), !is.na(PA)) %>% group_by(ManagedAreaName, analysisunit_halid, relyear) %>% summarize(n_P = sum(PA), ntot_PA = n(), prop_P = n_P/ntot_PA)
props <- SAV4 %>% filter(str_detect(analysisunit, "Total|Drift|decipiens|engelmannii|johnsonii|Unidentified", negate = TRUE), !is.na(PA)) %>% group_by(ManagedAreaName, analysisunit, relyear) %>% summarize(n_P = sum(PA), ntot_PA = n(), prop_P = n_P/ntot_PA)
setDT(props_halid)
setDT(props)
for(m in unique(props_halid$ManagedAreaName)){
  props_halid[ManagedAreaName == m, `:=` (n_allsp_P = sum(n_P), sp_prop = n_P/sum(n_P), sp_pct = (n_P/sum(n_P)) * 100), by = c("relyear")]
}
for(m in unique(props$ManagedAreaName)){
  # props_halcom[ManagedAreaName == m, `:=` (n_allsp_P_halcom = sum(n_P_halcom), sp_prop_halcom = n_P_halcom/sum(n_P_halcom), sp_pct_halcom = (n_P_halcom/sum(n_P_halcom)) * 100), by = c("relyear")]
  props[ManagedAreaName == m, `:=` (n_allsp_P = sum(n_P), sp_prop = n_P/sum(n_P), sp_pct = (n_P/sum(n_P)) * 100), by = c("relyear")]
}
setnames(props_halid, "analysisunit_halid", "analysisunit")
props2 <- distinct(rbind(props_halid, props))
setorder(props2, ManagedAreaName, relyear, analysisunit)
props <- props2

spcollist <- c("#005396","#005396",
                        "#0088B1",
                        "#00ADAE",
                        "#65CCB3",
                        "#AEE4C1",
                        "#FDEBA8",
                        "#F8CD6D",
                        "#F5A800",
                        "#F17B00",
                        "#900667",
                        "#000099")

spp <- c("Halophila spp.","Unidentified Halophila","Halophila johnsonii","Syringodium filiforme","Halophila decipiens","Halodule wrightii",
         "Halophila engelmannii","Thalassia testudinum","Ruppia maritima","Attached algae", "Total SAV", "Total seagrass")

spp_common <- c("Halophila spp.", "Unidentified Halophila", "Johnson's seagrass", "Manatee grass", "Paddle grass", 
                "Shoal grass", "Star grass", "Turtle grass", "Widgeon grass", "Attached algae", "Total SAV", "Total seagrass")

usenames <- "common" #alternative is "scientific"

spcols <- setNames(spcollist, spp_common)

SAV4[, `:=` (analysisunit_halid = fcase(analysisunit_halid == "Thalassia testudinum", "Turtle grass",
                                        analysisunit_halid == "Syringodium filiforme", "Manatee grass",
                                        analysisunit_halid == "Halodule wrightii", "Shoal grass",
                                        analysisunit_halid == "Ruppia maritima", "Widgeon grass",
                                        analysisunit_halid == "Halophila decipiens", "Paddle grass",
                                        analysisunit_halid == "Halophila engelmannii", "Star grass",
                                        analysisunit_halid == "Halophila johnsonii", "Johnson's seagrass",
                                        analysisunit_halid == "Unidentified Halophila", "Unidentified Halophila",
                                        analysisunit_halid == "Halophila spp.", "Halophila spp.",
                                        analysisunit_halid == "Total seagrass", "Total seagrass",
                                        analysisunit_halid == "Attached algae", "Attached algae",
                                        analysisunit_halid == "Drift algae", "Drift algae",
                                        analysisunit_halid == "Total SAV", "Total SAV"),
             analysisunit = fcase(analysisunit == "Thalassia testudinum", "Turtle grass",
                                  analysisunit == "Syringodium filiforme", "Manatee grass",
                                  analysisunit == "Halodule wrightii", "Shoal grass",
                                  analysisunit == "Ruppia maritima", "Widgeon grass",
                                  analysisunit == "Halophila decipiens", "Paddle grass",
                                  analysisunit == "Halophila engelmannii", "Star grass",
                                  analysisunit == "Halophila johnsonii", "Johnson's seagrass",
                                  analysisunit == "Unidentified Halophila", "Unidentified Halophila",
                                  analysisunit == "Halophila spp.", "Halophila spp.",
                                  analysisunit == "Total seagrass", "Total seagrass",
                                  analysisunit == "Attached algae", "Attached algae",
                                  analysisunit == "Drift algae", "Drift algae",
                                  analysisunit == "Total SAV", "Total SAV"))]

props[, analysisunit := fcase(analysisunit == "Thalassia testudinum", "Turtle grass",
                              analysisunit == "Syringodium filiforme", "Manatee grass",
                              analysisunit == "Halodule wrightii", "Shoal grass",
                              analysisunit == "Ruppia maritima", "Widgeon grass",
                              analysisunit == "Halophila decipiens", "Paddle grass",
                              analysisunit == "Halophila engelmannii", "Star grass",
                              analysisunit == "Halophila johnsonii", "Johnson's seagrass",
                              analysisunit == "Unidentified Halophila", "Unidentified Halophila",
                              analysisunit == "Halophila spp.", "Halophila spp.",
                              analysisunit == "Attached algae", "Attached algae")]

props <- props[, analysisunit := factor(analysisunit, levels = c("Unidentified Halophila",
                                                                 "Halophila spp.",
                                                                 "Johnson's seagrass",
                                                                 "Manatee grass",
                                                                 "Paddle grass",
                                                                 "Shoal grass",
                                                                 "Star grass",
                                                                 "Turtle grass",
                                                                 "Widgeon grass",
                                                                 "Attached algae"))]

# prcollist <- hcl.colors(n = length(unique(SAV4$ProgramID)), palette = "viridis")
prcollist_a <- sequential_hcl(length(unique(SAV4$ProgramName)), palette = "YlOrRd")
prcollist_b <- sequential_hcl(length(unique(SAV4$ProgramName)), palette = "YlGnBu", rev = TRUE)
prcollist <- append(prcollist_a[which(seq(1, length(prcollist_a)) %% 2 == 0)], 
                    prcollist_b[which(seq(1, length(prcollist_b)) %% 2 != 0)])
prcollist <- rev(prcollist)
set.seed(4691)
progs <- sample(sort(unique(SAV4$ProgramName)))
prcols <- setNames(prcollist, progs)

parameters <- data.table(column = c(as.name("BB_all"), as.name("BB_pct"), as.name("PC"), as.name("PO"), as.name("PA")),
                         name = c("Braun Blanquet score", "Median percent cover", "Visual percent cover", "Percent occurrence", "Frequency of occurrence"),
                         type = c("BBall", "BBpct", "PC", "PO", "PA"))

plot_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Arial"),
        # title = element_text(face="bold"),
        plot.title = element_text(hjust = 0.5, size = 12, color = "#314963"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "#314963"),
        legend.title = element_text(size = 10),
        legend.text.align = 0,
        axis.title.x = element_text(size = 10, margin = margin(t = 5, r = 0,
                                                               b = 10, l = 0)),
        axis.title.y = element_text(size = 10, margin = margin(t = 0, r = 10,
                                                               b = 0, l = 0)),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = -45, hjust = 0))

#Managed areas that should have Halophila species combined:
ma_halspp <- c("Banana River Aquatic Preserve", "Indian River-Malabar to Vero Beach Aquatic Preserve", 
               "Indian River-Vero Beach to Ft. Pierce Aquatic Preserve", "Jensen Beach to Jupiter Inlet Aquatic Preserve",
               "Loxahatchee River-Lake Worth Creek Aquatic Preserve", "Mosquito Lagoon Aquatic Preserve", 
               "Biscayne Bay Aquatic Preserve", "Florida Keys National Marine Sanctuary")

#save summary stats file
stats_pct <- SAV4[ManagedAreaName %in% ma_halspp, ] %>%
  group_by(ManagedAreaName, analysisunit) %>%
  summarize(ParameterName="Median percent cover (from BB scores)",
            N_Programs=length(unique(ProgramID)),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '),
            N_Data=length(BB_pct[!is.na(BB_pct)]),
            N_Years=length(unique(Year[!is.na(Year) & !is.na(BB_pct)])),
            EarliestYear=min(Year[!is.na(BB_pct)]),
            LatestYear=max(Year[!is.na(BB_pct)]),
            SufficientData=ifelse(N_Data>0 & N_Years>=5, TRUE, FALSE))
stats_pct2 <- SAV4[ManagedAreaName %in% setdiff(unique(SAV4$ManagedAreaName), ma_halspp), ] %>%
  group_by(ManagedAreaName, analysisunit_halid) %>%
  summarize(ParameterName="Median percent cover (from BB scores)",
            N_Programs=length(unique(ProgramID)),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '),
            N_Data=length(BB_pct[!is.na(BB_pct)]),
            N_Years=length(unique(Year[!is.na(Year) & !is.na(BB_pct)])),
            EarliestYear=min(Year[!is.na(BB_pct)]),
            LatestYear=max(Year[!is.na(BB_pct)]),
            SufficientData=ifelse(N_Data>0 & N_Years>=5, TRUE, FALSE))
setDT(stats_pct2)
setnames(stats_pct2, "analysisunit_halid", "analysisunit")
stats_pct <- distinct(rbind(stats_pct, stats_pct2))
setcolorder(stats_pct, c("ManagedAreaName", "analysisunit"))
setDT(stats_pct)
stats_pct[N_Years == 0, `:=` (EarliestYear = NA, LatestYear = NA)]
stats_pct$ProgramIDs
data.table::fwrite(stats_pct, "output/SAV_BBpct_Stats.txt", sep = "|")

stats_pa <- SAV4[ManagedAreaName %in% ma_halspp, ] %>%
  group_by(ManagedAreaName, analysisunit) %>%
  summarize(ParameterName="Frequency of occurrence",
            N_Programs=length(unique(ProgramID)),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '),
            N_Data=length(PA[!is.na(PA)]),
            N_Years=length(unique(Year[!is.na(Year) & !is.na(PA)])),
            EarliestYear=min(Year[!is.na(PA)]),
            LatestYear=max(Year[!is.na(PA)]),
            SufficientData=ifelse(N_Data>0 & N_Years>=5, TRUE, FALSE))
stats_pa2 <- SAV4[ManagedAreaName %in% setdiff(unique(SAV4$ManagedAreaName), ma_halspp), ] %>%
  group_by(ManagedAreaName, analysisunit_halid) %>%
  summarize(ParameterName="Frequency of occurrence",
            N_Programs=length(unique(ProgramID)),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '),
            N_Data=length(PA[!is.na(PA)]),
            N_Years=length(unique(Year[!is.na(Year) & !is.na(PA)])),
            EarliestYear=min(Year[!is.na(PA)]),
            LatestYear=max(Year[!is.na(PA)]),
            SufficientData=ifelse(N_Data>0 & N_Years>=5, TRUE, FALSE))
setDT(stats_pa2)
setnames(stats_pa2, "analysisunit_halid", "analysisunit")
stats_pa <- distinct(rbind(stats_pa, stats_pa2))
setcolorder(stats_pa, c("ManagedAreaName", "analysisunit"))
setDT(stats_pa)
stats_pa[N_Years == 0, `:=` (EarliestYear = NA, LatestYear = NA)]
# fwrite(stats_pa, here::here("output/data/SAV_PA_Stats.txt"), sep = "|", sep2 = ",")

# stats2 <- rbind(stats_pct, stats_pa)
# fwrite(stats2, here::here(paste0("output/data/SAV_BBpct_PA_Stats", Sys.Date(), ".txt")), sep = "|")
statpardat <- list("BB_pct" = stats_pct, "PA" = stats_pa)
openxlsx::write.xlsx(statpardat, here::here("output/SAV_BBpct_PA_Stats.xlsx"), colNames = c(TRUE, TRUE), colWidths = c("auto", "auto"), firstRow = c(TRUE, TRUE))
openxlsx::write.xlsx(statpardat, here::here(paste0("output/SAV_BBpct_PA_Stats_", Sys.Date(), ".xlsx")), colNames = c(TRUE, TRUE), colWidths = c("auto", "auto"), firstRow = c(TRUE, TRUE))

# #subset to run only part of the script------------------------------------------------------
# parameters <- parameters[column == "PA", ]

#Save session info-----------------------------------------------------
session <- sessionInfo()
saveRDS(session, here::here(paste0("output/SessionInfo_", Sys.Date())))

#start script----------------------------------------------------------------------
tic()
n <- 0
seed <- 352
set.seed(seed)

#############################
### MODEL & PLOT CREATION ###
#############################
for(p in parameters$column){
  
  cat(paste0("\nStarting indicator: ", p, "\n"))
  
  #List managed areas with at least 5 years of data
  nyears <- SAV4[!is.na(eval(p)) & !is.na(analysisunit), ] %>% group_by(ManagedAreaName, analysisunit) %>% summarize(type = paste0(p), nyr = length(unique(Year)))
  nyears2 <- SAV4[!is.na(eval(p)) & !is.na(analysisunit_halid), ] %>% group_by(ManagedAreaName, analysisunit_halid) %>% summarize(type = paste0(p), nyr = length(unique(Year)))
  setDT(nyears2)
  setnames(nyears2, "analysisunit_halid", "analysisunit")
  nyears <- distinct(rbind(nyears, nyears2))
  ma_include <- unique(subset(nyears, nyears$nyr >= 5)$ManagedAreaName)
  
  # Subset ma_include to first 5 entries
  # ma_include <- c("Pinellas County")
  # ma_include <- ma_include[c(1,2,3,4,5)]
  
  #For each managed area, make sure there are multiple levels of BB scores per species; remove ones that don't from further consideration.
  for(i in ma_include){
    
    cat(paste0("\nStarting MA: ", i, "\n"))
    
    if("none" %in% Analyses){
      if(p == parameters$column[length(parameters$column)] & i == ma_include[length(ma_include)]){
        toc()
      }
      next
    } 
    
    if(i %in% ma_halspp){
      species <- subset(nyears, nyears$ManagedAreaName == i & nyears$nyr >= 5 & analysisunit %in% c("Attached algae", "Drift algae", "Halophila spp.", "Manatee grass", 
                                                                                                    "Shoal grass", "Total seagrass", "Total SAV", "Turtle grass", 
                                                                                                    "Widgeon grass", "Syringodium filiforme", "Halodule wrightii", "Thalassia testudinum",
                                                                                                    "Ruppia maritima"))$analysisunit
    } else{
      species <- subset(nyears, nyears$ManagedAreaName == i & nyears$nyr >= 5 & analysisunit %in% c("Attached algae", "Drift algae", "Unidentified Halophila", 
                                                                                                    "Johnson's seagrass", "Manatee grass", "Paddle grass", 
                                                                                                    "Shoal grass", "Star grass", "Total seagrass", "Total SAV", 
                                                                                                    "Turtle grass", "Widgeon grass", "Syringodium filiforme", 
                                                                                                    "Halodule wrightii", "Thalassia testudinum","Ruppia maritima"))$analysisunit
    }
    
    models <- c()
    
    #Create data.tables to hold model results for managed area i----------------------------------------------------
    lmemodresults <- data.table(managed_area = character(),
                                species = character(),
                                filename = character(),
                                effect = character(),
                                group = character(),
                                term = character(),
                                estimate = numeric(),
                                std.error = numeric(),
                                df = numeric(),
                                statistic = numeric(),
                                p.value = numeric())
    
    
    #In case model doesn't converge on the first try, attempt each model up to 5 times before moving on
    for(j in species){
      
      cat(paste0("\n  Starting species: ", j, "\n"))
      
      if(paste0(p) %in% c("BB_pct", "PC") & ("BB_pct" %in% Analyses | "PC" %in% Analyses)){
        
        formula_j <- as.formula(paste0(p, " ~ relyear"))
        
        set.seed(seed + n)
        if(j %in% setdiff(unique(SAV4$analysisunit_halid), unique(SAV4$analysisunit))){
          model_j <- try(lme(formula_j,
                             random = list(SiteIdentifier = ~relyear),
                             control = list(msMaxIter = 1000, msMaxEval = 1000, sing.tol=1e-20),
                             na.action = na.omit, 
                             data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit_halid == j, ]), 
                         silent = TRUE)
          n <- n + 1
          x <- 0
          
          while(class(model_j) == "try-error" & x < 5){
            if(x %% 25 == 0) print(paste0("    Model failed, starting attempt ", x, " of 5"))
            
            set.seed(seed + n)
            model_j <- try(lme(formula_j,
                               random = list(SiteIdentifier = ~relyear),
                               control = list(msMaxIter = 1000, msMaxEval = 1000, sing.tol=1e-20),
                               na.action = na.omit, 
                               data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit_halid == j, ]),
                           silent = TRUE)
            n <- n + 1
            x <- x + 1
          }
        } else{
          model_j <- try(lme(formula_j,
                             random = list(SiteIdentifier = ~relyear),
                             control = list(msMaxIter = 1000, msMaxEval = 1000, sing.tol=1e-20),
                             na.action = na.omit, 
                             data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == j, ]), 
                         silent = TRUE)
          n <- n + 1
          x <- 0
          
          while(class(model_j) == "try-error" & x < 5){
            if(x %% 25 == 0) print(paste0("    Model failed, starting attempt ", x, " of 5"))
            
            set.seed(seed + n)
            model_j <- try(lme(formula_j,
                               random = list(SiteIdentifier = ~relyear),
                               control = list(msMaxIter = 1000, msMaxEval = 1000, sing.tol=1e-20),
                               na.action = na.omit, 
                               data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == j, ]),
                           silent = TRUE)
            n <- n + 1
            x <- x + 1
          }
        }
        
        
        #Individual model objects are needed for plotting all species together
        ##This allows get(model) functionality within addfits function
        eval(call("<-", as.name(paste0(gsub('\\b(\\pL)\\pL{2,}|.', '\\U\\1', i, perl = TRUE), 
                                       "_", 
                                       gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE))), 
                  model_j))
        
        #Save the model object as .rds
        saveRDS(model_j, here::here(paste0("output/models/SAV_", parameters[column == p, type], "_", 
                                           gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "_",
                                           gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE), 
                                           ".rds")))
        
        print(paste0("  Model object saved: ", 
                     gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                     "_", 
                     gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE)))
        
        #record lme model results------------------------------------------------------
        if(class(try(eval(as.name(paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "_", gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE)))), silent = TRUE)) != "try-error"){
          models <- append(models, as.name(paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "_", gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE))))
          modj_i <- setDT(broom.mixed::tidy(eval(as.name(paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "_", gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE))))))
          modj_i[, `:=` (managed_area = i,
                         species = j,
                         filename = paste0("SAV_", parameters[column == p, type], "_", 
                                           gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "_",
                                           gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE), ".rds"))]
          lmemodresults <- rbind(lmemodresults, modj_i)
          
        } else{
          failedmod <- data.table(model = paste0("SAV_", parameters[column == p, type], "_",
                                                 gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "_",
                                                 gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE), ".rds"),
                                  error = model_j[1])
          
          failedmods <- rbind(failedmods, failedmod)
          
          modj_i <- data.table(managed_area = i,
                               species = j,
                               filename = paste0("SAV_", parameters[column == p, type], "_", 
                                                 gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "_",
                                                 gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE), ".rds"),
                               effect = NA,
                               group = NA,
                               term = NA,
                               estimate = NA,
                               std.error = NA,
                               df = NA,
                               statistic = NA,
                               p.value = NA)
          lmemodresults <- rbind(lmemodresults, modj_i)
        }
      }
    }
    
    ###### TREND PLOTS #####
    if(paste0(p) %in% c("BB_pct", "PC") & ("BB_pct" %in% Analyses | "PC" %in% Analyses)){
      #Summarize # points per category
      
      if(i %in% ma_halspp){
        plotdat <- SAV4[ManagedAreaName == i & !is.na(eval(p)), ] %>% group_by(analysisunit, Year, relyear, eval(p)) %>% summarise(npt = n())
      } else{
        plotdat <- SAV4[ManagedAreaName == i & !is.na(eval(p)), ] %>% group_by(analysisunit_halid, Year, relyear, eval(p)) %>% summarise(npt = n())
      }
      setDT(plotdat)
      setnames(plotdat, "eval(p)", "data")
      aucol <- names(plotdat[,1])

      #split modeled vs unmodeled data
      modeledsp <- c()
      for(u in seq_along(models)){
        name_u <- fcase(str_detect(paste0(models[[u]]), "_ShGr"), "Shoal grass",
                        str_detect(paste0(models[[u]]), "_TuGr"), "Turtle grass",
                        str_detect(paste0(models[[u]]), "_MaGr"), "Manatee grass",
                        str_detect(paste0(models[[u]]), "_WiGr"), "Widgeon grass",
                        str_detect(paste0(models[[u]]), "_PaGr"), "Paddle grass",
                        str_detect(paste0(models[[u]]), "_StGr"), "Star grass",
                        str_detect(paste0(models[[u]]), "_JoSe"), "Johnson's seagrass",
                        str_detect(paste0(models[[u]]), "_UnHa"), "Unidentified Halophila",
                        str_detect(paste0(models[[u]]), "_HaSp"), "Halophila spp.",
                        str_detect(paste0(models[[u]]), "_ToSe"), "Total seagrass",
                        str_detect(paste0(models[[u]]), "_AtAl"), "Attached algae",
                        str_detect(paste0(models[[u]]), "_DrAl"), "Drift algae",
                        str_detect(paste0(models[[u]]), "_To"), "Total SAV")
        modeledsp <- append(modeledsp, name_u)
      }
      
      miny <- c()
      for(v in seq_along(models)){
        miny_v <- try(predict(eval(models[[v]]), level = 0), silent = TRUE)
        if(class(miny_v) == "try-error") next
        miny <- append(miny, min(miny_v))
      }
      miny <- ifelse(floor(min(miny)) < 0, floor(min(miny)), 0)
      
      # Scale x-axis data
      breaks_seq <- seq(from = min(plotdat$relyear),
                        to = max(plotdat$relyear),
                        by = 3)
      labels_seq <- seq(from = min(plotdat$Year),
                        to = max(plotdat$Year),
                        by = 3)
      
      #create base plot of seagrass percent cover data over time for managed area i
      plot_i <- ggplot(data = droplevels(plotdat),
                       aes(x = relyear, y = data)) +
        # geom_jitter(shape = 21, alpha=0.9, color="grey50") +
        # geom_bar(stat = "identity") +
        labs(title = parameters[column == p, name], subtitle = i,
             x = "Year",
             y = parameters[column == p, name],
             color = "Species model projections",
             linetype = "Significance") +
        plot_theme +
        ylim(miny, 100) +
        scale_x_continuous(breaks = breaks_seq, labels = labels_seq) +
        scale_colour_manual(values = spcols)
      
      if(length(models) > 0){
        #make sure that no failed models slipped through
        classes <- lapply(models, function(x) class(eval(x)))
        models <- models[classes != "try-error"]
        
        plot_i <- addfits(models, plot_i, p)
      }

      #Save the plot object as .rds
      saveRDS(plot_i, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_",
                                               paste0(str_sub(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 1, -1), "_trendplot.rds"))))
      
      #Save the results table objects as .rds
      saveRDS(lmemodresults, here::here(paste0("output/tables/SAV_", parameters[column == p, type], "_", 
                                               paste0(str_sub(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 1, -1), "_lmeresults.rds"))))
    }
    
    ###### BAR PLOTS ######
    if(paste0(p) == "PA" & "PA" %in% Analyses){
      #Bar chart of proportions by analysisunit
      breaks <- c(seq(min(SAV4[ManagedAreaName == i & !is.na(PA), relyear]),
                      max(SAV4[ManagedAreaName == i & !is.na(PA), relyear]),
                      by = 2))
      yrlist <- sort(unique(SAV4$Year))

      labels <- c()
      for(b in breaks){
        labels <- append(labels, yrlist[b + 1])
      }

      if(i %in% ma_halspp){
        
        bpdat <- props[ManagedAreaName == i & !is.na(analysisunit) & str_detect(analysisunit, "decipiens|engelmannii|johnsonii|Unidentified|Star|Paddle|Johnson", negate = TRUE), ]
        
        sp_list <- unique(bpdat$analysisunit)
        sp_list <- sp_list[order(match(sp_list, names(spcols)))]
        
        # add color scale, determining if scientific or common names
        sp_labels <- modify_species_labels(sp_list)
        
        barplot_sp <- ggplot(data = bpdat, aes(x = relyear, y = sp_pct, fill = analysisunit)) +
          geom_col(color = "grey20") +
          scale_x_continuous(breaks = breaks, labels = labels) +
          plot_theme +
          labs(title = parameters[column == p, name], subtitle = i,
               fill = "Species",
               x = "Year",
               y = "Occurrence frequency (%)") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_color_manual(values = subset(spcols, names(spcols) %in% sp_list),
                             labels = sp_labels,
                             aesthetics = c("color", "fill"))
      } else{
        
        bpdat <- props[ManagedAreaName == i & !is.na(analysisunit) & analysisunit != "Halophila spp.", ]
        
        sp_list <- unique(bpdat$analysisunit)
        sp_list <- sp_list[order(match(sp_list, names(spcols)))]
        
        # add color scale, determining if scientific or common names
        sp_labels <- modify_species_labels(sp_list)
        
        barplot_sp <- ggplot(data = bpdat, aes(x = relyear, y = sp_pct, fill = analysisunit)) +
          geom_col(color = "grey20") +
          scale_x_continuous(breaks = breaks, labels = labels) +
          plot_theme +
          labs(title = parameters[column == p, name], subtitle = i,
               fill = "Species",
               x = "Year",
               y = "Occurrence frequency (%)") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_color_manual(values = subset(spcols, names(spcols) %in% sp_list),
                             labels = sp_labels,
                             aesthetics = c("color", "fill"))
        }

      saveRDS(barplot_sp, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_",
                                            gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "_barplot_sp.rds")))
    }

    print(paste0("  Plot objects and results tables saved: ",
                 gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE),
                 "_",
                 gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE)))
  }
}


#Save failedmodslist-----------------------------------------------------
saveRDS(failedmods, here::here("output/models/failedmodslist.rds"))

#Get rid of eval(p)'s from plot file mappings---------------------------------------
files <- list.files(here::here("output/Figures/BB/")) #get file list
files <- str_subset(files, ".rds") #exclude non-.RDS files

filesupdated <- list()
for(f in seq_along(files)){
  file_f <- readRDS(here::here(paste0("output/Figures/BB/", files[f])))
  if(paste0(as_label(file_f$mapping$y)) == "eval(p)"){
    file_f$mapping$y <- parameters[name %in% file_f$labels$y, column][[1]]
    saveRDS(file_f, here::here(paste0("output/Figures/BB/", files[f])))
    rm(file_f)
    filesupdated <- append(filesupdated, files[f])
  } else {
    rm(file_f)
  }
  if(round((f/length(files))*100, 1) %% 10 == 0){
    print(paste0(round((f/length(files))*100), "% done!"))
  }
}

### Saving Trendplot and Barplot files
if ("trendplot" %in% plot_type) {
  files <- list.files(here::here("output/Figures/BB/")) #get file list
  plots <- stringr::str_subset(files, "_trendplot") #identify map file
  mods <- list.files(here::here("output/models/"))
  models2 <- str_subset(mods, paste0(str_sub(plots[1], 1, str_locate_all(plots[1], "_")[[1]][2])))
  # models2 <- str_subset(mods, "_lme")
  
  malist <- c()
  for(pl in plots){
    ma_p <- str_sub(pl, str_locate_all(pl, "_")[[1]][2], str_locate_all(pl, "_")[[1]][3])
    malist <- append(malist, ma_p)
  }
  
  failedmodslist <- readRDS(here::here("output/models/failedmodslist.rds"))
  
  for(m in malist){
    mods_m <- str_subset(models2, m)
    mods_m <- setdiff(mods_m, failedmodslist$model)
    mods_m <- setdiff(mods_m, subset(mods_m, str_detect(mods_m, "_PC_")))
    # params <- unique(str_split(mods_m, "_")[[1]][2])
    params <- "BBpct"
    
    for(param in params){
      mods_m_p <- str_subset(mods_m, param)
      plot_m <- try(str_subset(plots, unique(str_sub(mods_m_p, 1, str_locate_all(mods_m_p, "_")[[1]][3] - 1))), silent = TRUE)
      
      if(class(plot_m) != "try-error"){
        for(w in mods_m_p){
          aucol <- ifelse(str_sub(w, str_locate_all(w, "_")[[1]][2],
                                  ifelse(str_detect(w, "AP_"), str_locate(w, "AP_")[2],
                                         ifelse(str_detect(w, "NERR_"), 
                                                str_locate(w, "NERR_")[2], 
                                                str_locate(w, "NMS_")[2]))) %in% c("_BRAP_", "_IRMVBAP_", "_IRVBPAP_", "_JBJIAP_", "_LRLWCAP_", 
                                                                                   "_MLAP_", "_BBAP_", "_FKNMS_"), as.name("analysisunit"), as.name("analysisunit_halid"))
          
          eval(call("<-", as.name(paste0(str_sub(w, str_locate_all(w, "_")[[1]][2] + 1, 
                                                 ifelse(str_detect(w, "AP_"), str_locate(w, "AP_") - 1, 
                                                        ifelse(str_detect(w, "NERR_"), str_locate(w, "NERR_"), str_locate(w, "NMS_")))), 
                                         "_", 
                                         str_sub(w, -8, -5))), 
                    readRDS(here::here(paste0("output/models/", w)))))
        }
        
        plot_m2 <- readRDS(here::here(paste0("output/Figures/BB/", plot_m)))
        plot_m2 <- plot_m2 +
          plot_theme
        au <- names(plot_m2$data[,1])
        wid <- ifelse(length(unique(plot_m2$data$au)) == 1, 4,
                      ifelse(length(unique(plot_m2$data$au)) == 2, 6, 8))
        hei <- ifelse(length(unique(plot_m2$data$au)) <= 3, 3,
                      ifelse(length(unique(plot_m2$data$au)) <= 6, 6, 9))
        
        png(here::here(paste0("output/website/images/trendplots/", str_sub(plot_m, 1, -5), ".png")),
            width = 8,
            height = 6,
            units = "in",
            res = 300)
        # jpeg(here::here(paste0("output/Figures/BB/img/", str_sub(plot_m, 1, -5), ".jpg")),
        #      width = 10,
        #      height = 6,
        #      units = "in",
        #      res = 300)
        print(plot_m2)
        print(paste0("Plot saved: ", plot_m))
        dev.off()
        
      } else{
        next
      }
    }
  }
} 
if ("barplot" %in% plot_type) {
  #Save .png versions of "barplot" .rds files --------------------------------------------------
  files <- list.files(here::here("output/Figures/BB/")) #get file list
  plots2 <- stringr::str_subset(files, "_barplot") #identify map file
  
  for(pl2 in plots2){
    plot_pl2 <- readRDS(here::here(paste0("output/Figures/BB/", pl2)))
    plot_pl2 <- plot_pl2 +
      plot_theme
    
    png(here::here(paste0("output/website/images/barplots/", str_sub(pl2, 1, -5), ".png")),
        width = 8,
        height = 4,
        units = "in",
        res = 200)
    
    print(plot_pl2)
    print(paste0("Plot saved: ", pl2))
    dev.off()
  }
}

toc()
