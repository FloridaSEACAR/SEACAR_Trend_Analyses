# SAV Filtering and subsetting
# Uses SAV.R from /MA Report Generation/ as a guide
library(scales)
library(colorspace)
library(brms)
library(broom.mixed)
library(tidybayes)
library(bayesplot)
library(sf)
library(gtable)
library(grid)
library(gridExtra)
library(nlme)
library(patchwork)
library(extrafont)
library(magick)
library(readr)
library(purrr)
library(stringr)
library(utils)
library(ggpubr)
library(mgcv)
library(tidymv)
library(tidygam)

SAV <- sav_data[!is.na(ResultValue), ]

# Create data columns based on old parameter results to make script run
SAV$BB <- NA
SAV$mBB <- NA
SAV$PC <- NA
SAV$PO <- NA
SAV$PA <- NA

# Fill created columns with values based on parameter names
SAV$BB[SAV$ParameterName=="Braun Blanquet Score"] <-
  SAV$ResultValue[SAV$ParameterName=="Braun Blanquet Score"]

SAV$mBB[SAV$ParameterName=="Modified Braun Blanquet Score"] <-
  SAV$ResultValue[SAV$ParameterName=="Modified Braun Blanquet Score"]

SAV$PC[SAV$ParameterName=="Percent Cover"] <-
  SAV$ResultValue[SAV$ParameterName=="Percent Cover"]

SAV$PO[SAV$ParameterName=="Percent Occurrence"] <-
  SAV$ResultValue[SAV$ParameterName=="Percent Occurrence"]

SAV$PA[SAV$ParameterName=="Presence/Absence"] <-
  SAV$ResultValue[SAV$ParameterName=="Presence/Absence"]

# Filtering and subsetting
SAV2 <- subset(SAV, !is.na(SAV$BB) | !is.na(SAV$mBB) | !is.na(SAV$PC) | !is.na(SAV$PO))
SAV2 <- SAV2 %>% filter(BB >= 0 & BB <= 5 | is.na(BB))
SAV2 <- SAV2 %>% filter(mBB >= 0 & mBB <= 5 | is.na(mBB))
SAV2 <- SAV2 %>% filter(PC >= 0 & PC <= 100 | is.na(PC))
SAV2 <- SAV2 %>% filter(PO >= 0 & PO <= 100 | is.na(PO))
SAV2 <- SAV2 %>% filter(Month %in% c(4:10))
setDT(SAV2)

SAV2[!is.na(BB), BB_all := fcase(BB == 0, 0, 
                                 BB > 0 & BB <= 1, 1,
                                 BB > 1, round(BB))]
SAV2[!is.na(mBB), BB_all := fcase(mBB == 0, 0,
                                  mBB > 0 & mBB <= 1, 1, 
                                  mBB > 1, round(mBB))]
SAV2[!is.na(PC), BB_all := fcase(PC == 0, 0,
                                 PC > 0 & PC <= (2.5 + (15-2.5)/2), 1,
                                 PC <= (2.5 + (15-2.5) + (37.5-15)/2), 2,
                                 PC <= (2.5 + (15-2.5) + (37.5-15) + (62.5 - 37.5)/2), 3,
                                 PC <= (2.5 + (15-2.5) + (37.5-15) + (62.5 - 37.5) + (87.5 - 62.5)/2), 4, 
                                 PC > (2.5 + (15-2.5) + (37.5-15) + (62.5 - 37.5) + (87.5 - 62.5)/2), 5)]


#Replaces two blocks of code above by using the BB_all variable to create all estimates at once.
SAV2[!is.na(BB_all), BB_pct := fcase(BB_all == 0, 0, 
                                     BB_all > 0 & BB_all <= 0.1, rescale(BB_all, from=c(0, 0.1), to=c(0,0.02)), #Added by SRD 8/31/2021
                                     BB_all > 0.1 & BB_all <= 0.5, rescale(BB_all, from=c(0.1, 0.5), to=c(0.02,0.1)),
                                     BB_all > 0.5 & BB_all <= 1, rescale(BB_all, from=c(0.5,1), to=c(0.1,2.5)),
                                     BB_all > 1 & BB_all <= 2, rescale(BB_all, from=c(1,2), to=c(2.5,15)),
                                     BB_all > 2 & BB_all <= 3, rescale(BB_all, from=c(2,3), to=c(15,37.5)),
                                     BB_all > 3 & BB_all <= 4, rescale(BB_all, from=c(3,4), to=c(37.5,62.5)),
                                     BB_all > 4 & BB_all <= 5, rescale(BB_all, from=c(4,5), to=c(62.5,87.5)))]

SAV2[, BB_pct := as.numeric(BB_pct)]
SAV2[, BB_all := as.ordered(BB_all)]
SAV2[!is.na(PO), method := "Percent Occurrence"]
SAV2[!is.na(BB), method := "Braun Blanquet"]
SAV2[!is.na(mBB), method := "Modified Braun Blanquet"]
SAV2[!is.na(PC), method := "Percent Cover"]

SAV2[!is.na(BB_all), PA := ifelse(BB_all == 0, 0, 1)]
SAV2[!is.na(PO), PA := ifelse(PO == 0, 0, 1)]

SAV2[, relyear := Year - min(Year)]

SAV3 <- SAV2[SpeciesGroup1 %in% c("Seagrass", "Macroalgae", "Total SAV"), ]

#Temporary fix to cases where analysisunit is NA but CommonIdentifier is "Drift Algae" (and Drift_Attached is also NA); ~6,000 records
SAV3[CommonIdentifier == "Drift algae", Drift_Attached := "Drift"]

species_reject <- c("NA","Vallisneria americana", "Najas guadalupensis",
                    "Hydrilla verticillata", "Potamogeton pusillus",
                    "Zannichellia palustris")

SAV3[, `:=` (analysisunit_halid = ifelse(CommonIdentifier %in% species_reject, NA, 
                                         ifelse(str_detect(CommonIdentifier, "Halophila") & is.na(SpeciesName), "Unidentified Halophila", 
                                                ifelse(SpeciesGroup1 %in% c("Seagrass","Total SAV"), CommonIdentifier, Drift_Attached))),
             analysisunit = ifelse(CommonIdentifier %in% species_reject, NA, 
                                   ifelse(str_detect(CommonIdentifier, "Halophila"), "Halophila spp.", 
                                          ifelse(SpeciesGroup1 %in% c("Seagrass","Total SAV"), CommonIdentifier, Drift_Attached))))]

SAV3[str_detect(analysisunit, "Drift|Attached"), `:=` (analysisunit = paste0(analysisunit, " algae"))]
SAV3[str_detect(analysisunit_halid, "Drift|Attached"), `:=` (analysisunit_halid = paste0(analysisunit_halid, " algae"))]

SAV4 <- subset(SAV3, !is.na(SAV3$analysisunit))
rm(SAV, SAV2, SAV3)

# modified version of previous addfits_blacktrendlines function to create multi-plots
addfits_multiplots <- function(models, plot_i, param, aucol){
  for(n in 1:length(models)){
    model_name <- names(models[n])
    model <- models[[model_name]]
    
    sp <- unique(model$data[[aucol]])
    
    species_data <- SAV4[System == i & !is.na(eval(p)) & eval(as.name(aucol)) == sp, ]
    species_data$predictions <- predict(model, level = 0)
    
    plot_i <- plot_i +
      geom_line(data = species_data,
                aes(x = relyear, y = predictions), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)
  }
  
  # order_match <- ifelse(usenames=="common", "order(match(spp_common))", "order(match(spp))")
  
  plot_i <- plot_i +
    facet_wrap(~factor(modify_species_labels(eval(aucol), usenames)), 
               ncol = 3, strip.position = "top")
  
  return(plot_i)
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
    model_name <- names(models[i])
    model <- models[[i]]
    
    # selecting for Total SAV and Total Seagrass to apply aesthetic conditions later
    is_ToSa <- grepl("ToSa", model_name)
    is_ToSe <- grepl("ToSe", model_name)
    exclude <- c("DrAl","AtAl")
    
    # declaring species & system of each model
    species <- unique(model$data[[aucol]])
    system_area <- unique(model$data$System)
    
    #extract p-value
    p_val <- summary(model)$tTab[2,5]
    
    # exclude Drift algae from model plots
    if(!grepl(paste(exclude, collapse='|'), model_name)) {
      
      linetypes <- "solid"
      size <- 1
      alpha <- 1
      #alpha <- if (p_val <= 0.05) 1 else 0.8
      
      # filter dataframe for system_area & species
      species_data <- SAV4 %>%
        filter(System == system_area,
               !is.na({{p}}),
               {{ aucol }} == species)
      
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
        scale_linetype_manual(name="Trend significance (alpha = 0.05)",
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
  species_labels <- modify_species_labels(species_list, usenames)
  
  plot_i <- plot_i + scale_color_manual(values = subset(spcols, names(spcols) %in% species_list),
                                        breaks = species_list,
                                        labels = species_labels)
  
  return(plot_i)
}

# function to modify species labels prior to plotting (sci vs common names)
# also replaces "Unidentified Halophila" with "Halophila, unk."
modify_species_labels <- function(species_list, usenames) {
  
  if(usenames == "scientific") {
    lab <- species_list
  } else {
    lab <- sapply(species_list, function(name) {
      match_idx <- match(name, spp)
      if (!is.na(match_idx)) {
        return(spp_common[match_idx])
      }
      return(name)
    })
  }
  lab <- str_replace_all(lab, "Unidentified Halophila", "Halophila, unk.")
  return(lab)
}

## Choose which analyses to run, default is both trend and barplots
Analyses <- c("BB_pct", "PC", "PA")

#Empty data.table to house names of any failed models generated below.
failedmods <- data.table(model = character(),
                         error = character())

###############################
### SAV Data Prep & Summary ###
###############################

#Create a table of the proportion of present SAV types by managed area and year
props_halid <- SAV4 %>% 
  filter(str_detect(analysisunit_halid, "Total|Drift|spp\\.", negate = TRUE), !is.na(PA)) %>% 
  group_by(System, analysisunit_halid, relyear) %>% 
  summarize(n_P = sum(PA), ntot_PA = n(), prop_P = n_P/ntot_PA)

props <- SAV4 %>% 
  filter(str_detect(analysisunit, "Total|Drift|decipiens|engelmannii|johnsonii|Unidentified", negate = TRUE), !is.na(PA)) %>% 
  group_by(System, analysisunit, relyear) %>% summarize(n_P = sum(PA), ntot_PA = n(), prop_P = n_P/ntot_PA)

setDT(props_halid)
setDT(props)

for(s in unique(props_halid$System)){
  props_halid[System == s, `:=` (n_allsp_P = sum(n_P), sp_prop = n_P/sum(n_P), sp_pct = (n_P/sum(n_P)) * 100), by = c("relyear")]
}
for(s in unique(props$System)){
  props[System == s, `:=` (n_allsp_P = sum(n_P), sp_prop = n_P/sum(n_P), sp_pct = (n_P/sum(n_P)) * 100), by = c("relyear")]
}

setnames(props_halid, "analysisunit_halid", "analysisunit")
props2 <- distinct(rbind(props_halid, props))
setorder(props2, System, relyear, analysisunit)
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

usenames <- "scientific" #alternative is "common"

spcols <- setNames(spcollist, spp)

props <- props[, analysisunit := factor(analysisunit, levels = c("Unidentified Halophila",
                                                                 "Halophila spp.",
                                                                 "Halophila johnsonii",
                                                                 "Syringodium filiforme",
                                                                 "Halophila decipiens",
                                                                 "Halodule wrightii",
                                                                 "Halophila engelmannii",
                                                                 "Thalassia testudinum",
                                                                 "Ruppia maritima",
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

parameters <- data.table(column = c(as.name("BB_pct"), as.name("PC"), as.name("PA")),
                         name = c("Median percent cover", "Visual percent cover", "Frequency of occurrence"),
                         type = c("BBpct", "PC", "PA"))

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

######################
#### START SCRIPT ####
######################

n <- 0
seed <- 352
set.seed(seed)

data_directory_sav <- list()

for(p in parameters$column){
  cat(paste0("\nStarting indicator: ", p, "\n"))
  
  #List managed areas with at least 5 years of data
  nyears <- SAV4[!is.na(eval(p)) & !is.na(analysisunit), ] %>% group_by(System, analysisunit) %>% summarize(type = paste0(p), nyr = length(unique(Year)))
  nyears2 <- SAV4[!is.na(eval(p)) & !is.na(analysisunit_halid), ] %>% group_by(System, analysisunit_halid) %>% summarize(type = paste0(p), nyr = length(unique(Year)))
  setDT(nyears2)
  setnames(nyears2, "analysisunit_halid", "analysisunit")
  nyears <- distinct(rbind(nyears, nyears2))
  sys_include <- unique(subset(nyears, nyears$nyr >= 5)$System)
  # ma_include <- c("Banana River Aquatic Preserve", "Biscayne Bay Aquatic Preserve")
  
  #For each managed area, make sure there are multiple levels of BB scores per species; remove ones that don't from further consideration.
  
  for(i in sys_include){
    
    ma_abrev <- gsub(" ", "_", i)
    
    cat(paste0("\nStarting system: ", i, "\n"))
    
    species <- subset(nyears, nyears$System == i & nyears$nyr >= 5 & analysisunit %in% c("Attached algae", "Drift algae", "Unidentified Halophila", 
                                                                                                  "Halophila johnsonii", "Syringodium filiforme", "Halophila decipiens", 
                                                                                                  "Halodule wrightii", "Halophila engelmannii", "Total seagrass", "Total SAV", 
                                                                                                  "Thalassia testudinum", "Ruppia maritima"))$analysisunit
    
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
                             data = SAV4[System == i & !is.na(eval(p)) & analysisunit_halid == j, ]), 
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
                               data = SAV4[System == i & !is.na(eval(p)) & analysisunit_halid == j, ]),
                           silent = TRUE)
            n <- n + 1
            x <- x + 1
          }
        } else{
          model_j <- try(lme(formula_j,
                             random = list(SiteIdentifier = ~relyear),
                             control = list(msMaxIter = 1000, msMaxEval = 1000, sing.tol=1e-20),
                             na.action = na.omit, 
                             data = SAV4[System == i & !is.na(eval(p)) & analysisunit == j, ]), 
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
                               data = SAV4[System == i & !is.na(eval(p)) & analysisunit == j, ]),
                           silent = TRUE)
            n <- n + 1
            x <- x + 1
          }
        }
        
        short_model_name <- gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE)
        
        #record lme model results------------------------------------------------------
        if(class(try(eval(model_j), silent = TRUE)) != "try-error"){
          # append only the successful models to data_directory_sav object
          data_directory_sav[["models"]][[ma_abrev]][[p]][[short_model_name]] <- model_j
          
          modj_i <- setDT(broom.mixed::tidy(eval(model_j)))
          modj_i[, `:=` (managed_area = i,
                         species = j,
                         filename = paste0("SAV_", parameters[column == p, type], "_", 
                                           ma_abrev, "_",
                                           short_model_name, ".rds"))]
          lmemodresults <- rbind(lmemodresults, modj_i)
          
        } else{
          # data_directory_sav[["models"]][[ma_abrev]][[p]][[short_model_name]] <- model_j
          
          failedmod <- data.table(model = paste0("SAV_", parameters[column == p, type], "_",
                                                 ma_abrev, "_",
                                                 short_model_name, ".rds"),
                                  error = model_j[1])
          
          failedmods <- rbind(failedmods, failedmod)
          
          modj_i <- data.table(managed_area = i,
                               species = j,
                               filename = paste0("SAV_", parameters[column == p, type], "_", 
                                                 ma_abrev, "_",
                                                 short_model_name, ".rds"),
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
      
      plotdat <- SAV4[System == i & !is.na(eval(p)), ] %>% group_by(analysisunit_halid, Year, relyear, eval(p)) %>% summarise(npt = n())
      setDT(plotdat)
      setnames(plotdat, "eval(p)", "data")
      aucol <- names(plotdat[,1])
      
      # declaring available models
      models <- data_directory_sav[["models"]][[ma_abrev]][[as.character(p)]]
      
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
        labs(title = parameters[column == p, name], 
             subtitle = paste0(i, " - ", "Big Bend Seagrasses AP"),
             x = "Year",
             y = parameters[column == p, name],
             color = "Species",
             linetype = "Trend significance (alpha = 0.05)") +
        plot_theme +
        ylim(miny, 100) +
        scale_x_continuous(breaks = breaks_seq, labels = labels_seq) +
        scale_colour_manual(values = spcols)
      
      #create second base plot to feature scatter points and make facetted multi-plots
      plot_i_2 <- ggplot(data = droplevels(plotdat),
                         aes(x = relyear, y = data, fill = npt)) +
        geom_point(shape = 21, alpha = 0.9, color = "grey50") +
        geom_hline(yintercept = 0, color = "grey10", lwd = 0.5) +
        labs(title = parameters[column == p, name], 
             subtitle = paste0(i, " - ", "Big Bend Seagrasses AP"),
             x = "Year",
             y = parameters[column == p, name],
             fill = "Number of\nobservations") +
        plot_theme +
        ylim(miny, 100) +
        scale_fill_continuous_sequential(palette = "YlGnBu") +
        scale_x_continuous(breaks = breaks_seq, labels = labels_seq)
      
      if(length(models) > 0){
        #make sure that no failed models slipped through
        classes <- lapply(models, function(x) class(eval(x)))
        models <- models[classes != "try-error"]
        
        # trendlines single plot (addfits function)
        plot_i <- addfits(models, plot_i, p)
        
        # trendlines multi-plot (addfits_blacktrendlines function)
        aucol <- as.name(names(plot_i_2$data)[1])
        
        # modify_species_labels changes scientific into common if needed
        # also replaces "Unidentified Halophila" with "Halophila, unk."
        plot_i_2 <- addfits_multiplots(models, plot_i_2, p, aucol)
      }
      
      #Save the single plot object as .rds
      saveRDS(plot_i, here::here(paste0("output/Figures/SAV_", parameters[column == p, type], "_",
                                        paste0(str_sub(ma_abrev, 1, -1), "_trendplot.rds"))))
      
      #Save the multi plot object as .rds
      saveRDS(plot_i_2, here::here(paste0("output/Figures/SAV_", parameters[column == p, type], "_",
                                          paste0(str_sub(ma_abrev, 1, -1), "_multiplot.rds"))))
      
      #Save the results table objects as .rds
      data_directory_sav[["lmeresults"]][[ma_abrev]] <- lmemodresults
      
      # saveRDS(lmemodresults, here::here(paste0("output/tables/SAV_", parameters[column == p, type], "_", 
      #                                          paste0(str_sub(ma_abrev, 1, -1), "_lmeresults.rds"))))
    }
    
    ###### BAR PLOTS ######
    if(paste0(p) == "PA" & "PA" %in% Analyses){
      #Bar chart of proportions by analysisunit
      breaks <- c(seq(min(SAV4[System == i & !is.na(PA), relyear]),
                      max(SAV4[System == i & !is.na(PA), relyear]),
                      by = 2))
      yrlist <- sort(unique(SAV4$Year))
      
      labels <- c()
      for(b in breaks){
        labels <- append(labels, yrlist[b + 1])
      }
        
      bpdat <- props[System == i & !is.na(analysisunit) & analysisunit != "Halophila spp.", ]
      
      sp_list <- unique(bpdat$analysisunit)
      sp_list <- sp_list[order(match(sp_list, names(spcols)))]
      
      # add color scale, determining if scientific or common names
      sp_labels <- modify_species_labels(sp_list, usenames)
      
      barplot_sp <- ggplot(data = bpdat, aes(x = relyear, y = sp_pct, fill = analysisunit)) +
        geom_col(color = "grey20") +
        scale_x_continuous(breaks = breaks, labels = labels) +
        plot_theme +
        labs(title = parameters[column == p, name], 
             subtitle = paste0(i, " - ", "Big Bend Seagrasses AP"),
             fill = "Species",
             x = "Year",
             y = "Occurrence frequency (%)") +
        scale_color_manual(values = subset(spcols, names(spcols) %in% sp_list),
                           labels = sp_labels,
                           aesthetics = c("color", "fill"))
      
      saveRDS(barplot_sp, here::here(paste0("output/Figures/SAV_", parameters[column == p, type], "_",
                                            ma_abrev, "_barplot_sp.rds")))
    }
    
    print(paste0("  Plot objects and results tables saved: ",
                 ma_abrev,
                 "_",
                 short_model_name))
  }
}

#Save failedmodslist-----------------------------------------------------
# saveRDS(failedmods, here::here("output/models/failedmodslist.rds"))

#Get rid of eval(p)'s from plot file mappings---------------------------------------
files <- list.files(here::here("output/Figures/")) #get file list
files <- str_subset(files, ".rds") #exclude non-.RDS files

filesupdated <- list()
for(f in seq_along(files)){
  file_f <- readRDS(here::here(paste0("output/Figures/", files[f])))
  if(paste0(as_label(file_f$mapping$y)) == "eval(p)"){
    file_f$mapping$y <- parameters[name %in% file_f$labels$y, column][[1]]
    saveRDS(file_f, here::here(paste0("output/Figures/", files[f])))
    rm(file_f)
    filesupdated <- append(filesupdated, files[f])
  } else {
    rm(file_f)
  }
  if(round((f/length(files))*100, 1) %% 10 == 0){
    print(paste0(round((f/length(files))*100), "% done!"))
  }
}

# Begin trend table preparation
# Combine lme results into single file
lme_table <- bind_rows(data_directory_sav[["lmeresults"]])

#Keep only rows that are values with "fixed" in the effect column
lme_table <- lme_table[lme_table$effect=="fixed" & !is.na(lme_table$effect),]

#For each managed area and species, get the LME intercept, slope, and p values
lme_table <- lme_table %>%
  group_by(managed_area, species) %>%
  summarise(LME_Intercept = estimate[term == "(Intercept)"],
            LME_Slope = estimate[term == "relyear"],
            p = p.value[term == "relyear"], .groups = "keep")

#Add statistical trend column to denote where p<=0.05 and whether LME_slope increase or decreasing
lme_table$StatisticalTrend <- ifelse(lme_table$p <= 0.05 & lme_table$LME_Slope > 0, "Significantly increasing trend",
                                  ifelse(lme_table$p <= 0.05 & lme_table$LME_Slope <0, "Significantly decreasing trend", "No significant trend"))

#Change column names to better match other outputs
lme_table <- setnames(lme_table, c("managed_area", "species"), c("System", "Species"))

# round P-val, LME_slope and LME_intercept
lme_table$p <- round(lme_table$p,4)
lme_table$LME_Intercept <- round(lme_table$LME_Intercept,4)
lme_table$LME_Slope <- round(lme_table$LME_Slope,4)

# Generate BBpct_stats for sufficientdata etc. columns
stats <- SAV4 %>%
  group_by(System, analysisunit_halid) %>%
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
# Rename AU column to species
setDT(stats)
setnames(stats, "analysisunit_halid", "Species")

# Merge data frames
stats <- merge.data.frame(stats, lme_table,
                          by=c("System","Species"), all=TRUE)
# Set order of dataframe
stats <- as.data.table(stats[order(stats$System, stats$Species), ])

# Impute NA values where necessary
stats$EarliestYear[stats$EarliestYear=="Inf"] <- NA
stats$LatestYear[stats$LatestYear=="-Inf"] <- NA

# Filling remaining values in StatisticalTrend column
stats$StatisticalTrend[stats$SufficientData==FALSE] <- "Insufficient data to calculate trend"
stats$StatisticalTrend[stats$SufficientData==TRUE & is.na(stats$LME_Slope)] <- "Model did not fit the available data"

stats$years <- paste0(stats$EarliestYear," - ",stats$LatestYear)
stats$years[stats$SufficientData==FALSE] <- NA

# Write output table to a pipe-delimited txt file
fwrite(stats, "output/tables/SAV_BBpct_LMEresults_All.txt", sep="|")

# SAV Map generation
# Store results in sav_maps_list to call within report
sav_maps_list <- list()
# Define palette
pal <- colorFactor("plasma", SAV4$ProgramID)

for(sys in sys_include){
  # Gather amount of data at each sampling location
  data <- SAV4[System==sys, ] %>% 
    group_by(ProgramLocationID) %>%
    summarise(n_data = n(),
              lat = unique(Latitude_D),
              lon = unique(Longitude_),
              ProgramID = unique(ProgramID))
  # Define radius to display n_data
  rad <- sqrt(data$n_data)

  # Map
  map <- leaflet(data, options = leafletOptions(zoomControl = FALSE)) %>% 
    addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
    addCircleMarkers(lat = ~lat, lng = ~lon, fillColor=~pal(ProgramID),
                     weight=0.5, fillOpacity = 0.4, radius=rad,
                     color="black") %>%
    addLegend(pal=pal, values=~ProgramID, 
              labFormat=labelFormat(prefix="Program "), title="")
  
  # Add mini map and scale
  map <- map %>%
    addMiniMap(centerFixed = c(mean(data$lat),mean(data$lon)), 
               zoomLevelOffset = -5, 
               position = 'topleft', 
               tiles = providers$CartoDB.PositronNoLabels) %>%
    addScaleBar(position = "bottomright",
                options = scaleBarOptions(metric=TRUE))
  
  # Map output filepath
  map_out <- paste0("output/maps/SAV_",sys,".png")
  # Export map to png
  mapshot(map, file = map_out, vwidth = 1200, vheight = 1080)
  # draw .png with ggplot
  p1 <- ggdraw() + draw_image(map_out, scale = 1)
  # Store static map objects
  sav_maps_list[[sys]] <- p1
}

# SAV LMEResults Table Function
# For use in report generation
sav_trend_table <- function(sys){
  table <- stats[System == sys, c("Species","StatisticalTrend","years","LME_Intercept","LME_Slope","p")]
  
  caption <- paste0("Percent Cover Trend Analysis for ", sys)
  
  sav_kable <- table %>%
    kable(format="simple",caption=caption,
          col.names = c("*Species*","*Trend Significance* (0.05)","*Period of Record*","*LME_Intercept*","*LME_Slope*","*p*")) %>%
    kable_styling()
  
  print(sav_kable)
  cat("\n")
}