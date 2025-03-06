MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE, na.strings = "")

# Functions ----
diagnosticplots <- function(model, indicator, managedarea, sizeclass="",
                            historical=FALSE){
  ind <- case_when(str_detect(indicator, "ercent") ~ "Pct",
                   str_detect(indicator, "ensity") ~ "Den",
                   str_detect(indicator, "^S|^s") ~ "SH")
  
  ma <- managedarea
  
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
  
  #Save diagnostic plot(s) of chains
  diag <- plot(model, plot=FALSE)
  
  title <- textGrob(paste0(ma, " (", ind, " ", sizelab, ")"),
                    just="left",
                    gp=gpar(fontsize=10))
  
  diag[[1]] <- gtable_add_rows(
    diag[[1]],
    heights=grobHeight(title)+unit(5, "mm"),
    pos=0
  )
  
  diag[[1]] <- gtable_add_grob(
    diag[[1]],
    title,
    clip="off",
    1, 1, 1, 1)
  
  if(class(try(diag[[2]], silent=TRUE)) != "try-error"){
    diag[[2]] <- gtable_add_rows(
      diag[[2]],
      heights=grobHeight(title)+unit(5, "mm"),
      pos=0
    )
  }
  
  if(class(try(diag[[3]], silent=TRUE)) != "try-error"){
    diag[[3]] <- gtable_add_rows(
      diag[[3]],
      heights=grobHeight(title)+unit(5, "mm"),
      pos=0
    )
  }
  
  #save chains plots
  jpeg(filename=paste0("output/Figures/", ind, "_AllDates_GLMM_", ma,
                       "_PDistandMChains_", ifelse(sizeclass != "",
                                                   paste0(size, "_"), ""),
                       ifelse(historical==TRUE, "hist_", "_"),
                       Sys.Date(), ".png"),
       width=6,
       height=ifelse(length(diag)==1, 6, ifelse(length(diag)==2, 12, 18)),
       units="in", quality=100, res=300)
  print(grid.arrange(grobs=diag, ncol=1))
  dev.off()
  
  #Save posterior predictive check plot
  postpc <- tryCatch(pp_check(model),
                     error=function(e) NA)
  k <- 1001
  
  while(is.na(postpc)==TRUE & k <= 1000){
    postpc <- tryCatch(pp_check(model), 
                       error=function(e) NA)
    k <- k+1
  }
  
  if(!is.na(postpc)){
    postpc <- postpc +
      labs(title=paste0(ind, "_AllDates_GLMM_", ma, "_PPcheck_",
                        ifelse(sizeclass != "", paste0(size, "_"), ""),
                        ifelse(historical==TRUE, "_hist_", "_"),
                        Sys.Date(), ".png"))
    
    ma_short <- MA_All[ManagedAreaName==ma, Abbreviation]
    
    ggsave(paste0("output/Figures/", ind, "_AllDates_GLMM_", ma_short, "_PPcheck_",
                  ifelse(sizeclass != "", paste0(size, "_"), ""),
                  ifelse(historical==TRUE, "_hist_", "_"), Sys.Date(),
                  ".png"),
           postpc,
           width=6,
           height=6,
           units="in",
           dpi=300)
  }
  
  print(paste0("Plots saved."))
  
}

# Save marginal effects plots
meplots <- function(models, data, indicator, managedarea, sizeclass="",
                    zoom=FALSE){
  ind <- case_when(str_detect(indicator, "ercent") ~ "Pct",
                   str_detect(indicator, "ensity") ~ "Den",
                   str_detect(indicator, "^S|^s") ~ "SH")
  
  ma <- managedarea
  
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
  
  
  
  
  if(ind=="Den"){ 
    nyrs <- max(data$LiveDate)-min(data$LiveDate)+1
    maxyr <- max(data$LiveDate)
    minyr <- min(data$LiveDate)
    if(grepl("Natural", unique(data$MA_plotlab))==TRUE){
      type <- "Natural"
    } else{
      type <- "Restored"
    }
    yrdiff <- unique(data$YearDiff)
    # Creates break intervals for plots based on number of years of data
    if(nyrs>=40){
      # Set breaks to every 10 years if more than 40 years of data
      brk <- 10
    }else if(nyrs<40 & nyrs>=20){
      # Set breaks to every 5 years if between 40 and 20 years of data
      brk <- 5
    }else if(nyrs<20 & nyrs>=12){
      # Set breaks to every 3 years if between 20 and 12 years of data
      brk <- 3
    }else if(nyrs<12 & nyrs>=8){
      # Set breaks to every 2 years if between 12 and 8 years of data
      brk <- 2
    }else if(nyrs<8){
      # Set breaks to every year if less than 8 years of data
      brk <- 1
    }else if(nyrs<3){
      brk <- 1
      maxyr <- maxyr + 1
      minyr <- minyr - 1
    }
    yrlist <- seq(minyr,maxyr,brk)
    # nbreaks <- ifelse(nyrs < 11, nyrs+1, 12)
    # breaks <- if(minyr==0){
    #   c(minyr, round(minyr+c(1:(nbreaks-2))*((nyrs/nbreaks) +
    #                                            (nyrs/nbreaks)/nbreaks)),
    #     maxyr)+1
    # } else{
    #   c(minyr, round(minyr+c(1:(nbreaks-2))*((nyrs/nbreaks) +
    #                                            (nyrs/nbreaks)/nbreaks)),
    #     maxyr)
    # }
    
    denplots <- plot(conditional_effects(models[[1]], re_formula=NULL),
                     plot=FALSE)
    
    plot1 <- ggplot() +
      {if("meanDen_int" %in% colnames(data)){
        geom_point(data=data, aes(x=LiveDate,
                                  y=meanDen_int), position=plot_jitter,
                   shape=21, size=2, color="#333333", fill="#cccccc",
                   alpha=1, inherit.aes=FALSE)
      } else{
        geom_point(data=data, aes(x=LiveDate,
                                  y=Density_m2), position=plot_jitter,
                   shape=21, size=2, color="#333333", fill="#cccccc",
                   alpha=1, inherit.aes=FALSE)
      }
      } +
      list(geom_ribbon(data=denplots$RelYear$data,
                       aes(x=RelYear+yrdiff, y=Density_m2,
                           ymin=lower__, ymax=upper__),
                       fill="#000099", alpha=0.1, inherit.aes=FALSE),
           geom_line(data=denplots$RelYear$data,
                     aes(x=RelYear+yrdiff,
                         y=estimate__),
                     color="#000099", lwd=0.75, inherit.aes=FALSE)) +
      scale_x_continuous(limits=c(minyr-0.25, maxyr+0.25),
                         breaks=yrlist) +
      plot_theme +
      {if("meanDen_int" %in% colnames(data)){
        labs(title="Oyster Density",
             subtitle=managedarea,
             x="Year",
             y=bquote('Estimated density ('*~m^{-2}*')'))
      }else{
        labs(title="Oyster Density",
             subtitle=managedarea,
             x="Year",
             y=bquote('Density ('*~m^{-2}*')'))
      }}
    # labs(title="Oyster Density",
    #      subtitle=managedarea,
    #      x="Year",
    #      y=ifelse("meanDen_int" %in% colnames(data),
    #                           "Estimated density (square meters)",
    #                           bquote('Richness (species/100'*~m^{2}*')')))
    
    
    ma_short <- MA_All[ManagedAreaName==ma, Abbreviation]
    
    ggsave(paste0("output/Density/Figures/Oyster_Dens_GLMM_", ma_short, "_", type,
                  ifelse(sizeclass != "", paste0(size), "_raw"), ".png"),
           plot1,
           width=8,
           height=4,
           units="in",
           dpi=200)
  }
  
  
  #Marginal effects plot including random effects for percent live
  if(ind=="Pct"){
    nyrs <- max(data$LiveDate)-min(data$LiveDate)+1
    maxyr <- max(data$LiveDate)
    minyr <- min(data$LiveDate)
    yrdiff <- unique(data$YearDiff)
    # Creates break intervals for plots based on number of years of data
    if(nyrs>=40){
      # Set breaks to every 10 years if more than 40 years of data
      brk <- 10
    }else if(nyrs<40 & nyrs>=20){
      # Set breaks to every 5 years if between 40 and 20 years of data
      brk <- 5
    }else if(nyrs<20 & nyrs>=12){
      # Set breaks to every 3 years if between 20 and 12 years of data
      brk <- 3
    }else if(nyrs<12 & nyrs>=8){
      # Set breaks to every 2 years if between 12 and 8 years of data
      brk <- 2
    }else if(nyrs<8){
      # Set breaks to every year if less than 8 years of data
      brk <- 1
    }else if(nyrs<3){
      brk <- 1
      maxyr <- maxyr + 1
      minyr <- minyr - 1
    }
    yrlist <- seq(minyr,maxyr,brk)
    # nbreaks <- ifelse(nyrs < 11, nyrs+1, 12)
    # breaks <- if(minyr==0){
    #   c(minyr, round(minyr+c(1:(nbreaks-2))*((nyrs/nbreaks) +
    #                                            (nyrs/nbreaks)/nbreaks)),
    #     maxyr)+1
    # } else{
    #   c(minyr, round(minyr+c(1:(nbreaks-2))*((nyrs/nbreaks) +
    #                                            (nyrs/nbreaks)/nbreaks)),
    #     maxyr)
    # }
    
    set.seed(987)
    pctplots <- plot(conditional_effects(models[[1]], re_formula=NULL),
                     plot=FALSE)
    
    plot1 <- ggplot() +
      geom_point(data=data, aes(x=LiveDate,
                                y=100*PercentLive_dec), position=plot_jitter,
                 shape=21, size=2, color="#333333", fill="#cccccc",
                 alpha=1, inherit.aes=FALSE) +
      {if(names(pctplots$RelYear$data[2])=="PercentLive_dec"){
        list(geom_ribbon(data=pctplots$RelYear$data,
                         aes(x=RelYear+yrdiff,
                             y=100*PercentLive_dec, ymin=100*lower__,
                             ymax=100*upper__), fill="#000099",
                         alpha=0.1, inherit.aes=FALSE),
             geom_line(data=pctplots$RelYear$data,
                       aes(x=RelYear+yrdiff,
                           y=100*estimate__), color="#000099",
                       lwd=0.75, inherit.aes=FALSE))
      } else{
        list(geom_ribbon(data=pctplots$RelYear$data,
                         aes(x=RelYear+yrdiff,
                             y=100*LiveObs, ymin=100*lower__,
                             ymax=100*upper__), fill="#000099",
                         alpha=0.1, inherit.aes=FALSE),
             geom_line(data=pctplots$RelYear$data,
                       aes(x=RelYear+yrdiff,
                           y=100*estimate__), color="#000099",
                       lwd=0.75, inherit.aes=FALSE))
      }} +
      scale_x_continuous(limits=c(minyr-0.25, maxyr+0.25),
                         breaks=yrlist) +
      plot_theme +
      theme(legend.text=element_text(size=10), 
            legend.title=element_text(size=10)) +
      {
        if(managedarea=="Lemon Bay Aquatic Preserve"){
          labs(title="Percent of Live vs. Dead Oysters",
               subtitle=managedarea,
               x="Year",
               y="Live vs. dead (%)")
        } else {
          labs(title="Oyster Percent Live Cover",
               subtitle=managedarea,
               x="Year",
               y="Live cover (%)")
        }
      }
    
    ma_short <- MA_All[ManagedAreaName==ma, Abbreviation]
    
    ggsave(paste0("output/Percent_Live/Figures/Oyster_PrcLive_GLMM_", ma_short,
                  "_raw.png"),
           plot1,
           width=8,
           height=4,
           units="in",
           dpi=200)
    
    #Plot of modeled mean percent live
    if("Region" %in% names(pctplots)){
      meanPct <- pctplots$Region$data
      setnames(meanPct, "effect1__", "Region")
      
      meanpctplot <- ggplot(meanPct, aes(x=Region, y=estimate__,
                                         ymin=lower__, ymax=upper__)) +
        geom_pointinterval(fill="black", size=3,
                           fatten_point=4, shape=21,
                           color="black") +
        labs(title="Oyster Percent Live Cover",
             subtitle=managedarea,
             y="Live cover (%)",
             fill=NULL) +
        plot_theme +
        theme(legend.text=element_text(size=10), 
              legend.title=element_text(size=10))
      
      ma_short <- MA_All[ManagedAreaName==ma, Abbreviation]
      
      ggsave(paste0("output/Percent_Live/Figures/Oyster_PrcLive_GLMM_", ma_short,
                    "_raw_MeanRes.png"),
             meanpctplot,
             width=8,
             height=4,
             units="in",
             dpi=200)
    }
    
    #Plot of RelYear * Region interaction
    if("RelYear:Region" %in% names(pctplots)){
      pctplots$RelYear$data$RelYear <-
        pctplots$RelYear$data$RelYear-
        (min(pctplots$RelYear$data$RelYear)-1)
      RelYrbyRegion <- pctplots$`RelYear:Region`
      
      
      intplot <- RelYrbyRegion +
        geom_point(data=data, aes(x=RelYear-(min(RelYear)-1),
                                  y=PercentLive_dec,
                                  fill=Region),
                   alpha=0.5, shape=21, size=3, color="black",
                   inherit.aes=FALSE) +
        scale_x_continuous(breaks=breaks,
                           labels=c(yrlist[breaks])) +
        labs(title=ma,
             x="Year",
             y="Proportion live",
             fill="Region") +
        plot_theme +
        theme(legend.text=element_text(size=12), 
              legend.title=element_text(size=13), 
              legend.position="none") +
        facet_wrap(~ Region, ncol=3, scales="free")
      
      ma_short <- MA_All[ManagedAreaName==ma, Abbreviation]
      
      ggsave(paste0("output/Percent_Live/Figures/Oyster_PrcLive_GLMM_", ma_short,
                    "_raw.png"),
             intplot,
             width=10,
             height=10,
             units="in",
             dpi=300)
    }
  }
}

# Create model results tables and save diagnostic plots
modresults <- function(datafile, models, indicator, meplotzoom=FALSE){
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
                    managed_area=unique(datafile$ManagedAreaName),
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
    oysterresults <<- rbind(oysterresults, oyres_i)
    
    # Save diagnostic plots
    #diagnosticplots(modelobj, indicator,
    #unique(datafile$ManagedAreaName), sizeclass,
    #ifelse(str_detect(modelobj$file, "_hist"), TRUE, FALSE))  
  }
  
  # Save marginal effects plots
  meplots(models, datafile, indicator, unique(datafile$ManagedAreaName),
          sizeclass, meplotzoom)
}
# Marginal effects plots for shell height (attempt to combine models into one plot)
meplotssh <- function(models1, data1, sizeclass1="", models2, data2,
                      sizeclass2="", managedarea, indicator, zoom=FALSE){
  ind <- case_when(str_detect(indicator, "ercent") ~ "Pct",
                   str_detect(indicator, "ensity") ~ "Den",
                   str_detect(indicator, "^S|^s") ~ "SH")
  ma <- managedarea
  
  sizeclass1 <- unique(data1$SizeClass)
  sizeclass2 <- unique(data2$SizeClass)
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
  }
  
  #Marginal effects plot including random effects
  ## Hist plot settings
  y_max <- round(max(data2[!is.na(ShellHeight_mm), ShellHeight_mm]), -0)+1
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
    nyrs <- (maxyr)-(minyr)+1
    
    # Creates break intervals for plots based on number of years of data
    if(nyrs>=40){
      # Set breaks to every 10 years if more than 30 years of data
      brk <- 10
    }else if(nyrs<40 & nyrs>=20){
      # Set breaks to every 5 years if between 30 and 15 years of data
      brk <- 5
    }else if(nyrs<20 & nyrs>=12){
      # Set breaks to every 3 years if between 15 and 9 years of data
      brk <- 3
    }else if(nyrs<12 & nyrs>=8){
      # Set breaks to every 2 years if between 9 and 6 years of data
      brk <- 2
    }else if(nyrs<8 & nyrs>=3){
      # Set breaks to every year if less than 6 years of data
      brk <- 1
    }else if(nyrs<3){
      brk <- 1
      maxyr <- maxyr + 1
      minyr <- minyr - 1
    }
    return(list("seq" = seq(minyr,maxyr,brk),"maxyr" = maxyr, "minyr" = minyr))
  }
  
  yrlist_hist <- set_breaks(type = "hist", data1 = data1, data2 = data2)[["seq"]]
  maxyr_hist <- set_breaks(type = "hist", data1 = data1, data2 = data2)[["maxyr"]]
  minyr_hist <- set_breaks(type = "hist", data1 = data1, data2 = data2)[["minyr"]]
  yrlist_live <- set_breaks(type = "live", data1 = data1, data2 = data2)[["seq"]]
  maxyr_live <- set_breaks(type = "live", data1 = data1, data2 = data2)[["maxyr"]]
  minyr_live <- set_breaks(type = "live", data1 = data1, data2 = data2)[["minyr"]]
  
  ## Check data for Exact and Estimate
  n_hist1 <- nrow(data1[data1$LiveDate_Qualifier=="Estimate" &
                          !is.na(data1$ShellHeight_mm),])
  n_live1 <- nrow(data1[data1$LiveDate_Qualifier=="Exact" &
                          !is.na(data1$ShellHeight_mm),])
  n_hist2 <- nrow(data2[data2$LiveDate_Qualifier=="Estimate" &
                          !is.na(data2$ShellHeight_mm),])
  n_live2 <- nrow(data2[data2$LiveDate_Qualifier=="Exact" &
                          !is.na(data2$ShellHeight_mm),])
  
  set.seed(987)
  if(!is.null(models1)){
    liveplot_1 <- plot(conditional_effects(models1[[1]], re_formula=NULL), plot=FALSE)
  }
  
  if(!is.null(models2)){
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
  
  p_color <- c(col1, col2)
  
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
    geom_point(data=data1[!is.na(RelYear) & !is.na(LiveDate), ],
               aes(x=LiveDate, y=ShellHeight_mm, shape="size1"),
               position=plot_jitter, size=2, color="#333333", fill = "#cccccc",
               alpha=0.6, show.legend = TRUE) +
    geom_point(data=data2[!is.na(RelYear) & !is.na(LiveDate), ],
               aes(x=LiveDate, y=ShellHeight_mm, shape="size2"),
               position=plot_jitter, size=2, color="#333333", fill = "#cccccc",
               alpha=0.6, show.legend = TRUE) +
    plot_theme +
    theme(legend.position="right") +
    scale_shape_manual(name="Shell heights",
                       breaks = c("size1", "size2"),
                       values=p_shape,
                       labels=sizelab) +
    scale_color_manual(name="Shell heights",
                       breaks = c("size1", "size2"),
                       values=p_color,
                       labels=sizelab, 
                       aesthetics = c("color", "fill"))
  
  leg <- get_legend(plot_leg)
  rm(plot_leg)
  
  # Dead oyster shell plot
  plot1 <- ggplot() +
    geom_hline(yintercept=75, size=1, color="grey") +
    {if(n_hist1>0){
      geom_point(data=data1[!is.na(RelYear) &
                              !is.na(LiveDate) &
                              LiveDate_Qualifier=="Estimate", ],
                 aes(x=LiveDate, y=ShellHeight_mm, shape="size1"),
                 position=plot_jitter, size=2, color="#333333", fill="#cccccc",
                 alpha=0.2, inherit.aes=FALSE) 
    }} +
    {if(n_hist2>0){
      geom_point(data=data2[!is.na(RelYear) & !is.na(LiveDate) &
                              LiveDate_Qualifier=="Estimate", ],
                 aes(x=LiveDate, y=ShellHeight_mm, shape="size2"),
                 position=plot_jitter, size=2, color="#333333", fill="#cccccc",
                 alpha=0.2, inherit.aes=FALSE)
    }} +
    scale_x_continuous(limits=c(minyr_hist-0.25, maxyr_hist+0.25),
                       breaks=yrlist_hist) +
    scale_y_continuous(breaks=y_breaks,
                       labels=y_labs, minor_breaks=y_minor) +
    plot_theme +
    theme(plot.subtitle=element_text(hjust=0, size=10, color="#314963"),
          legend.position="none",
    ) +
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
  
  # Live oyster shell plot
  plot2 <- ggplot() +
    geom_hline(yintercept=75, size=1, color="grey") +
    {if(n_live1>0){
      geom_point(data=data1[!is.na(RelYear) & !is.na(LiveDate) &
                              LiveDate_Qualifier=="Exact", ],
                 aes(x=LiveDate, y=ShellHeight_mm, shape="size1"),
                 position=plot_jitter, size=2, color="#333333", fill="#cccccc",
                 alpha=0.2, inherit.aes=FALSE) 
    }} +
    {if(n_live2>0){
      geom_point(data=data2[!is.na(RelYear) & !is.na(LiveDate) &
                              LiveDate_Qualifier=="Exact", ],
                 aes(x=LiveDate, y=ShellHeight_mm, shape="size2"),
                 position=plot_jitter, size=2, color="#333333", fill="#cccccc",
                 alpha=0.2, inherit.aes=FALSE)
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
          legend.position="none",
          axis.text.y=element_blank(),  #remove y-axis labels
          axis.ticks.y=element_blank(),  #remove y-axis ticks
          axis.title.y=element_blank()   #removes y-axis title
    ) +
    labs(subtitle="Live Oyster Shells",
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
  
  # Set plot title
  plot_title <- ggplot() +
    labs(title="Oyster Size Class", subtitle=ma) +
    plot_theme +
    theme(plot.subtitle=element_text(hjust=0.5, size=10, color="#314963"),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), axis.line=element_blank())
  
  plot_comb <- ggarrange(plot1, plot2, leg, nrow=1,
                         widths=c(0.46, 0.39, 0.15))
  
  plot_comb <- ggarrange(plot_title, plot_comb, ncol=1,
                         heights=c(0.125, 0.875))
  
  ma_abrev <- MA_All[ManagedAreaName==ma, Abbreviation]
  
  ggsave(paste0("output/Shell_Height/Figures/Oyster_SH_GLMM_", ma_abrev, ".png"),
         plot_comb,
         width=8,
         height=4,
         units="in",
         dpi=200,
         bg="white")
  rm(liveplot_1, liveplot_2, liveplot1_avail, liveplot2_avail)
}

# Create model results tables and save diagnostic plots
modresultssh <- function(datafile1, models1, datafile2, models2, indicator,
                         meplotzoom=FALSE){
  datafile1$SizeClass[datafile1$SizeClass=="25to75mm" &
                        datafile1$MA_plotlab==
                        "St. Martins Marsh Aquatic Preserve_Natural"] <-
    "35-75mm"
  sizeclass1 <- unique(datafile1$SizeClass)
  for(m in seq_along(models1)){
    modelobj <- models1[[m]]
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
                    managed_area=unique(datafile1$ManagedAreaName),
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
    
    oysterresults <<- rbind(oysterresults, oyres_i)
    
    # Save diagnostic plots
    #diagnosticplots(modelobj, indicator,
    #unique(datafile$ManagedAreaName), sizeclass,
    #ifelse(str_detect(modelobj$file, "_hist"), TRUE, FALSE))  
  }
  
  datafile2$SizeClass[datafile2$SizeClass=="25to75mm" &
                        datafile2$MA_plotlab==
                        "St. Martins Marsh Aquatic Preserve_Natural"] <- "35-75mm"
  sizeclass2 <- unique(datafile2$SizeClass)
  
  for(m in seq_along(models2)){
    modelobj <- models2[[m]]
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
        managed_area = unique(datafile2$ManagedAreaName),
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
    oysterresults <<- rbind(oysterresults, oyres_i)
    
    # Save diagnostic plots
    #diagnosticplots(modelobj, indicator,
    #unique(datafile$ManagedAreaName), sizeclass,
    #ifelse(str_detect(modelobj$file, "_hist"), TRUE, FALSE))  
  }
  
  # Save marginal effects plots
  meplotssh(models1, datafile1, sizeclass1, models2, datafile2, sizeclass2,
            unique(datafile1$ManagedAreaName), indicator, meplotzoom)
}