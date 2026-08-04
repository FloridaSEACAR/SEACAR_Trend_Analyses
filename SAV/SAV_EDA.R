# Perform exploratory data analysis for SAV
# Functions and set up for streamlined EDA plots
eda_output_path <- "output/Figures/EDA/"
plot_names <- c("parvYear_bysp", "parvYear_bypr", "spvYear_bypr", "qsvYear_bysp",
                "qsvYear_bypr", "metvYear_bysp", "metvYear_bypr", "metvqs_bysp",
                "metvqs_bypr", "grvYear_bysp", "grvYear_bypr", "dpvYear_bysp", "dpvYear_bypr")
# Create EDA folder
if(!file.exists(eda_output_path)) dir.create(eda_output_path)

plot_eda <- function(plot_type, p, i){
  # plot_type is EDA plot type (plot_names above)
  # p is parameter name as.name()
  # i is managed area name
  ma_abrev <- MA_All[ManagedAreaName==i, Abbreviation]
  grouping <- str_split_1(plot_type, "_")[2]
  y_axis <- str_split_1(str_split_1(plot_type, "_")[1], "v")[1]
  x_axis <- str_split_1(str_split_1(plot_type, "_")[1], "v")[2]

  # Color palette set-up (should match spatio-temporal scope plots) - for program plots
  progs <- SAV4[ManagedAreaName == i & !is.na(BB_pct), unique(ProgramName)]
  # Set palette for these programs
  color_pal <- prcollist[round(seq(1, length(prcollist), length.out = length(progs)))]
  names(color_pal) <- progs

  if(x_axis=="Year"){x_par <- "Year"} else if(x_axis=="qs"){x_par <- "QuadSize_m2"}

  au <- ifelse(i %in% ma_halspp, "analysisunit", "analysisunit_halid")

  if(y_axis=="met"){
    dat <- SAV4[ManagedAreaName == i & get(au)!="No grass in quadrat", ]
  } else {
    dat <- SAV4[ManagedAreaName==i & !is.na(eval(p)) & get(au)!="No grass in quadrat", ]
  }

  if(grouping=="bysp"){
    legend_lab <- "Species"
    color_group <- au
    color_vals <- subset(spcols, names(spcols) %in% dat[, unique(get(au))])
  } else if(grouping=="bypr"){
    legend_lab <- "Program Name"
    color_group <- "ProgramName"
    color_vals <- color_pal
  }

  if(y_axis=="par"){
    y_lab <- parameters[column == p, name]
    y_par <- p
  } else if(y_axis=="sp"){
    y_lab <- "Species"
    y_par <- au
  } else if(y_axis=="qs"){
    y_lab <- "Quadrat size (m^2)"
    y_par <- "QuadSize_m2"
  } else if(y_axis=="met"){
    y_lab <- "Method"
    y_par <- "method"
  } else if(y_axis=="gr"){
    y_lab <- "Grid number"
    y_par <- "Grid_n"
  } else if(y_axis=="dp"){
    y_lab <- "Depth (m)"
    y_par <- "Depth_M"
  }

  if(y_axis=="qs"){
    subtitle <- paste0("Unique QuadSize values: ", paste(unique(dat$QuadSize_m2), " m^2", collapse = ","))
  } else {
    subtitle <- ""
  }

  plot <- ggplot(data = dat,
                 aes(x = get(x_par), y = get(y_par), color = as.factor(get(color_group)))) +
    geom_jitter(alpha=0.5) +
    theme_bw() +
    labs(title = i, y = y_lab, color = legend_lab, x = x_par,
         subtitle = subtitle) +
    scale_color_manual(values = color_vals,
                       aesthetics = c("color", "fill"))
  file_path <- paste0(eda_output_path, ma_abrev, "_", parameters[column == p, type], "_", plot_type, ".png")
  ggsave(plot, filename = file_path, height = 6, width = 8)
}

for(p in parameters$column){
  cat(paste0("\nStarting indicator: ", p, "\n"))
  #List managed areas with at least 5 years of data
  nyears <- SAV4[!is.na(eval(p)) & !is.na(analysisunit), ] %>% group_by(ManagedAreaName, analysisunit) %>% reframe(type = paste0(p), nyr = length(unique(Year)))
  nyears2 <- SAV4[!is.na(eval(p)) & !is.na(analysisunit_halid), ] %>% group_by(ManagedAreaName, analysisunit_halid) %>% reframe(type = paste0(p), nyr = length(unique(Year)))
  setDT(nyears2)
  setnames(nyears2, "analysisunit_halid", "analysisunit")
  nyears <- distinct(rbind(nyears, nyears2))
  ma_include <- unique(subset(nyears, nyears$nyr >= 5)$ManagedAreaName)
  for(i in ma_include){
    ma_abrev <- MA_All %>% filter(ManagedAreaName==i) %>% pull(Abbreviation)
    cat(paste0("\nStarting MA: ", i, "\n"))
    for(plot_type in plot_names){
      # Only create Grid_n plots if there are >0 unique Grid_n values
      if(plot_type %in% c("grvYear_bysp", "grvYear_bypr") & !length(SAV4[ManagedAreaName == i & !is.na(eval(p)) & !is.na(Grid_n), Grid_n]) > 0) next
      # Only create Depth_m plots if there are >0 unique Depth_m values
      if(plot_type %in% c("dpvYear_bysp", "dpvYear_bypr") & !length(SAV4[ManagedAreaName == i & !is.na(eval(p)) & !is.na(Depth_M), Depth_M]) > 0) next
      # Run EDA plot function (exports .png into EDA output folder)
      plot_eda(plot_type, p, i)
    }
  }
}
