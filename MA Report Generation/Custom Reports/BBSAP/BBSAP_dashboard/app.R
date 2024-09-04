# Big Bend Seagrasses Aquatic Preserve
# Shiny App developed by Tyler Hill @ Florida DEP
# Run report.R to generate .rds objects necessary for this app

### Uncomment the following lines to run and test offline
### Comment out before deploying app
###############################################
# library(rstudioapi)
# wd <- dirname(getActiveDocumentContext()$path)
# setwd(wd)
###############################################

library(data.table)
library(dplyr)
library(stringr)
library(leaflet)
library(leaflet.providers)
library(shiny)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(DT)
library(lubridate)
library(shinyjs)
library(bslib)
library(shinydashboard)
library(patchwork)
library(flextable)

# SEACAR Plot theme
plot_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="transparent", colour = NA),
        text=element_text(family="Arial"),
        plot.title=element_text(hjust=0.5, size=14, color="#314963"),
        plot.subtitle=element_text(hjust=0.5, size=12, color="#314963"),
        plot.background = element_rect(fill="transparent", colour = NA),
        legend.title=element_text(size=14),
        legend.text = element_text(size=12),
        legend.text.align = 0,
        legend.background = element_rect(fill = "transparent"),
        axis.title.x = element_text(size=12, margin = margin(t = 5, r = 0,
                                                             b = 10, l = 0)),
        axis.title.y = element_text(size=12, margin = margin(t = 0, r = 10,
                                                             b = 0, l = 0)),
        axis.text=element_text(size=11),
        axis.text.x=element_text(angle = -45, hjust = 0))

# Custom SEACAR palette
seacar_palette <- c("#964059","#E05E7B","#E98C86","#F1B8AB","#F8CAAA","#F8E6B9",
                    "#FEEEE1","#DAE9DA","#8BE4C2","#7EE7E8","#8FD0EC","#6FA1DD",
                    "#889BD1","#8F83D3","#6B59AB")

seacar_sp_palette <- c("#005396","#0088B1","#00ADAE","#65CCB3","#AEE4C1",
                       "#FDEBA8","#F8CD6D","#F5A800","#F17B00")

plot_discrete <- function(sys, param, data, output = "plot"){
  
  y_labels <- param
  
  #Determine max and min time (Year) for plot x-axis
  t_min <- min(data$Year)
  t_max <- max(data$YearMonthDec)
  t_max_brk <- as.integer(round(t_max, 0))
  t <- t_max-t_min
  min_RV <- min(data$Mean)
  
  # Sets break intervals based on the number of years spanned by data
  if(t>=30){
    brk <- -10
  }else if(t<30 & t>=10){
    brk <- -4
  }else if(t<10 & t>=4){
    brk <- -2
  }else if(t<4 & t>=1){
    brk <- -1
  }else if(t<1){
    brk <- -1
    t_max <- round(t_max)
  }
  
  # Create plot object with data and trendline
  p1 <- ggplot(data=data,
               aes(x=YearMonthDec, y=Mean)) +
    # geom_line(size=0.75, color="#333333", alpha=0.6) +
    geom_point(shape=21, size=3, color="#333333", fill="#cccccc", alpha=0.75) +
    geom_segment(aes(x = start_x, y = start_y, xend = end_x, yend = end_y, color = sig), 
                 linewidth=1.2, alpha=0.7, show.legend = TRUE) +
    labs(title=paste0(param),
         subtitle=paste0(sys, " - ", "Big Bend Seagrasses AP"),
         x="Year", y=y_labels) +
    scale_x_continuous(limits=c(t_min-0.25, t_max+0.25),
                       breaks=seq(t_max_brk, t_min, brk)) +
    scale_color_manual(name = "Trend type",
                       values = c("Significant Trend" = "#000099",
                                  "Non-significant Trend" = "#900667")) +
    plot_theme +
    facet_wrap(~Type, dir="h") +
    theme(strip.text.x = element_text(size=12))
  
  # Creates ResultTable to display statistics below plot
  ResultTable <- skt_data_combined[System==sys & ParameterName==param, ] %>%
    select(Type, RelativeDepth, N_Data, N_Years, Median, Independent, tau, p,
           SennSlope, SennIntercept, ChiSquared, pChiSquared, Trend)
  # Create table object
  # t1 <- ggtexttable(ResultTable, rows=NULL, theme=ttheme(base_size=10)) # %>%
    # tab_add_footnote(text="p < 0.00005 appear as 0 due to rounding.\n
    #                   SennIntercept is intercept value at beginning of
    #                   record for monitoring location",
    #                  size=10, face="italic")
  
  t1 <- gen_grob(flextable(ResultTable))
  
  # Align plot with table below it
  # plot <- (p1 / t1) + plot_layout(nrow=2, heights = c(2,1))
  plot <- p1 + t1 + plot_layout(heights=c(2,1))
  
  if(output=="plot"){
    return(p1)
  } else if(output=="table"){
    return(ResultTable)
  } else if(output=="table2"){
    return(t1)
  } else if(output=="both"){
    return(plot)
  }
}

# Function to return WQ model results page
model_table <- function(sys, param, type){
  skt_data_combined %>% 
    filter(
      (sys == "All" | System == sys),
      (param == "All" | ParameterName == param),
      (type == "All" | Type == type)) %>%
    mutate(Period = paste0(EarliestYear, " - ", LatestYear)) %>%
    select(System, Type, ParameterName, RelativeDepth, ActivityType, 
           SufficientData, N_Data, N_Years, Period, Median, p, SennSlope, 
           SennIntercept, ChiSquared, pChiSquared, Trend)
}

# Show modalDialog plots
showPlot <- function(sys, type){
  sys_sub <- gsub(" ","_", sys)
  fig_folder <- "www/figures/"
  if(type=="barplot"){
    title <- paste0("Frequency of Occurrence in ", sys)
    src <- paste0(fig_folder, "SAV_PA_", sys_sub, "_barplot_sp.png")
    h <- "70%"
    w <- "70%"
  } else if(type=="multiplot"){
    title <- paste0("Median Percent Cover in ", sys)
    src <- paste0(fig_folder, "SAV_BBpct_", sys_sub, "_multiplot.png")
    h <- "70%"
    w <- "70%"
  } else if(type=="trendplot"){
    title <- paste0("Median Percent Cover trends in ", sys)
    src <- paste0(fig_folder, "SAV_BBpct_", sys_sub, "_trendplot.png")
    h <- "100%"
    w <- "100%"
  }
  
  showModal(
    modalDialog(
      title = title,
      tags$div(
        tags$img(
          src = src,
          height = h,
          width = w
        ),
        style = "text-align:center;"
      ),
      size = "l",
      easyClose = T
    )
  )
}

# Determining lab/field/all status for labeling
field_params <- c("Dissolved Oxygen","Dissolved Oxygen Saturation",
                  "pH","Secchi Depth","Turbidity","Water Temperature")
lab_params <- c("Chlorophyll a, Corrected for Pheophytin",
                "Chlorophyll a, Uncorrected for Pheophytin",
                "Colored Dissolved Organic Matter","Total Nitrogen",
                "Total Phosphorus","Turbidity")
combined_params <- c("Salinity","Total Suspended Solids")

# RDS filepath
rds_load_path <- "rds/"

# use the following lines to load objects created in report.R
files_to_load <- c("data_combined", "publish_date", "sys_include",
                   "map_df", "sav_map_df", "skt_data_combined")

for(file in files_to_load){
  eval(call("<-", as.name(file), readRDS(paste0(rds_load_path, file, ".rds"))))
}

# Generate maps for each System
params <- unique(map_df$ParameterName)
# Available systems, N to S order
systems <- c("St. Marks", "Aucilla", "Econfina", "Keaton Beach", "Steinhatchee", 
             "Horseshoe Beach", "Suwanee", "Cedar Key", "Waccasassa")
systems <- factor(systems, levels = systems)

sysPal <- colorFactor(seacar_palette, systems, ordered = TRUE)
paramPal <- colorFactor(seacar_palette, params)

groupNames <- c()
for(sys in unique(map_df$System)){
  
  # Blank map for each system to fill with parameter information
  map <- leaflet() %>% addProviderTiles(providers$CartoDB.PositronNoLabels)
  
  for(type in map_df[System==sys, unique(Type)]){
    type_params <- map_df[System==sys & Type==type, unique(ParameterName)]
    for(param in type_params){
      
      filtered_data <- map_df[System==sys & Type==type & ParameterName==param, ]
      
      groupNames <- c(groupNames, paste0(param,"_",type))
      
      map <- map %>%
        addCircleMarkers(data = filtered_data,
                         lat = ~OriginalLatitude, lng = ~OriginalLongitude,
                         weight = 0.5, fillOpacity = 0.4, opacity = 0.4, color="black",
                         fillColor = paramPal(param), group = paste0(param,"_",type),
                         label = ~label, popup = ~popup)
    }
    
  }
  
  # Add SAV data where possible
  if(sys %in% sys_include){
    map <- map %>% 
      addCircleMarkers(data = sav_map_df[System==sys, ],
                       lat = ~OriginalLatitude, lng = ~OriginalLongitude,
                       rad = 5,
                       weight = 0.5, fillOpacity = 0.4, opacity = 0.4, color="lightblue",
                       fillColor = "#005396", group = paste0(sys,"_sav"),
                       label = ~label, popup = ~popup)
  }
  
  # Load map as object of name "[system]_map"
  eval(call("<-", paste0(sys,"_map"), map))
  
}

allMap_df <- setDT(map_df %>% 
  group_by(System, ProgramLocationID, OriginalLatitude, OriginalLongitude, Type) %>% 
  summarise())

allMap <- leaflet() %>% addProviderTiles(providers$CartoDB.PositronNoLabels,
                                         group = "Positron by CartoDB")
for(type in unique(allMap_df$Type)){
  allMap <- allMap %>%
    addCircleMarkers(data = allMap_df[Type==type, ], lat = ~OriginalLatitude, lng = ~OriginalLongitude,
                     weight = 0.5, fillOpacity = 0.4, opacity = 0.4, color = "black",
                     fillColor = ~sysPal(System), group = type)
}

allMap <- allMap %>% addLegend(position = "topright", pal = sysPal, values = systems)

# allMap %>% addLayersControl(baseGroups = c("Positron by CartoDB"),
#                             overlayGroups = c("Estuary","River"),
#                             options = layersControlOptions(collapsed=TRUE))

# UI ----
source("UI_Snippets.R")

# Enables recognition of images folder
addResourcePath(prefix="www", directoryPath = "www")

header <- dashboardHeader(
  title = "BBSAP Dashboard"
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("WQ Model Results", tabName = "model_results", icon = icon("table")),
    selectInput(inputId = "systemSelect",
                label = "Select a System to view",
                choices = c("All", levels(systems)),
                selected = "All"),
    selectizeInput(inputId = "paramSelect",
                   label = "Select a Parameter to view",
                   choices = "All",
                   selected = "Aucilla"),
    selectizeInput(inputId = "typeSelect",
                   label = "Select River or Estuary",
                   choices = "Select a data type",
                   selected = "All")
  )
)

# System display page
sysPage <- dashboardBody(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css")
  ),
  tabItems(
    tabItem(
      tabName = "dashboard",
      fluidRow(
        column(6, 
               leafletOutput("systemMap",
                             height = "30vh"),
               div(style = "max-width:fit-content; margin-left:auto; margin-right:auto;",
                   shinyjs::hidden(checkboxInput("savCheckBox",
                                                 label = "Toggle SAV sampling locations", 
                                                 value = TRUE)))
               ),
        column(6,
               shinyjs::hidden(
                 uiOutput("plotLinks")
               ))),
      fluidRow(
        column(6, 
               plotOutput("wq_plot")))
      ),
    tabItem(
      tabName = "model_results",
      fluidRow(
        column(12,
               div(style = "max-width:fit-content; margin-left:auto; margin-right:auto;",
                   downloadButton("downloadWQ", "Download Model Results"))),
        column(12,
               div(style = "width:100%; overflow-x:auto;",
                   DT::dataTableOutput("model_results_table"))
               )
      )
    )
  ),
)

ui <- dashboardPage(header, sidebar, sysPage, skin="blue")

# Server ----
server <- function(input, output, session){
  
  sys <- reactive({input$systemSelect})
  
  param <- reactive({input$paramSelect})
  
  type <- reactive({input$typeSelect})
  
  types <- reactive({
    data_combined[System==input$systemSelect, unique(Type)]
  })
  
  wq_data <- reactive({
    data_combined[System==sys() & ParameterName==param(), ]
  })
  
  # SAV Check Box
  observeEvent(input$savCheckBox,{
    if(input$savCheckBox){
      leafletProxy("systemMap") %>%
        showGroup(paste0(sys(), "_sav"))
    } else {
      leafletProxy("systemMap") %>%
        hideGroup(paste0(sys(), "_sav"))
    }
  })
  
  observeEvent(input$systemSelect, {
    # Grab unique parameters for each system
    params <- data_combined[System==input$systemSelect, unique(ParameterName)]
    
    # Update values in param select box
    updateSelectizeInput(inputId = "paramSelect", choices = c("All", params))
    
    # Update values in type select box
    updateSelectizeInput(inputId = "typeSelect", choices = c("All", types()))
    
    # Show or hide SAV check Box (hide when "All")
    if(input$systemSelect!="All" & sys() %in% sys_include){
      shinyjs::showElement("savCheckBox")
      shinyjs::showElement("plotLinks")
    } else {
      shinyjs::hideElement("savCheckBox")
      shinyjs::hideElement("plotLinks")
    }
    
  })
  
  observeEvent(input$paramSelect, {
    layerNames <- c()
    for(type in types()){
      layerNames <- c(layerNames, paste0(input$paramSelect, "_", type))
    }
    
    if(input$paramSelect=="All"){
      leafletProxy("systemMap") %>%
        showGroup(groupNames)
    } else {
      leafletProxy("systemMap") %>%
        showGroup(layerNames) %>%
        hideGroup(groupNames[!groupNames %in% layerNames])
    }
    
    updateSelectizeInput(inputId = "typeSelect", selected = "All")

  })
  
  observeEvent(input$typeSelect, {
    if(input$typeSelect=="All"){
      if(param()=="All"){
        layerName <- groupNames
      } else {
        layerName <- groupNames[str_detect(groupNames, param())]
      }
    } else if(!input$typeSelect=="All"){
      if(param()=="All"){
        layerName <- groupNames[str_detect(groupNames, input$typeSelect)]
      } else {
        layerName <- paste0(param(), "_", input$typeSelect)
      }
    }
    
    leafletProxy("systemMap") %>%
      showGroup(layerName) %>%
      hideGroup(groupNames[!groupNames %in% layerName])
  })
  
  output$systemMap <- renderLeaflet({
    if(sys()=="All"){
      allMap
    } else {
      get(paste0(sys(),"_map"))
    }
  })
  
  output$wq_plot <- renderPlot({
    if(param()!="All"){
      plot_discrete(sys = sys(), param = param(), data = wq_data(), output = "both")
    }
  }, bg = "transparent")
  
  output$model_results_table <- renderDataTable({
    DT::datatable(
      model_table(sys(), param(), type()),
      options = list(scrollX = TRUE, autoWidth = TRUE)
    )
  })
  
  # output$wq_table2 <- renderPlot({
  #   if(param()!="All"){
  #     plot_discrete(sys = sys(), param = param(), data = wq_data(), output = "table2")
  #   }
  # })
  # 
  # output$wq_table <- DT::renderDataTable({
  #   if(param()!="All"){
  #     plot_discrete(sys = sys(), param = param(), data = wq_data(), output = "table")
  #   }
  # })
  
  output$plotLinks <- renderUI({
    tagList(
      tags$h4("Submerged Aquatic Vegetation"),
      tags$h6("Trends and Visualizations"),
      tags$ul(
        tags$li(
          actionButton(paste0(sys(),"__multiplot"),
                       label = "Median percent cover",
                       onclick = 'Shiny.onInputChange("select_button", this.id);')
        ),
        tags$li(
          actionButton(paste0(sys(),"__trendplot"),
                       label = "Median percent cover trends",
                       onclick = 'Shiny.onInputChange("select_button", this.id);')
        ),
        tags$li(
          actionButton(paste0(sys(),"__barplot"),
                       label = "Frequency of Occurrence",
                       onclick = 'Shiny.onInputChange("select_button", this.id);')
        )
      )
    )
  })
  
  observeEvent(input$select_button, {
    sys <- str_split(input$select_button, "__")[[1]][[1]]
    type <- str_split(input$select_button, "__")[[1]][[2]]
    showPlot(sys, type)
  })
  
  output$downloadWQ <- downloadHandler(
    filename = function(){"Discrete_WQ_SKT_Stats.txt"},
    content = function(file){
      file.copy('data/Discrete_WQ_SKT_Stats.txt', file)
    }
  )
}

shinyApp(ui = ui, server = server)

# library(rsconnect)
# deployApp(appFiles = c("app.R", "UI_Snippets.R",
#                        "rds/data_combined.rds", "rds/publish_date.rds",
#                        "rds/skt_data_combined.rds", "rds/map_df.rds",
#                        "rds/sav_map_df.rds", "rds/sys_include.rds",
#                        "www/style.css", "www/dep-logos.png", "www/figures"))