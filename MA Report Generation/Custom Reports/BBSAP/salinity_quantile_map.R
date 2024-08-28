gradientMap <- function(sys, type, param, n){
  # sysData <- wq_data[System==sys, ]
  riverData <- wq_data[System==sys & Type %in% type & ParameterName==param, ]
  
  units <- unique(riverData$ParameterUnits)
  
  pal <- colorQuantile(palette = "viridis", riverData$ResultValue, n=n)
  
  leaflet(riverData) %>%
    addTiles() %>%
    addCircleMarkers(~Longitude_, ~Latitude_D, color = ~pal(ResultValue), 
                     popup = paste(sep = "<br/>",
                                   "salinity: ",riverData$ResultValue),
                     fillOpacity = 0.2,
                     weight = 2) %>%
    addLegend(position = 'topright', pal = pal, value = riverData$ResultValue,
              labFormat = function(type, cuts, p){
                n <- length(cuts)
                p <- paste0(round(p * 100), '%')
                cuts <- paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]), " ", units)
                paste0(
                  '<span title="', p[-n], " - ", p[-1], '">', cuts,
                  '</span>'
                )
              })
  
}

gradientMap("Suwanee", "River", "Salinity", 4)
gradientMap("Suwanee", "Estuary", "Salinity", 4)
gradientMap("Suwanee", c("River","Estuary"), "Salinity", 10)


gradientMap("Suwanee", c("River","Estuary"), "Dissolved Oxygen", 10)







pal <- colorQuantile(palette = "viridis", riverData$ResultValue, n=4)

map <- leaflet(river) %>%
  addTiles() %>%
  addCircleMarkers(~Longitude_, ~Latitude_D, color = ~pal(ResultValue), 
                   popup = paste(sep = "<br/>",
                                 "salinity: ",river$ResultValue),
                   fillOpacity = 0.2,
                   weight = 2) %>%
  addLegend(position = 'topright', pal = pal, value = river$ResultValue,
            labFormat = function(type, cuts, p){
              n <- length(cuts)
              p <- paste0(round(p * 100), '%')
              cuts <- paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]), " ppt")
              paste0(
                '<span title="', p[-n], " - ", p[-1], '">', cuts,
                '</span>'
              )
            })

pal <- colorQuantile(palette = "plasma", suwanee$ResultValue, n=10)

map <- leaflet(suwanee) %>%
  addTiles() %>%
  addCircleMarkers(~Longitude_, ~Latitude_D, color = ~pal(ResultValue), 
                   popup = paste(sep = "<br/>",
                                 "salinity: ",suwanee$ResultValue),
                   fillOpacity = 0.2,
                   weight = 2) %>%
  addLegend(position = 'topright', pal = pal, value = suwanee$ResultValue,
            labFormat = function(type, cuts, p){
              n <- length(cuts)
              p <- paste0(round(p * 100), '%')
              cuts <- paste0(formatC(cuts[-n]), " - ", formatC(cuts[-1]), " ppt")
              paste0(
                '<span title="', p[-n], " - ", p[-1], '">', cuts,
                '</span>'
              )
            })
