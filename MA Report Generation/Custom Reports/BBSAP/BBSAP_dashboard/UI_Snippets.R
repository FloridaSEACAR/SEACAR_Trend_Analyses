publish_date <- readRDS("rds/publish_date.rds")
# Text to declare funding
funding_text <- paste(
  tags$div("Funding Acknowledgement", style="text-align:center; font-weight:bold;"),
  
  "SEACAR is funded, in part, through a grant agreement from the 
  Florida Department of Environmental Protection, Florida Coastal Management 
  Program, by a grant provided by the Office for Coastal Management 
  under the <a href='https://coast.noaa.gov/czm/act/' target='_blank'>
  Coastal Zone Management Act of 1972</a>, as amended, 
  National Oceanic and Atmospheric Administration. 
  The views, statements, findings, conclusions, and recommendations expressed 
  herein are those of the author(s) and do not necessarily reflect the views 
  of the State of Florida, NOAA or any of their subagencies.",
  
  tags$div(
    tags$img(
      src = "www/dep-logos.png",
      alt = "Funding logos",
      height = "100",
      width = "290"
    ),style="text-align:center;"
  ),
  
  tags$div(paste0("Published: ", publish_date), style="text-align:center;"),
  
  sep="<br><br>"
)

funding <- function(){
  wellPanel(
    tags$div(id="funding_footer",
             htmlOutput("funding"),
             tags$style(type="text/css","#funding_footer{width:100%;}")
             )
  )
}