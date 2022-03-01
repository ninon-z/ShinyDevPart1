library(highcharter)
library(tidytext)
library(readxl)
library(lubridate)
library(plyr)
library(shiny)
library(shinyjs)
library(highcharter)
library(dplyr)
library(purrr)
#library(geojsonio)
library(stringr)
library(argonR)
library(argonDash)
library(data.table)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(waiter)
#library(countup)
#library(NLP)
#library(twitteR)
#library(syuzhet)
library(tm)
library(SnowballC)
library(stringi)
library(topicmodels)
#library(ROAuth)
#library(wordcloud)
library(shinyBS)
library(deSolve)
library(waiter)
library(broom)
library(plotly)
library(reactable)
library(shinymanager)
library(ggmap)
library(readxl)
library(shinydashboard)





library(shiny)
library(highcharter)
#library(leaflet)
library(sp)
library(plotly)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
#library(ggmap)
#library(xts)
library(gifski)
library(av)
library(shinyjs)
library(jsonlite)
library(urltools)
library(utils)
#library(rvest)
library(stringr)
#library(rgeos)
#library(xml2)
library(selectr)
#library(raster)
#library(purrr)
library(RColorBrewer)
library(DT)
library(shinyBS)
library(shinybusy)
library(shinymanager)
library(data.table)
library(plyr)

## app.R ##

library(shinydashboard)

library(rintrojs)
library(shinyBS)
library(shinyWidgets)
library(shinycssloaders)
library(shinythemes)

library(shinydashboardPlus)
#library(RSQLite)
library(tidyverse)
library(forcats)
library(shinythemes) # layouts for shiny
library(ggplot2) #data visualization
library(sodium)
library(bslib)
library(waiter)
library(webshot) #to download plotly charts
library(reshape2)
library(tidyselect) #cleaning names
#library(viridis)
library(viridisLite)
library(forcats)

library(ggplotify)
library(extrafont)

#remotes::install_github("deepanshu88/summaryBox")
library(summaryBox)


# FLUID DESIGN FUNCTION ---------------------------------------------------

fluid_design <- function(id, w, x, y, z) {
  fluidRow(
    div(
      id = id,
      column(
        width = 6,
        uiOutput(w),
        uiOutput(y)
      ),
      column(
        width = 6,
        uiOutput(x),
        uiOutput(z)
      )
    )
  )
}

# table_options FUNCTION ---------------------------------------------------
table_options <- function() {
  list(
    dom = 'Bfrtip',
    #Bfrtip
    pageLength = 5,
    buttons = list(
      c('copy', 'csv', 'excel', 'pdf', 'print'),
      list(
        extend = "collection",
        text = 'Show All',
        action = DT::JS(
          "function ( e, dt, node, config ) {
          dt.page.len(-1);
          dt.ajax.reload();}"
        )
      ),
      list(
        extend = "collection",
        text = 'Show Less',
        action = DT::JS(
          "function ( e, dt, node, config ) {
          dt.page.len(5);
          dt.ajax.reload();}"
        )
      )
    ),
    deferRender = TRUE,
    lengthMenu = list(c(10, 20,-1), c('10', '20', 'All')),
    searching = FALSE,
    editable = TRUE,
    scroller = TRUE,
    scrollX = TRUE,
    lengthChange = FALSE ,
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': ' #596368', 'color': '#fff'});", ##5A5E6B
      "}"
    )
  )
}