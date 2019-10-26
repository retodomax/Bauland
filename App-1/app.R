library(shiny)
library(leaflet)




# preparation -------------------------------------------------------------

## Read in files from Bundesamt website

library(tidyverse)
library(magrittr)
library(readxl)
library(stringr)
library(leaflet)



# import age tables -------------------------------------------------------

load("../Data/canton_20to39.RData")
ch <- readRDS(file = "../Data/01_Maps/gadm-swiss-maps/gadm36_CHE_1_sp.rds")



# colors ------------------------------------------------------------------
pal <- colorNumeric("viridis", NULL)


# canton names ------------------------------------------------------------

cantlist <- as.list(ch$NAME_1)
names(cantlist) <- ch$NAME_1
cantlist
list(ch$NAME_1)



# shiny app ---------------------------------------------------------------


ui <- fluidPage(
  titlePanel("Generation Y Entwicklung"),
  sidebarLayout(
    sidebarPanel(
      # h3("Eingabe"),
      checkboxInput("tiles", "Map (Tiles)", value = F),
      selectInput("aufl", "Aufloesung", choices = list("Kanton" = 1,
                                                       "Gemeinde" = 2)),
      sliderInput("jahr", "Jahr",
                  min = 2010, max = 2030, value = 2018, step = 1, sep = "", ticks = F),
      # numericInput("jahr", "Jahr", value = 2018),
      selectInput("kanton", "Kanton", choices = cantlist)),
      mainPanel(
        leafletOutput("mymap"),
        plotOutput("trend")
      )
  )
)


server <- function(input, output, session){
  output$mymap <- renderLeaflet({
    ch$new <- cant_y %>% 
      filter(year == input$jahr) %$% 
      pop_y_pc
    leaflet(ch) %>%
      addPolygons(color = "#444444", opacity = 1, stroke = T, weight = 1, smoothFactor = 2,
                  fillOpacity = 0.7,
                  fillColor = ~pal(new), 
                  label = ~paste0(NAME_1, ": ", formatC(new)),
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = TRUE)) %>% 
      addLegend(pal = pal, values = ~new, opacity = 1, title = "Altersgruppe 20-39 [%]") %>% 
      {if(input$tiles) addTiles(.) else .}
  })
  
  output$trend <- renderPlot({
    # canton_name <- "Zürich"
    plotdata <- cant_y %>% 
      filter(NAME_1 == input$kanton) %>% 
      filter(year > 2008) %>% 
      arrange(year)
    
    plot(pop_y_pc ~ year, data = plotdata, type = "n",
         xlab = "Jahr", ylab = "Altersgruppe 20-39 [%]")
    abline(out_mod[[input$kanton]], col = rgb(220,220,220, maxColorValue = 255), lwd = 3)
    points(plotdata$year[plotdata$year < 2019], plotdata$pop_y_pc[plotdata$year < 2019], pch = 16)
    abline(v = input$jahr, col = rgb(73,136,202, maxColorValue = 255), lwd = 2)
  })
}

shinyApp(ui = ui, server = server)

