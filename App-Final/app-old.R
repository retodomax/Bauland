
# packages ----------------------------------------------------------------
library(shiny)
library(tidyverse)
library(magrittr)
library(readxl)
library(stringr)
library(leaflet)


# data --------------------------------------------------------------------
data <- readRDS("../Data/02_Unif_Data/unif_data.RDS")
var_names <- read_csv("../Data/00_Raw_Data/var_names.csv") %>%
  fill(Kategorie, kat_short)
# data <- readRDS("Data/02_Unif_Data/unif_data.RDS")
# var_names <- read_csv("Data/00_Raw_Data/var_names.csv") %>%
#   fill(Kategorie, kat_short)

##### for correct ui selection values
data_selection <- split(var_names$var_short, seq(length(var_names$var_short))) %>% 
  setNames(var_names$Variable) %>% 
  split(factor(var_names$Kategorie,
               levels = unique(var_names$Kategorie)))

reso_order <- tibble(reso = c("Schweiz",
                              "Kanton",
                              "Bezirk",
                              "Gemeinde"),
                     order = 1:4)
selection_table <- data %>% 
  group_by(target, reso) %>% 
  summarise(min_year = min(year, na.rm = T),
            max_year = max(year, na.rm = T)) %>% 
  left_join(reso_order) %>% 
  arrange(order)

# map ---------------------------------------------------------------------

load("../Data/01_Maps/all_maps.RData")
myleaf <- leaflet(options = leafletOptions(attributionControl = F,
                                           minZoom = 7.1,
                                           maxZoom = 12,
                                           maxBounds = list(c(48, 5.5),
                                                            c(45.5, 11))))
pal <- colorNumeric("viridis", NULL)


# shiny app ---------------------------------------------------------------


ui <- fluidPage(
  titlePanel("Generation Y"),
  sidebarLayout(
    
    sidebarPanel(
      # h3("Eingabe"),
      selectizeInput("target", "Daten", choices = data_selection,
                     options = list(placeholder = "Wählen Sie ein Datenset",
                                    onInitialize = I('function() { this.setValue(""); }'))),
      uiOutput("secondSelection"),
      uiOutput("thirdSelection"),
      checkboxInput("tiles", "Hintergrundkarte", value = F)
      ),
    
      mainPanel(
        leafletOutput("mymap")
      )
  )
)


server <- function(input, output, session){
  
  
  
  
  output$secondSelection <- renderUI({
    if(nchar(input$target) == 0){
      selectInput("reso", "Auflösung",
                  choices =       c("Schweiz",
                                    "Kanton",
                                    "Bezirk",
                                    "Gemeinde"))
    } else {
      auswahl <- selection_table$target == input$target
      selectInput("reso", "Auflösung",
                  choices = selection_table[["reso"]][auswahl],
                  selected = isolate(input$reso)
                  )
    }
  })
  

  
  output$thirdSelection <- renderUI({
    if(nchar(input$target) == 0){
      sliderInput("jahr", "Jahr",
                  min = 2018,
                  max = 2018,
                  value = 2018, step = 1, sep = "", ticks = F)
    } else {
      auswahl <- selection_table$target == input$target &
        selection_table$reso == input$reso
      sliderInput("year", "Jahr",
                  min = selection_table[["min_year"]][auswahl],
                  max = selection_table[["max_year"]][auswahl],
                  value = isolate(input$jahr), step = 1, sep = "", ticks = F)
    }
    })
  
  
  output$mymap <- renderLeaflet({
    # mydat <- data %>%
    #   filter(reso == input$reso) %>%
    #   filter(target == input$target) %>%
    #   filter(year == input$year)
    # mydat <- data %>% 
    #   filter(year == input$year) %>% 
    #   filter(reso == "Kanton") %>% 
    #   filter(target == "geb_ant_geb")

    if(!is_empty(input$year)){
      mydat <- data %>%
        filter(target == input$target,
               reso == input$reso,
               year == input$year)
    }
    

    print(input$target)
    print(input$reso)
    print(input$year)
    
    if(!is_empty(input$reso)){
      if(is_empty(input$year)){   ##### without Dataset ########################
        if(input$reso == "Schweiz"){
          myleaf  %>%
            addPolygons(data = ch_ch, color = "gray",
                        opacity = 1, weight = 0.5, smoothFactor = 0.5,
                        fillOpacity = 0.7) %>% 
            addPolygons(data = ch_see, color = rgb(77,118,205, maxColorValue = 255),
                        opacity = 1, weight = 0.7, smoothFactor = 0.5) %>% 
            {if(input$tiles) addTiles(.) else .}
        } else if(input$reso == "Kanton"){
          myleaf %>% 
            addPolygons(data = ch_kt, color = "gray",
                        opacity = 1, weight = 0.5, smoothFactor = 0.5,
                        fillOpacity = 0.7,
                        highlightOptions = highlightOptions(color = "gray", weight = 1.5),
                        label = ~paste0(kt_order$kt_name)) %>% 
            addPolygons(data = ch_see, color = rgb(77,118,205, maxColorValue = 255),
                        opacity = 1, weight = 0.7, smoothFactor = 0.5) %>% 
            {if(input$tiles) addTiles(.) else .}
        } else if(input$reso == "Bezirk"){
          myleaf %>% 
            addPolygons(data = ch_bz, color = "gray",
                        opacity = 1, weight = 0.3, smoothFactor = 0.5,
                        fillOpacity = 0.7,
                        highlightOptions = highlightOptions(color = "gray", weight = 1.5),
                        label = ~paste0(bz_order$bz_name)) %>% 
            addPolygons(data = ch_kt, color = "gray",
                        opacity = 1, weight = 0.5, smoothFactor = 0.5,
                        fill = F) %>% 
            addPolygons(data = ch_see, color = rgb(77,118,205, maxColorValue = 255),
                        opacity = 1, weight = 0.7, smoothFactor = 0.5) %>% 
            {if(input$tiles) addTiles(.) else .}
          
        } else if(input$reso == "Gemeinde"){
          myleaf %>% 
            addPolygons(data = ch_pg, color = "gray",
                        opacity = 1, weight = 0.3, smoothFactor = 0.5,
                        fillOpacity = 0.7,
                        highlightOptions = highlightOptions(color = "gray", weight = 1.5),
                        label = ~paste0(pg_order$pg_name)) %>% 
            addPolygons(data = ch_kt, color = "gray",
                        opacity = 1, weight = 0.5, smoothFactor = 0.5,
                        fill = F) %>% 
            addPolygons(data = ch_see, color = rgb(77,118,205, maxColorValue = 255),
                        opacity = 1, weight = 0.7, smoothFactor = 0.5) %>% 
            {if(input$tiles) addTiles(.) else .}
            
          
        }
      } else {   ##### with Dataset ########################
        if(input$reso == "Schweiz"){

          
          
        } else if(input$reso == "Kanton"){
          ch_kt$value <- mydat %>% 
            right_join(kt_order, by = c("nr" = "kt_nr")) %$% 
            value
          myleaf %>% 
            addPolygons(data = ch_kt, color = "gray",
                        opacity = 1, weight = 0.5, smoothFactor = 0.5,
                        fillOpacity = 0.7,
                        highlightOptions = highlightOptions(color = "gray", weight = 1.5),
                        fillColor = ~pal(value),
                        label = ~paste0(kt_order$kt_name, ": ", formatC(value))) %>% 
            addPolygons(data = ch_see, color = rgb(77,118,205, maxColorValue = 255),
                        opacity = 1, weight = 0.7, smoothFactor = 0.5) %>% 
                        {if(input$tiles) addTiles(.) else .}
          
        } else if(input$reso == "Bezirk"){
          
        } else if(input$reso == "Gemeinde"){
          
        }
      }
    }
  })
}

shinyApp(ui = ui, server = server)

