#' ---
#' project: Bauland    ####################################################
#' title:   swissBOUNDARIES
#' author:  "[Reto Zihlmann](http://www.n.ethz.ch/~retoz)"
#' date:    2019-10-17 11:01:17
#' output:  github_document   #############################################
#' ---


# packages ----------------------------------------------------------------

# devtools::install_git("https://github.com/retodomax/FunRZ")
library(magrittr, warn.conflicts = F); library(FunRZ);   library(tidyverse)
Sys.setlocale("LC_ALL", "English");                   Sys.setenv(LANG="EN")


# import shapefile --------------------------------------------------------

require(rgdal)
ch <- readOGR(dsn = "Data/SHAPEFILE_LV95_LN02", layer = "swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET")

plot(ch)


# transform coordinate system ---------------------------------------------

ch <- spTransform(ch, CRS("+init=epsg:4326"))

ch$ERSTELL_J


# plot --------------------------------------------------------------------

leaflet(ch) %>% 
  addTiles() %>% 
  addPolygons(color = "#444444", opacity = 1, stroke = T, weight = 1, smoothFactor = 2,
              fillOpacity = 0.7,
              fillColor = ~pal(EINWOHNERZ), 
              label = ~paste0(NAME, ": ", formatC(EINWOHNERZ)),
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = TRUE))
