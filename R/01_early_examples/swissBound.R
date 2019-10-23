#' ---
#' project: Bauland    ####################################################
#' title:   swissBOUNDARIES3D
#' author:  "[Reto Zihlmann](http://www.n.ethz.ch/~retoz)"
#' date:    2019-10-17 09:39:47
#' output:  github_document   #############################################
#' ---


# packages ----------------------------------------------------------------

# devtools::install_git("https://github.com/retodomax/FunRZ")
library(magrittr, warn.conflicts = F); library(FunRZ);   library(tidyverse)
Sys.setlocale("LC_ALL", "English");                   Sys.setenv(LANG="EN")



require(rgdal)
ch <- readOGR(dsn = "Data/SHAPEFILE_LV95_LN02", layer = "swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET")

## plot as usually
plot(ch)

## plot with leaflet
pal <- colorNumeric("viridis", NULL)

leaflet(ch) %>%
  addTiles() %>%
  addPolygons()



# try to change the CRS ---------------------------------------------------

crs <- leafletCRS(crsClass = "L.Proj.CRS", code = "EPSG:2056",
                  resolutions = 1.5^(25:15))

crs2 <- leafletCRS(crsClass = "L.Proj.CRS", code = "EPSG:2056",
                       proj4def = "+proj=utm +zone=33 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
                       resolutions = 2^(13:-1), # 8192 down to 0.5
                       origin = c(0, 0)
)




proj4.defs("EPSG:2056","+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs");

crs3 <- leafletCRS(crsClass = "L.Proj.CRS", code = "EPSG:2056",
                   proj4def = "+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs",
                   origin = c(0,0),
                   resolutions = 1.5^(25:15))


crs3 <- leafletCRS(crsClass = "L.Proj.CRS", code = "EPSG:21781",
                   proj4def = "+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.4,15.1,405.3,0,0,0,0 +units=m +no_defs",
                   resolutions = 2^(13:-1))


leaflet(data = ch, options = leafletOptions(worldCopyJump = F, crs = crs3)) %>%
  # addTiles() %>%
  addPolygons()


# transform the swiss coordinates -----------------------------------------

class(ch)
st_trans

coordinates(ch)
ch@proj4string

ch2 <- spTransform(ch, CRS("+init=epsg:4326"))
plot(ch2)
ch2$NAME

leaflet(ch2) %>% 
  addTiles() %>% 
  addPolygons(color = "#444444", opacity = 1, stroke = T, weight = 1, smoothFactor = 2,
              fillOpacity = 0.7,
              fillColor = ~pal(EINWOHNERZ), 
              label = ~paste0(NAME, ": ", formatC(EINWOHNERZ)),
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = TRUE))

