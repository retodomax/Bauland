#' ---
#' project: Bauland    ####################################################
#' title:   ThemaKart
#' author:  "[Reto Zihlmann](http://www.n.ethz.ch/~retoz)"
#' date:    2019-10-17 12:21:02
#' output:  github_document   #############################################
#' ---


# packages ----------------------------------------------------------------

# devtools::install_git("https://github.com/retodomax/FunRZ")
library(magrittr, warn.conflicts = F); library(FunRZ);   library(tidyverse)
# Sys.setlocale("LC_ALL", "English");                   Sys.setenv(LANG="EN")

library(rgdal)
library(leaflet)

# import ThemaKart --------------------------------------------------------


ch_ch <- readOGR(dsn = "Data/01_Maps/ThemaKart/BASIS/01_INST/K4_suis18480101_gf",
              layer = "K4suis18480101gf_ch2007Poly") %>% 
  spTransform(CRS("+init=epsg:4326"))

ch_kt <- readOGR(dsn = "Data/01_Maps/ThemaKart/BASIS/01_INST/K4_kant19970101_gf",
                 layer = "K4kant19970101gf_ch2007Poly") %>% 
  spTransform(CRS("+init=epsg:4326"))

ch_bz <- readOGR(dsn = "Data/01_Maps/ThemaKart/BASIS/01_INST/K4_bezk20190101_gf",
                 layer = "K4bezk20190101gf_ch2007Poly") %>% 
  spTransform(CRS("+init=epsg:4326"))

ch_pg <- readOGR(dsn = "Data/01_Maps/ThemaKart/BASIS/01_INST/K4_polg20190101_gf",
              layer = "K4polg20190101gf_ch2007Poly") %>% 
  spTransform(CRS("+init=epsg:4326"))

ch_see <- readOGR(dsn = "Data/01_Maps/ThemaKart/BASIS/00_TOPO/K4_seenyyyymmdd",
                  layer = "k4seenyyyymmdd11_ch2007Poly") %>% 
  spTransform(CRS("+init=epsg:4326"))

ch_sees <- readOGR(dsn = "Data/01_Maps/ThemaKart/BASIS/00_TOPO/K4_seenyyyymmdd",
                   layer = "k4seenyyyymmdd22_ch2007Poly") %>% 
  spTransform(CRS("+init=epsg:4326"))



# import BFS nr -----------------------------------------------------------

bfs_nr_kt <- read_csv("Data/BFS_nr/kt.csv", locale = locale(encoding = "Latin1"))
bfs_nr_bz <- read_csv("Data/BFS_nr/bz.csv", locale = locale(encoding = "Latin1"))
bfs_nr_pg <- read_csv("Data/BFS_nr/pg.csv", locale = locale(encoding = "Latin1"))

# set number to numeric ---------------------------------------------------

ch_kt$ID0 <- as.numeric(as.character(ch_kt$ID0))
ch_bz$BEZIRKSNUM <- as.numeric(as.character(ch_bz$BEZIRKSNUM))
ch_pg$GDENR <- as.numeric(as.character(ch_pg$GDENR))



# order object to combine -------------------------------------------------

ch_order <- tibble(nr = 8100,
                   shape_ch_name = ch_ch$ID1,
                   bfs_name = "Schweiz")

kt_order <- tibble(nr = ch_kt$ID0,
            shape_kt_name = ch_kt$ID1) %>% 
  left_join(bfs_nr_kt, by = c("nr" = "kt_nr")) %>% 
  rename(bfs_name = kt_name)

bz_order <- tibble(nr = ch_bz$BEZIRKSNUM,
                   shape_bz_name = ch_bz$NAME) %>% 
  left_join(bfs_nr_bz, by = c("nr" = "bz_nr")) %>% 
  rename(bfs_name = bz_name)

pg_order <- tibble(nr = ch_pg$GDENR,
            shape_pg_name = ch_pg$GDENAME) %>% 
  left_join(bfs_nr_pg, by = c("nr" = "pg_nr")) %>% 
  rename(bfs_name = pg_name)



# add BFS name to maps ----------------------------------------------------

ch_ch$bfs_name <- ch_order$bfs_name
ch_kt$bfs_name <- kt_order$bfs_name
ch_bz$bfs_name <- bz_order$bfs_name
ch_pg$bfs_name <- pg_order$bfs_name

ch_ch$bfs_nr <- ch_order$nr
ch_kt$bfs_nr <- kt_order$nr
ch_bz$bfs_nr <- bz_order$nr
ch_pg$bfs_nr <- pg_order$nr

# save.image("Data/01_Maps/all_maps.RData")


# Combine with data -------------------------------------------------------

# # 1) import kt_data_table
# # 2) run:
# kt_data <- kt_order %>% 
#   left_join(kt_data_table)





# plot --------------------------------------------------------------------

pal <- colorNumeric("viridis", NULL)
leaflet() %>% 
  addPolygons(data = ch_kt, color = "#444444", opacity = 1, stroke = T, weight = 0.5, smoothFactor = 2,
              fillOpacity = 0.7,
              fillColor = ~pal(ID0), 
              label = ~paste0(ID1, ": ", formatC(ID0)),
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = TRUE)) %>% 
  addPolygons(data = ch_see,
              color = rgb(77,118,205, maxColorValue = 255), opacity = 1, stroke = T,
              weight = 1, smoothFactor = 1)

## empty schweiz
leaflet(options = leafletOptions(attributionControl = F,
                                 minZoom = 7.1,
                                 maxZoom = 12,
                                 maxBounds = list(c(48, 5.5),
                                                  c(45.5, 11)))) %>%
  addPolygons(data = ch_ch, color = "gray",
              opacity = 1, weight = 0.5, smoothFactor = 0.5,
              fillOpacity = 0.7) %>% 
  addPolygons(data = ch_see, color = rgb(77,118,205, maxColorValue = 255),
              opacity = 1, weight = 0.7, smoothFactor = 0.5)


## empty kanton
leaflet(options = leafletOptions(attributionControl = F,
                                 minZoom = 7.1,
                                 maxZoom = 12,
                                 maxBounds = list(c(48, 5.5),
                                                  c(45.5, 11)))) %>%
  addPolygons(data = ch_kt, color = "gray",
              opacity = 1, weight = 0.5, smoothFactor = 0.5,
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(color = "gray", weight = 1.5),
              label = ~paste0(kt_order$kt_name)) %>% 
  addPolygons(data = ch_see, color = rgb(77,118,205, maxColorValue = 255),
              opacity = 1, weight = 0.7, smoothFactor = 0.5)


## empty bezirk
leaflet(options = leafletOptions(attributionControl = F,
                                 minZoom = 7.1,
                                 maxZoom = 12,
                                 maxBounds = list(c(48, 5.5),
                                                  c(45.5, 11)))) %>%
  addPolygons(data = ch_bz, color = "gray",
              opacity = 1, weight = 0.3, smoothFactor = 0.5,
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(color = "gray", weight = 1.5),
              label = ~paste0(bz_order$bz_name)) %>% 
  addPolygons(data = ch_kt, color = "gray",
              opacity = 1, weight = 0.5, smoothFactor = 0.5,
              fill = F) %>% 
  addPolygons(data = ch_see, color = rgb(77,118,205, maxColorValue = 255),
              opacity = 1, weight = 0.7, smoothFactor = 0.5)


## empty gemeinde
leaflet(options = leafletOptions(attributionControl = F,
                                 minZoom = 7.1,
                                 maxZoom = 12,
                                 maxBounds = list(c(48, 5.5),
                                                  c(45.5, 11)))) %>%
  addPolygons(data = ch_pg, color = "gray",
              opacity = 1, weight = 0.3, smoothFactor = 0.5,
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(color = "gray", weight = 1.5),
              label = ~paste0(pg_order$pg_name)) %>% 
  addPolygons(data = ch_kt, color = "gray",
              opacity = 1, weight = 0.5, smoothFactor = 0.5,
              fill = F) %>% 
  addPolygons(data = ch_see, color = rgb(77,118,205, maxColorValue = 255),
              opacity = 1, weight = 0.7, smoothFactor = 0.5)

