## New data base for CH
# https://gadm.org/download_country_v3.html


library(raster)
library(leaflet)


ch <- readRDS(file = "gadm-swiss-maps/gadm36_CHE_1_sp.rds")
ch
head(ch)
ch
ch$NAME_1

## add numeric variable
ch$new <- rnorm(length(ch))

# 3) ----------------------------------------------------------------------

pal <- colorNumeric("viridis", NULL)

leaflet(ch) %>%
  # addTiles() %>%
  addPolygons(stroke = T, weight = 1, smoothFactor = 2, fillOpacity = 0.7,
              fillColor = ~pal(new))
