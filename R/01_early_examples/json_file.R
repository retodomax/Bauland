## Try to flip the y coordinate


# 1) import LV03 (CH1903) coordinates of municipals
## --> flip coordinate
# 2) trasform to WGS84
# 3) plot with leaflet


# 1) ----------------------------------------------------------------------

ch_cant <- geojsonio::geojson_read("swiss-maps/topo/ch-cantons.json",
                                   what = "sp")


library(sf)
library(leaflet)

ch_sf <- st_as_sf(ch_cant)

nr <- 1


for(nr in 1:26){
  for (i in 1:length(st_geometry(ch_sf)[[nr]])) {
    st_geometry(ch_sf)[[nr]][[i]][[1]][, 1] <- st_geometry(ch_sf)[[nr]][[i]][[1]][, 1]
    st_geometry(ch_sf)[[nr]][[i]][[1]][, 2] <- -st_geometry(ch_sf)[[nr]][[i]][[1]][, 2]
  }
}




ch_cant <-  as(ch_sf, 'Spatial')

# 2) ----------------------------------------------------------------------

library(sp)

ch_cant@proj4string <- CRS("+init=epsg:21781")    # CH1903 / LV03 seems to have this epsg code

ch_cant2 <- spTransform(ch_cant, CRS("+init=epsg:4326"))



# 3) ----------------------------------------------------------------------


leaflet(ch_cant2) %>%
  addTiles() %>% 
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.3)
