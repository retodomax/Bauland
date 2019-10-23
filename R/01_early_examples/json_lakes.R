library(leaflet)

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m  # Print the map



# try out swiss map

library(geojsonio)
library(rgdal)


ch_cant <- geojsonio::geojson_read("swiss-maps/topo/ch-cantons.json",
                                      what = "sp")
ch_cant_lake <- geojsonio::geojson_read("swiss-maps/topo/ch-cantons-lakes.json",
                                      what = "sp")
ch_mun <- geojsonio::geojson_read("swiss-maps/topo/ag-municipalities.json",
                                      what = "sp")
ch_lake <- geojsonio::geojson_read("swiss-maps/topo/ch-lakes.json",
                                      what = "sp")




nycounties <- geojsonio::geojson_read("swiss-maps/topo/ch-country.json",
                                      what = "sp")
leaflet(nycounties) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1)


nycounties <- geojsonio::geojson_read("swiss-maps/topo/sh-municipalities.json",
                                      what = "sp")
leaflet(nycounties) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1)


plot(nycounties)
plot(nycounties, add = T)

class(nycounties)
showDefault(nycounties)


plot(ch_cant, axes = T, las = 1)
abline(h = 200)
abline(v = 600)
plot(ch_lake, border = "blue", col = "yellow", lwd = 2, add = T)
plot(ch_lake, axes = T)

plot(pols, axes=TRUE, las=1)
plot(pols, border='blue', col='yellow', lwd=3, add=TRUE)






mylakes <- rgdal::readOGR("swiss-maps/topo/ch-cantons.json")
plot(mylakes, axes = T)

