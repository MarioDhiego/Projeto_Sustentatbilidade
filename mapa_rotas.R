

library(leaflet)
library(osmdata)
library(httr2)
library(curl)


# Coordenadas de Belém e Castanhal
belem_lat <- -1.455
belem_lng <- -48.302
castanhal_lat <- -1.297
castanhal_lng <- -47.922

# Baixar dados OSM para a região entre as duas cidades
bbox <- c(left = -48.5, bottom = -1.6, right = -47.8, top = -1.2)  # Limites da área de Belém a Castanhal

# Baixar dados da estrada dentro da área definida
osm_data <- opq(bbox = bbox) %>%
  add_osm_feature(key = "highway") %>%  # Filtrar apenas estradas
  osmdata_sf()

# Plotar a rota no mapa
leaflet(data = osm_data$osm_lines) %>%
  addTiles() %>%
  addPolylines(color = "blue", weight = 2) %>%
  addMarkers(lng = belem_lng, lat = belem_lat, popup = "Belém") %>%
  addMarkers(lng = castanhal_lng, lat = castanhal_lat, popup = "Castanhal")
