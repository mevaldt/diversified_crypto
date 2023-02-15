library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(dplyr)
library(leaflet)
available_countries = c("Brazil", "United States", "France", "Germany")

world <- ne_countries(scale = "medium", returnclass = "sf")


world_polygon <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  filter(name %in% available_countries) %>% 
  mutate(
    region = ifelse(region_un == 'Europe', 'Europe', name)
  ) %>% 
  group_by(region) %>% 
  mutate(geometry = st_union(geometry))


world_polygon %>% 
  leaflet() %>% 
  addTiles(options = leafletOptions(minZoom = 3)) %>%
  setView(lng = 0, lat = 31.32, zoom = 2) %>% 
  addPolygons(fillOpacity = 0,
              weight      = 0.75,
              color       = "#222222") %>% 
  addPolygons(data             = world_polygon,
              label            = ~region,
              layerId          = ~region,
              stroke           = T,
              color            = "#1772c2",
              weight           = 0.3,
              smoothFactor     = 0.1,
              fillOpacity      = 0.7,
              highlightOptions = highlightOptions(color        = "white",
                                                  weight       = 2,
                                                  bringToFront = TRUE)) %>%
  addProviderTiles(providers$CartoDB.Voyager)
