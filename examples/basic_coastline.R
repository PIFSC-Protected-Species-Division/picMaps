
library(sf)
library(mapview)
library(crsuggest)

library(picMaps)
# If this is the first time using {picMaps} you have to download the coastline data first
# See ?picMaps::set_data_storage() and ?picMaps::osm_download
# e.g.,
# set_data_storage("~/.picmaps_data") # You can use whatever path you want
# osm_download()


# Make some spatial data around Marianas
x <- st_bbox(c(xmin=144, xmax=146, ymin=13, ymax=16), crs=4326) %>%
  st_as_sfc()
prj <- as.numeric(suggest_crs(x)$crs_code[[1]])
x <- st_transform(x, prj) %>% st_as_sf()
mapview(x)

# Default specs
marianas <- osm_coast(x)
mapview(marianas)


# Make some spatial data around the main Hawaiian Islands
x <- st_bbox(c(xmin=198.5, xmax=206, ymin=18.5, ymax=23), crs=4326) %>%
  st_as_sfc()
prj <- as.numeric(suggest_crs(x)$crs_code[[1]])
x <- st_transform(x, prj) %>% st_as_sf()
mapview(x)

# Full resolution, single multipolygon data
mhi <- osm_coast(x, keep=1, union=TRUE)
mapview(mhi)
