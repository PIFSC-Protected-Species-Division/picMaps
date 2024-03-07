library(picMaps)
library(sf)
library(mapview)
library(crsuggest)
library(ggplot2)
library(ggspatial)

# If this is the first time using {picMaps} you have to download the coastline data first
# See ?picMaps::set_data_storage() and ?picMaps::etopo_download


# Make some spatial data around the main Hawaiian Islands
x <- st_bbox(c(xmin=198.5, xmax=206, ymin=18.5, ymax=23), crs=4326) %>%
  st_as_sfc()
prj <- as.numeric(suggest_crs(x)$crs_code[[1]])
x <- st_transform(x, prj) %>% st_as_sf()
mapview(x)

# Extract some coastline data for plotting
mhi <- osm_coast(x, 1, TRUE)
mhi_bb <- mhi %>% st_buffer(20000) %>% st_bbox()
mapview(mhi)

# Extract ETOPO bathymetry data
mhi_bathy <- etopo_rast(x)
mhi_bathy[mhi_bathy>10] <- NA
mapview(mhi_bathy)

### ggplot
ggplot() + layer_spatial(mhi_bathy) + layer_spatial(mhi, color=NA, fill=gray(.5)) +
  coord_sf(xlim=c(mhi_bb$xmin, mhi_bb$xmax), ylim=c(mhi_bb$ymin, mhi_bb$ymax), expand=FALSE) +
  theme_bw() + guides(fill=guide_legend(title="Depth (m)"))
