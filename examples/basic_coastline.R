library(picMaps)
library(sf)
library(mapview)
library(crsuggest)

# osm_download(dir="~/research/projects/spatial_data/osm")

# Make some spatial data around Marianas
x <- st_bbox(c(xmin=144, xmax=146, ymin=13, ymax=16), crs=4326) |>
  st_as_sfc()
prj <- as.numeric(suggest_crs(x)$crs_code[[1]])
x <- st_transform(x, prj) |> st_as_sf()
mapview(x)

marianas <- osm_coast(x, keep=0.33, union=FALSE)
mapview(marianas)


# Make some spatial data around MHI
x <- st_bbox(c(xmin=198.5, xmax=206, ymin=18.5, ymax=23), crs=4326) |>
  st_as_sfc()
prj <- as.numeric(suggest_crs(x)$crs_code[[1]])
x <- st_transform(x, prj) |> st_as_sf()
mapview(x)

mhi <- osm_coast(x, keep=1, union=FALSE)
mapview(mhi)
