<!-- README.md is generated from README.Rmd. Please edit that file -->

[![picMaps status
badge](https://pifsc-protected-species-division.r-universe.dev/badges/picMaps)](https://pifsc-protected-species-division.r-universe.dev/picMaps)

# Installation

## R-Universe

#### Binary

``` r
install.packages('picMaps', 
                 repos=c('https://pifsc-protected-species-division.r-universe.dev','https://cloud.r-project.org')
)
```

#### Source

*You will need a C++ compiler for R*

``` r
install.packages('crawlUtils', type='source', 
                 repos=c('https://pifsc-protected-species-division.r-universe.dev','https://cloud.r-project.org')
)
```

## Github

*You will need a C++ compiler for R*

``` r
remotes::install_github('pifsc-protected-species-division/picMaps')
```

# Initial Use

#### Data storage

To use this package you will have to first specify a storage directory
for the downloaded data. This is accomplished in the following way

``` r
library(picMaps)
set_data_storage(path = "~/.picmaps_data")
```

The default directory created is `~/.picmaps_data`, but you are free to
specify whatever directory you want. If it does not exist it will be
created.

#### Data download

After the package is aware of the desired storage directory, you can
download OSM coastline polygon data from
<https://osmdata.openstreetmap.de/data/land-polygons.html> or ETOPO
bathymetry data from
<https://www.ncei.noaa.gov/products/etopo-global-relief-model>.

To download OSM coastline polygons run the following

``` r
library(picMaps)
osm_download(force = FALSE, clean_shp=TRUE)
```

If `force=TRUE` the data will be download even if `picMaps` detects the
data in the storage directory. When data is downloaded. The `.shp` files
will be imported to `R` and transformed to a searchable `.gpkg` file. If
`clean_shp=TRUE` the downloaded `.shp` files will be deleted.

To download the ETOPO bathymetry rasters, use the following

``` r
library(picMaps)
etopo_download(resolution = 60, force = FALSE)
```

The default resolution is 60 arcsecs, but 30 arcsecs can also
downloaded. Again, if the data are already present in the storage
directory you must set `force=TRUE` to re-download it.

# Regular Usage

Here is an example of data usage after download. We will use the data to
make a map near the main Hawaiian Islands. First we define a spatial
object, `x`, for we we would like the coastline polygons. All coastline
polygons that are at least partly within the bounding box will be
selected from the `.gpkg` file. The argument `keep=0.2` will keep 20% of
the polygon points. For large scale maps this is generally
adequate.`union=FALSE` will maintain separate single polygons for land.

``` r
library(sf)
#> Linking to GEOS 3.11.0, GDAL 3.5.3, PROJ 9.1.0; sf_use_s2() is TRUE
library(terra)
#> terra 1.7.71
library(crsuggest)
#> Using the EPSG Dataset v10.019, a product of the International Association of Oil & Gas Producers. 
#> Please view the terms of use at https://epsg.org/terms-of-use.html.
library(picMaps)
#> picMaps 0.1.9004 (April 19, 2024)

x <- st_bbox(c(xmin=198.5, xmax=206, ymin=18.5, ymax=23), crs=4326) |>
  st_as_sfc()
prj <- as.numeric(suggest_crs(x)$crs_code[[1]])
x <- st_transform(x, prj) |> st_as_sf()

mhi <- osm_coast(x, keep=0.2, union=FALSE)
#> Reading layer `osm_coast' from data source 
#>   `/Users/devin.johnson/.picmaps_data/osm_coast/osm_coast.gpkg' 
#>   using driver `GPKG'
#> Re-reading with feature count reset from 118 to 117
#> Simple feature collection with 117 features and 0 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -160.5453 ymin: 18.91069 xmax: -154.8067 ymax: 22.23542
#> Geodetic CRS:  WGS 84
```

Now we will obtain the associated bathysphere data

``` r
bathy <- etopo_rast(x)
bathy[bathy>0] <- NA
```

And finally, use them in a plot.

``` r
library(ggplot2)
library(ggplot2)
library(ggspatial)

tropic_ocean <- rev(c("#8aeed5","#63b9db","#4479e1","#2645e0","#071eed"))
        
ggplot() +
  layer_spatial(data=bathy, ) +
  scale_fill_gradientn(colours=tropic_ocean, name="Depth (m)", na.value = NA) +
  layer_spatial(data=mhi, fill="tan") +
  theme_bw()
#> Warning: Removed 9376 rows containing missing values or values outside the scale range
#> (`geom_raster()`).
```

![](README-ex_map-1.png)

# Disclaimer

*This software package is developed and maintained by scientists at the
NOAA Fisheries Pacific Islands Fisheries Science Center and should be
considered a fundamental research communication. The recommendations and
conclusions presented here are those of the authors and this software
should not be construed as official communication by NMFS, NOAA, or the
U.S. Dept. of Commerce. In addition, reference to trade names does not
imply endorsement by the National Marine Fisheries Service, NOAA. While
the best efforts have been made to insure the highest quality, tools
such as this are under constant development and are subject to change.*
