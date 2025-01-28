#' @title Get A Coastline \code{sf} Polygon Object For Plotting and Mapping
#' @description Uses downloaded OSM data for constructing an \code{sf} polygon coastline data object.
#' Prior to using this function you must run \code{\link[picMaps]{osm_download}}.
#' @param x An \code{sf} spatial object. The coastline will be cropped to the bounding box.
#' @param keep The amount of data retained after simplification with \code{\link[rmapshaper]{ms_simplify}}
#' @param union Logical. Should the returned object be returned as a single \code{sf} \code{MULTIPOLYGON} object
#' @author Josh M. London and Devin S. Johnson
#' @import sf
#' @importFrom rmapshaper ms_simplify
#' @export
#'
osm_coast <- function(x, keep=0.2, union=FALSE) {
  f <- file.path(get_data_loc(), "osm_coast","osm_coast.gpkg")
  if(!file.exists(f)) stop("OSM data has not been previously downloaded.\nSee ?picMaps::osm_download")

  x_crs <- sf::st_crs(x)
  is_shifted <- st_is_longlat(x) & st_bbox(x)[3]>180
  x <- x %>%  st_geometry() %>%  st_transform(4326) %>% st_bbox() %>%  st_as_sfc() %>% st_wrap_dateline()
  x <- st_cast(x, "POLYGON")

  land <- NULL
  for(i in 1:length(x)){
    bboxWKT <- st_as_text(sf::st_geometry(x[i]))
    tmp <- st_read(dsn=f, wkt_filter=bboxWKT, quiet = TRUE)
    if(nrow(tmp)==0){
      next
    } else{
      land <- rbind(land, tmp)
    }
  }

  land <- st_transform(land, crs=x_crs)
  if(is_shifted) land <- st_shift_longitude(land)
  if(keep>0 & keep<1) land <- rmapshaper::ms_simplify(land, keep=keep)
  if(union) land <- st_geometry(land) |> st_union()

  land <- st_as_sf(land)

  return(land)
}


