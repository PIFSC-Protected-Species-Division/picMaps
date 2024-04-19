#' @title Coast polygons for Main Hawaiian Islands
#' @param keep see \code{\link[picMaps]{osm_coast}}
#' @param union see \code{\link[picMaps]{osm_coast}}
#' @export
#' @importFrom sf st_bbox st_as_sfc
hawaii_coast <- function(keep=1, union=TRUE){
  x <- st_bbox(c(xmin=198.5, xmax=206, ymin=18.5, ymax=23), crs=4326) %>%
    st_as_sfc() %>% st_as_sf()
  osm_coast(x, keep=keep, union=union)
}
