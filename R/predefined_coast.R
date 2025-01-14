#' @title Coast polygons for Main Hawaiian Islands
#' @param keep see \code{\link[picMaps]{osm_coast}}
#' @param union see \code{\link[picMaps]{osm_coast}}
#' @param crs A coordinate system object, See \code{\link[sf]{st_transform}}. Defaults to
#' ESPG = 6633, the official state projection.
#' @export
#' @importFrom sf st_bbox st_as_sfc
hawaii_coast <- function(keep=0.5, union=TRUE, crs=6633){
  x <- st_bbox(c(xmin=198.5, xmax=206, ymin=18.5, ymax=23), crs=4326) |>
    st_as_sfc() |> st_as_sf()
  x <- st_transform(x, crs=crs)
  y <- osm_coast(x, keep=keep, union=union)
  st_geometry(y) <- "geometry"
  return(y)
}

#' @title Coast polygons for the Mariana Islands
#' @param keep see \code{\link[picMaps]{osm_coast}}
#' @param union see \code{\link[picMaps]{osm_coast}}
#' @param crs A coordinate system object, See \code{\link[sf]{st_transform}}. Defaults to
#' ESPG = 6637, the Guam map grid projection (UTM zone 55N).
#' @export
#' @importFrom sf st_bbox st_as_sfc
mariana_coast <- function(keep=0.5, union=TRUE, crs=6637){
  x <- st_bbox(c(xmin=144, xmax=146.5, ymin=13, ymax=20.6), crs=4326) |>
    st_as_sfc() |> st_as_sf()
  x <- st_transform(x, crs=crs) |> st_as_sf()
  y <- osm_coast(x, keep=keep, union=union)
  st_geometry(y) <- "geometry"
  return(y)
}
