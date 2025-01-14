#' @title Hawaiian Island EEZ boundary polygon
#' @importFrom sf read_sf
#' @export
hawaii_eez <- function(){
  x <- file.path(system.file(package="picMaps"), "inst", "hi_eez", "hi_eez.shp")
  y <- read_sf(x) |> st_geometry(y) |> st_transform(4326) |> st_as_sf()
  st_geometry(y) <- "geometry"
  return(y)
}

#' @title Mariana Islands US EEZ boundary polygon
#' @importFrom sf read_sf
#' @export
mariana_eez <- function(){
  x <- file.path(system.file(package="picMaps"), "inst", "CNMIeez", "CNMIeez.shp")
  y <- read_sf(x) |> st_geometry(y) |> st_transform(4326) |> st_as_sf()
  st_geometry(y) <- "geometry"
  return(y)
}

#' @title Papahānaumokuākea boundary polygon
#' @importFrom sf read_sf
#' @export
papahanaumokuakea <- function(){
  x <- file.path(system.file(package="picMaps"), "inst", "pmnm", "pmnm.shp")
  y <- read_sf(x) |> st_geometry(y) |> st_transform(4326) |> st_as_sf()
  st_geometry(y) <- "geometry"
  return(y)
}


