#' @title Hawaiian Island EEZ boundary polygon
#' @importFrom sf read_sf
#' @export
hi_eez <- function(){
  x <- file.path(system.file(package="picMaps"), "inst", "hi_eez", "hi_eez.shp")
  read_sf(x)
}

#' @title Mariana Islands US EEZ boundary polygon
#' @importFrom sf read_sf
#' @export
mariana_eez <- function(){
  x <- file.path(system.file(package="picMaps"), "inst", "CNMIeez", "CNMIeez.shp")
  read_sf(x)
}

#' @title Papahānaumokuākea boundary polygon
#' @importFrom sf read_sf
#' @export
pmnm <- function(){
  x <- file.path(system.file(package="picMaps"), "inst", "pmnm", "pmnm.shp")
  read_sf(x)
}
