#' @title Main Hawaiian Islands Region Boundary
#' @importFrom sf st_transform st_buffer st_convex_hull st_shift_longitude
#' @export
mhi_region <- function(){
  hi <- hawaii_coast(keep = 0.75) %>% st_transform(32604) %>% st_buffer(72000)
  out <- st_convex_hull(hi)
  out <- st_transform(out, 4326) %>% st_shift_longitude()
  return(out)
}


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
  y <- st_shift_longitude(y)
  st_geometry(y) <- "geometry"
  return(y)
}

#' @title Papahānaumokuākea boundary polygon
#' @importFrom sf read_sf
#' @export
papahanaumokuakea <- function(){
  x <- file.path(system.file(package="picMaps"), "inst", "pmnm", "pmnm.shp")
  y <- read_sf(x) |> st_geometry(y) |> st_transform(4326) |> st_as_sf()
  y <- st_shift_longitude(y)
  st_geometry(y) <- "geometry"
  return(y)
}

#' @title American Samoa US EEZ boundary polygon
#' @importFrom sf read_sf
#' @export
am_samoa_eez <- function(){
  x <- file.path(system.file(package="picMaps"), "inst", "american_samoa_eez", "american_samoa_eez.shp")
  y <- read_sf(x) |> st_geometry(y) |> st_transform(4326) |> st_as_sf()
  y <- st_shift_longitude(y)
  st_geometry(y) <- "geometry"
  return(y)
}

#' @title Howland and Baker Islands US EEZ boundary polygon
#' @importFrom sf read_sf
#' @export
howland_baker_eez <- function(){
  x <- file.path(system.file(package="picMaps"), "inst", "howland_baker_eez", "howland_baker_eez.shp")
  y <- read_sf(x) |> st_geometry(y) |> st_transform(4326) |> st_as_sf()
  y <- st_shift_longitude(y)
  st_geometry(y) <- "geometry"
  return(y)
}

#' @title Jarvis Islands US EEZ boundary polygon
#' @importFrom sf read_sf
#' @export
jarvis_eez <- function(){
  x <- file.path(system.file(package="picMaps"), "inst", "jarvis_eez", "jarvis_eez.shp")
  y <- read_sf(x) |> st_geometry(y) |> st_transform(4326) |> st_as_sf()
  y <- st_shift_longitude(y)
  st_geometry(y) <- "geometry"
  return(y)
}


#' @title Johnston Atoll US EEZ boundary polygon
#' @importFrom sf read_sf
#' @export
johnston_eez <- function(){
  x <- file.path(system.file(package="picMaps"), "inst", "johnston_eez", "johnston_eez.shp")
  y <- read_sf(x) |> st_geometry(y) |> st_transform(4326) |> st_as_sf()
  y <- st_shift_longitude(y)
  st_geometry(y) <- "geometry"
  return(y)
}


#' @title Palmyra Kingman US EEZ boundary polygon
#' @importFrom sf read_sf
#' @export
palmyra_kingman_eez <- function(){
  x <- file.path(system.file(package="picMaps"), "inst", "palmyra_kingman_eez", "palmyra_kingman_eez.shp")
  y <- read_sf(x) |> st_geometry(y) |> st_transform(4326) |> st_as_sf()
  y <- st_shift_longitude(y)
  st_geometry(y) <- "geometry"
  return(y)
}

#' @title Palmyra Kingman US EEZ boundary polygon
#' @importFrom sf read_sf
#' @export
wake_eez <- function(){
  x <- file.path(system.file(package="picMaps"), "inst", "wake_eez", "wake_eez.shp")
  y <- read_sf(x) |> st_geometry(y) |> st_transform(4326) |> st_as_sf()
  y <-suppressWarnings(st_shift_longitude(y))
  st_geometry(y) <- "geometry"
  return(y)
}


#' @title All Pacific Island US EEZ
#' @importFrom sf read_sf
#' @export
all_eez <- function(){
y <- vector("list", 6)
y[[1]] <- hawaii_eez()
y[[1]]$eez <- "Hawaii"
y[[2]] <- howland_baker_eez()
y[[2]]$eez <- "Howland_Baker"
y[[3]] <- jarvis_eez()
y[[3]]$eez <- "Jarvis"
y[[4]] <- palmyra_kingman_eez()
y[[4]]$eez <- "Palmyra_Kingman"
y[[5]] <- wake_eez()
y[[5]]$eez <- "Wake"
y[[6]] <- johnston_eez()
y[[6]]$eez <- "Johnston"

out <- do.call("rbind", y)
out <- st_as_sf(out)
out <- st_shift_longitude(out)
return(out)

}


################################################################################

#' @title NW Hawaiian Islands polygons
#' @importFrom sf read_sf
#' @export
nwhi <- function(){
  x <- file.path(system.file(package="picMaps"), "inst", "nwhi_coast", "Coastline.shp")
  y <- read_sf(x) |> st_geometry(y) |> st_transform(4326) |> st_as_sf()
  y <- st_shift_longitude(y)
  st_geometry(y) <- "geometry"
  return(y)
}

#' @title Pelagic False Killer Whale Management Area Polygon
#' @importFrom sf read_sf
#' @export
pfkw_mgmt <- function(){
  x <- file.path(system.file(package="picMaps"), "inst", "pelagicFKW", "pFKW_MgmtArea_line.shp")
  y <- read_sf(x) |> st_geometry(y) |> st_transform(4326) |> st_as_sf()
  y <- st_shift_longitude(y)
  st_geometry(y) <- "geometry"
  y <- st_cast(y, "POLYGON")
  return(y)
}


#' @title Main Hawaiian Islands Longline Exclusion Zone
#' @importFrom sf read_sf
#' @export
longline_exclusion <- function(){
  x <- file.path(system.file(package="picMaps"), "inst", "mhi_lez", "LonglineZonePoly.shp")
  y <- read_sf(x) |> st_geometry(y) |> st_transform(4326) |> st_as_sf()
  y <- st_shift_longitude(y)
  st_geometry(y) <- "geometry"
  y <- st_cast(y, "POLYGON")
  return(y)
}

#' @title Central Pacific Boundary for Pelagic False Killer Whales
#' @importFrom sf st_bbox st_as_sfc st_as_sf
#' @export
cenpac <- function(){
  cenpac <- c(175,0,228,40)
  names(cenpac) <- c("xmin","ymin","xmax","ymax")
  cenpac <- st_as_sfc(st_bbox(cenpac), crs=4326) %>% st_as_sf(crs=4326)
  cenpac
}

#' @title Lalo Islands
#' @importFrom sf read_sf
#' @export
lalo <- function(){
  x <- file.path(system.file(package="picMaps"), "inst", "lalo", "lalo.shp")
  y <- read_sf(x)
  y <- st_shift_longitude(y)
  return(y)
}
