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

########################
### State of HI data ###
########################

#' @title Oahu roads
#' @importFrom sf read_sf
#' @export
oahu_roads <- function(){
  x <- file.path(system.file(package="picMaps"), "inst", "hi_state", "Oahu_Roads","Oahu_Roads.shp")
  y <- read_sf(x)
  return(y)
}

#' @title Watershed zones out to 3mi
#' @importFrom sf read_sf
#' @export
hi_ridge_to_reef <- function(){
  x <- file.path(system.file(package="picMaps"), "inst", "hi_state", "Region_Ridge_to_Reef","Region_Ridge_to_Reef_(DAR).shp")
  y <- read_sf(x)
  return(y)
}

#' @title Watersheds
#' @param type DAR version (`type="DAR"`; default) or `type="regular"`.
#' @importFrom sf read_sf
#' @export
hi_watersheds <- function(type="DAR"){
  if(type=="regular"){
    x <- file.path(system.file(package="picMaps"), "inst", "hi_state", "Watersheds","Watersheds.shp")
  } else {
    x <- file.path(system.file(package="picMaps"), "inst", "hi_state", "Watersheds_(DAR)","Watersheds_(DAR_Version).shp")
  }
  y <- read_sf(x)
  return(y)
}

#' @title HI Official State Coastline
#' @importFrom sf read_sf
#' @export
hi_coast_state <- function(){
  x <- file.path(system.file(package="picMaps"), "inst", "hi_state", "Coastline","Coastline.shp")
  y <- read_sf(x)
  y <- y[y$water==0,]
  return(y)
}

#' @title Compile full layer of Main Hawaiian Island watersheds
#' @importFrom sf read_sf
#' @import dplyr
#' @export
hi_full_watersheds <- function(){
  objectid <- region_nam <- region_id <- hawn_name <- isle <- NULL
  hi <- hi_coast_state()
  hi <- hi[hi$water==0,]
  wsr <- hi_ridge_to_reef()
  ws <- hi_watersheds("DAR")
  n_ws <- nrow(ws)
  ws_exp <- st_intersection(ws,wsr)
  ws_exp$a <- st_area(ws_exp)
  ws_exp <- ws_exp[order(ws_exp$a, decreasing=T),]
  ws_exp <- ws_exp[1:n_ws,] |>
    select(objectid, region_nam, region_id, hawn_name) |>
    st_drop_geometry()
  ws <- full_join(ws, ws_exp)
  ws_exp <- st_intersection(ws,hi) |> select(objectid, isle) |> st_drop_geometry()
  ws <- full_join(ws, ws_exp)
  ## Add Kaho'olawe
  kahoolawe <- hi[hi$isle=="kahoolawe",]
  ws2 <- hi_watersheds("regular")[kahoolawe,]
  n_ws2 <- nrow(ws2)
  wsr2 <- wsr[ws2,]
  ws2_exp <- st_intersection(ws2, wsr2)
  ws2_exp$a <- st_area(ws2_exp)
  ws2_exp <- ws2_exp[order(ws2_exp$a, decreasing = T),][1:n_ws2,] |>
    select(objectid, region_nam, region_id, hawn_name) |>
    st_drop_geometry()
  ws2 <- full_join(ws2, ws2_exp)
  ws2$isle <- "Kahoolawe"
  ws2$objectid <- 2000 + ws2$objectid

  out <- bind_rows(ws, ws2)
}


#' @title Import benthic habitat data for the Main Hawaiian Islands
#' @importFrom sf read_sf
#' @import dplyr
#' @export
hi_benthic_habitat<- function(){
  x <- file.path(system.file(package="picMaps"), "inst", "hi_state", "Benthic_Habitat","Benthic_habitat.shp")
  y <- read_sf(x)
  y <- st_make_valid(y)
  return(y)
}

