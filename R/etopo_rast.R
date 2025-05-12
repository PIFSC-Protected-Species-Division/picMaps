#' @title Extract Bathymetry Data From Downloaded ETOPO 2022 data.
#' @param x an \code{sf} spatial object that will define the extent of the bathymetry raster.
#' @param resolution Resolution of the ETOPO bathymetry raster, either \code{60} or \code{30} arc-seconds. Defaults to \code{resolution = 60}.
#' @param ... Addtional arguments passed to \code{terra::\link[terra]{project}}.
#' @description The majority of this function is taken from the \code{topotools} package here: \url{https://github.com/BigelowLab/topotools/tree/main}. It was modified to work within
#' the `picMaps` package.
#' @references
#' \itemize{
#' \item NOAA National Centers for Environmental Information. 2022:
#' ETOPO 2022 15 Arc-Second Global Relief Model. NOAA National Centers for
#' Environmental Information. DOI: 10.25921/fd45-gt74. Accessed 2023-03-17.
#' \item \url{https://github.com/BigelowLab/topotools/tree/main}
#' }
#' @importFrom terra ext rast values flip project crs rotate
#' @export

etopo_rast <- function(x, resolution = 60, ...){
  . <- NULL
  dir <- file.path(get_data_loc(), "etopo")
  if(resolution==60){
    f <- "ETOPO_2022_v1_60s_N90W180_bed.nc"
  } else if(resolution == 30){
    f <- "ETOPO_2022_v1_30s_N90W180_bed.nc"
  } else{
    stop("resolution must be either 60 or 30 arc-secs!")
  }
  filename <- file.path(dir,f)
  if(!file.exists(filename)) stop("ETOPO data does not seem to be downloaded for this resolution. See ?picMaps::set_data_storage() and ?picMaps::etopo_download()")

  bathy <- terra::rast(filename)
  if(is_longlat_360(x)) bathy <- terra::rotate(bathy, left=FALSE)
  bb <- x %>% st_transform(4326)
  bathy <- terra::crop(bathy, bb)
  return(bathy)
}


#'---------
#' Utility funcs
#'--------------

is_longlat_360 <- function(x){
  if(!st_is_longlat(x)) return(FALSE)
  b <- st_bbox(x)
  if(any(b[c(1,3)]>180)) return(TRUE)
}
