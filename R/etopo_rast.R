#' @title Extract Bathymetry Data From Downloaded ETOPO 2022 data.
#' @param x an \code{sf} spatial object that will define the extent of the bathymetry raster.
#' @param resolution Resolution of the ETOPO bathymetry raster, either \code{60} or \code{30} arc-seconds. Defaults to \code{resolution = 60}.
#' @param proj Logical. Should the resulting raster be projected to the same CRS as \code{x}? Defaults to \code{project = TRUE}.
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
#' @importFrom ncdf4 nc_open nc_close ncvar_get
#' @importFrom terra ext rast values flip project crs
#' @export

etopo_rast <- function(x, resolution = 60, proj=TRUE, ...){
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

  on.exit(ncdf4::nc_close(rc))
  rc <- try(ncdf4::nc_open(filename))
  if (inherits(rc, "try-error")){
    print(rc)
    return(NULL)
  }

  bb <- x %>% st_transform(4326) %>% st_bbox(x) %>% {as.vector(.)[c(1,3,2,4)]}
  nav <- etopo_nc_nav(rc, bb = bb)
  M <- ncdf4::ncvar_get(rc, varid = nav$varname, start = nav$start, count = nav$count)
  R <- terra::rast(names = nav$varname, crs = nav$crs, ext = terra::ext(nav$ext), nrows = nav$count[2], ncols = nav$count[1])
  terra::values(R) <-  t(M)
  R <- terra::flip(R, "vertical")
  if(proj) R <- terra::project(x=R, y=terra::crs(terra::vect(x)), ...)
  return(R)
}


###
### Utility functions
###

etopo_nc_nav <- function(x,
                         bb = c(-161.5, -154.0, 18.5, 23.0),
                         res = etopo_nc_res(x),
                         varname = "z"){
  stopifnot(inherits(x, 'ncdf4'))
  if (!(varname[1] %in% names(x$var))) stop("varname not known:", varname[1])
  if (length(res) == 1) res <- c(res[1],res[1])
  r2 <- res/2
  # pad bb by res/2 so that we cast a large enough net
  bb2 <- bb + c(-r2[1], r2[1], -r2[2], r2[2])
  ix <- sapply(bb2[1:2],
               function(xbb) which.min(abs(x$dim$lon$vals-xbb)))
  we <- x$dim$lon$vals[ix]
  iy <- sapply(bb2[3:4],
               function(ybb) which.min(abs(x$dim$lat$vals-ybb)))
  sn <- x$dim$lat$vals[iy]

  list(bb = bb,
       res = res,
       start = c(ix[1], iy[1]),
       count = c(ix[2] - ix[1] + 1, iy[2] - iy[1] + 1),
       ext = c(we + (res[1]/2 * c(-1,1)), sn + (res[2]/2 * c(-1,1)) ),
       crs = "+proj=longlat +datum=WGS84",
       varname = varname)
}



etopo_nc_res <- function(x, lookup = TRUE){
  if (lookup){
    res <- c(diff(x$dim$lon$vals)[1], diff(x$dim$lat$vals)[1])
  } else {
    res <- c(0.0166666666666799, 0.0166666666666657)
  }
  res
}

