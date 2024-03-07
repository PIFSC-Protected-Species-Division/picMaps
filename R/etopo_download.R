#' @title Download ETOPO 2022 Bathymetery data
#' @description Downloads bathymetery data for map making.
#' @param resolution numeric resolution in arcsecs (one of 30, or 60). Defaults to 60 arcsecs.
#' @param force Logical. If data has previously been downloaded, it will force a new
#' download and update of the OSM data.
#' @references https://osmdata.openstreetmap.de/data/land-polygons.html
#' @author Devin S. Johnson
#' @importFrom curl multi_download
#' @export


etopo_download <- function(resolution = 60, force = FALSE) {

  dir <- file.path(get_data_loc(), "etopo")
  f <- paste(c("ETOPO_2022_v1_", resolution, "s_N90W180_bed.nc"), collapse="")
  if(file.exists(file.path(dir,f)) & !force) {
    stop("ETOPO data with specified resolution has already been downloaded. Use 'force=TRUE' to update.")
  } else {
    if (resolution == 30) {
      etopo_url <- "https://www.ngdc.noaa.gov/thredds/fileServer/global/ETOPO2022/30s/30s_bed_elev_netcdf/ETOPO_2022_v1_30s_N90W180_bed.nc"
    } else if (resolution == 60) {
      etopo_url <- "https://www.ngdc.noaa.gov/thredds/fileServer/global/ETOPO2022/60s/60s_bed_elev_netcdf/ETOPO_2022_v1_60s_N90W180_bed.nc"
    } else {
      stop("resolution should be one of 30 or 60 arc-secs")
    }
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    # download the files
    res <- curl::multi_download(etopo_url, destfile = file.path(dir, f))
    if (!res$success) {
      warning("the download failed!")
    } else {
      message("download successul; access the relief data with `picMaps::etopo_rast()`")
    }
  }
}

