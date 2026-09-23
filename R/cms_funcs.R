#' @title Download Copernicus Marine Service physical oceanography data (surface values)
#' @param region An `sf` or `bbox` object. The bounding box will be used as the bounds for CMS
#' data subsetting and download
#' @param variable Variables from the GLOBAL_MULTIYEAR_PHY_001_030 product for download.
#' @param layer One of `"daily"` or `"monthly"` for either temporal resolution.
#' @param timerange A 2 vector of start and end dates. Both must be POSIX.
#' @param return_type One of `"stars"` or `"raster"`. A `stars` object is the default.
#' @param progress Output process of download.
#' @param asset See `?CopernicusMarine::cms_download_subset`
#' @param username Copernicus Marine Service account username,
#' see `?CopernicusMarine::cms_download_subset` and `?CopernicusMarine::cms_get_username`
#' @param password Copernicus Marine Service password,
#' see `?CopernicusMarine::cms_download_subset` and `?CopernicusMarine::cms_get_password`
#' @import CopernicusMarine stars sf
#' @rawNamespace import(terra, except = c(intersect, union, tail))
#' @importFrom abind adrop
#' @author Devin S. Johnson
#' @export
fetch_cms_phys_surface <- function(
    region,
    variable,
    layer,
    timerange,
    return_type = "raster",
    progress = FALSE,
    asset = "ARCO",
    username = NULL,
    password = NULL
){
  if(is.null(username)) username <- cms_get_username()
  if(is.null(password)) password <- cms_get_password()
  vars_2d <- variable[variable %in% c("zos","mlotst","bottomT","siconc","sithick","usi","vsi")]
  vars_3d <- variable[variable %in% c("thetao","so","uo","vo")]
  if(length(vars_2d)>0){
    message("Downloading 2D CMS variables...")
    cms_data_2d <- picMaps::fetch_cms_phys_2d(
      region = region,
      variable = vars_2d,
      layer = layer,
      timerange = timerange,
      return_type = return_type,
      progress = progress,
      asset = asset,
      username = username,
      password = password)
  } else{
    cms_data_2d <- NULL
  }
  if(length(vars_3d)>0){
    message("Downloading 3D CMS variables...")
    cms_data_3d <- picMaps::fetch_cms_phys_3d(
      region = region,
      variable = vars_3d,
      layer = layer,
      timerange = timerange,
      return_type = return_type,
      progress = progress,
      asset = asset,
      username = username,
      password = password)
  } else{
    cms_data_3d <- NULL
  }
cms_data <- c(cms_data_2d, cms_data_3d)
return(cms_data)
}



#' @title Download Copernicus Marine Service physical oceanography data (3d variables)
#' @param region An `sf` or `bbox` object. The bounding box will be used as the bounds for CMS
#' data subsetting and download
#' @param variable Variables from the GLOBAL_MULTIYEAR_PHY_001_030 product for download. Must be
#' any subset of `c("thetao","so","uo","vo")` any other variables will be ignored.
#' @param layer One of `"daily"` or `"monthly"` for either temporal resolution.
#' @param timerange A 2 vector of start and end dates. Both must be POSIX.
#' @param verticalrange Range of depth values to download, e.g., `c(0,-100)` will download
#' all CMS depth layers between 0-100m. Default is `0` for surface layer.
#' @param return_type One of `"stars"` or `"raster"`. A `stars` object is the default.
#' @param progress Output process of download.
#' @param asset See `?CopernicusMarine::cms_download_subset`
#' @param username Copernicus Marine Service account username,
#' see `?CopernicusMarine::cms_download_subset` and `?CopernicusMarine::cms_get_username`
#' @param password Copernicus Marine Service password,
#' see `?CopernicusMarine::cms_download_subset` and `?CopernicusMarine::cms_get_password`
#' @import CopernicusMarine stars sf
#' @rawNamespace import(terra, except = c(intersect, union, tail))
#' @importFrom abind adrop
#' @author Devin S. Johnson
#' @export
fetch_cms_phys_3d <- function(
    region,
    variable,
    layer,
    timerange,
    verticalrange = 0,
    return_type = "raster",
    progress = FALSE,
    asset = "ARCO",
    username = NULL,
    password = NULL
){
  if(is.null(username)) username <- cms_get_username()
  if(is.null(password)) password <- cms_get_password()
  vars <- variable[variable %in% c("thetao","so","uo","vo")]
  if(length(vars)==0){
    message("There are no 3d variables to download...")
    return(NULL)
  }

  timerange <- strftime(timerange, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  region <- st_transform(region, 4326) |> st_bbox() |> st_as_sfc()
  overlap_dateline <- st_overlap_dateline(region)

  # cms_lon <- seq(-180, 180-(1/12), 1/12)
  # cms_lat <- seq(-80, 90-(1/12), 1/12)

  if(layer=="daily"){
    layer <-  "cmems_mod_glo_phy_my_0.083deg_P1D-m"
  } else if(layer=="monthly"){
    layer <- "cmems_mod_glo_phy_my_0.083deg_P1M-m"
  } else{
    stop("Unrecognized `layer` specification!")
  }

  if(overlap_dateline){ # region crosses dateline
    region <- st_shift_longitude(region)
    bb_e <- bb_w <- st_bbox(region)
    bb_e['xmax'] <- 180 - 1/12
    bb_w['xmin'] <- -180
    bb_w['xmax'] <- bb_w['xmax'] - 360
    message("Downloading CMS data west of dateline...")
    cmsd_e <- cms_download_subset(
      product   = "GLOBAL_MULTIYEAR_PHY_001_030",
      layer     = layer,
      variable  = vars,
      region    = bb_e,
      timerange = timerange,
      verticalrange = verticalrange,
      asset = asset,
      progress  = progress,
      username = username,
      password = password
    ) |> adrop()
    attributes(cmsd_e)$dimensions$longitude$offset <- min(st_get_dimension_values(cmsd_e, "longitude")) - 1/24
    attributes(cmsd_e)$dimensions$longitude$delta <- 1/12
    attributes(cmsd_e)$dimensions$longitude["values"] <- list(NULL)
    attributes(cmsd_e)$dimensions$latitude$offset <- min(st_get_dimension_values(cmsd_e, "latitude")) -1/24
    attributes(cmsd_e)$dimensions$latitude$delta <- 1/12
    attributes(cmsd_e)$dimensions$latitude["values"] <- list(NULL)
    nms <- names(cmsd_e)
    cmsd_e <- lapply(nms, function(var) {
      r <- rast(cmsd_e[var])
      time(r) <- st_get_dimension_values(cmsd_e,"time")
      return(r)
    })
    names(cmsd_e) <- nms
    r_cmsd_e <- sds(cmsd_e)
    message("Downloading CMS data east of dateline...")
    cmsd_w <- cms_download_subset(
      product   = "GLOBAL_MULTIYEAR_PHY_001_030",
      layer     = layer,
      variable  = vars,
      region    = bb_w,
      timerange = timerange,
      verticalrange = verticalrange,
      asset = asset,
      progress  = progress,
      username = username,
      password = password
    ) |> adrop()
    attributes(cmsd_w)$dimensions$longitude$offset <- min(st_get_dimension_values(cmsd_w, "longitude"))-1/24
    attributes(cmsd_w)$dimensions$longitude$delta <- 1/12
    attributes(cmsd_w)$dimensions$longitude["values"] <- list(NULL)
    attributes(cmsd_w)$dimensions$latitude$offset <- min(st_get_dimension_values(cmsd_w, "latitude")) -1/24
    attributes(cmsd_w)$dimensions$latitude$delta <- 1/12
    attributes(cmsd_w)$dimensions$latitude["values"] <- list(NULL)
    cmsd_w <- lapply(nms, function(var) {
      r <- rast(cmsd_w[var])|> rotate()
      time(r) <- st_get_dimension_values(cmsd_w,"time")
      return(r)
    })
    names(cmsd_w) <- nms
    r_cmsd_w <- sds(cmsd_w)

    r <- lapply(nms, \(var) merge(r_cmsd_e[var], r_cmsd_w[var]))
    names(r) <- nms
    r <- sds(r)
    if(return_type=="stars"){
      r <- lapply(r, \(x) st_as_stars(x))
      r <- do.call(c, r)
      names(r) <- nms
    }
    return(r)
  }
  else{ # Region doesn't cross dateline
    bb <- st_bbox(region)
    if(bb['xmin']>180) bb['xmin'] <- bb['xmin'] - 360
    if(bb['xmax']>180) bb['xmax'] <- bb['xmax'] - 360
    message("Downloading CMS data...")
    cmsd <- cms_download_subset(
      product   = "GLOBAL_MULTIYEAR_PHY_001_030",
      layer     = "cmems_mod_glo_phy_my_0.083deg_P1D-m",
      variable  = vars,
      region    = bb,
      timerange = timerange,
      verticalrange = verticalrange,
      asset = asset,
      progress  = progress,
      username = username,
      password = password
    ) |> adrop()
    attributes(cmsd)$dimensions$longitude$offset <- min(st_get_dimension_values(cmsd, "longitude")) - 1/24
    attributes(cmsd)$dimensions$longitude$delta <- 1/12
    attributes(cmsd)$dimensions$longitude["values"] <- list(NULL)
    attributes(cmsd)$dimensions$latitude$offset <- min(st_get_dimension_values(cmsd, "latitude")) -1/24
    attributes(cmsd)$dimensions$latitude$delta <- 1/12
    attributes(cmsd)$dimensions$latitude["values"] <- list(NULL)
    nms <- names(cmsd)
    cmsd <- lapply(nms, function(var) {
      r <- rast(cmsd[var])
      time(r) <- st_get_dimension_values(cmsd,"time")
      return(r)
    })
    names(cmsd) <- nms
    r_cmsd <- sds(cmsd)
    if(return_type=="stars"){
      r <- lapply(nms, \(var) st_as_stars(r_cmsd[var]))
      r <- do.call(c, r)
      names(r) <- nms
      return(r)
    } else{
      return(r_cmsd)
    }
  }
}


#' @title Download Copernicus Marine Service physical oceanography data (2d variables)
#' @param region An `sf` or `bbox` object. The bounding box will be used as the bounds for CMS
#' data subsetting and download
#' @param variable Variables from the GLOBAL_MULTIYEAR_PHY_001_030 product for download. Must be
#' any subset of `c("thetao","so","uo","vo")` any other variables will be ignored.
#' @param layer One of `"daily"` or `"monthly"` for either temporal resolution.
#' @param timerange A 2 vector of start and end dates. Both must be POSIX.
#' @param return_type One of `"stars"` or `"raster"`. A `stars` object is the default.
#' @param progress Output process of download.
#' @param asset See `?CopernicusMarine::cms_download_subset`
#' @param username Copernicus Marine Service account username,
#' see `?CopernicusMarine::cms_download_subset` and `?CopernicusMarine::cms_get_username`
#' @param password Copernicus Marine Service password,
#' see `?CopernicusMarine::cms_download_subset` and `?CopernicusMarine::cms_get_password`
#' @import CopernicusMarine stars sf
#' @rawNamespace import(terra, except = c(intersect, union, tail))
#' @importFrom abind adrop
#' @author Devin S. Johnson
#' @export
fetch_cms_phys_2d <- function(
    region,
    variable,
    layer,
    timerange,
    return_type = "raster",
    progress = FALSE,
    asset = "ARCO",
    username = NULL,
    password = NULL
){
  if(is.null(username)) username <- cms_get_username()
  if(is.null(password)) password <- cms_get_password()
  vars <- variable[variable %in% c("zos","mlotst","bottomT","siconc","sithick","usi","vsi")]

  if(length(vars)==0){
    message("There are no 2d variables to download...")
    return(NULL)
  }

  timerange <- strftime(timerange, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  region <- st_transform(region, 4326) |> st_bbox() |> st_as_sfc()
  overlap_dateline <- st_overlap_dateline(region)

  # cms_lon <- seq(-180, 180-(1/12), 1/12)
  # cms_lat <- seq(-80, 90-(1/12), 1/12)

  if(layer=="daily"){
    layer <-  "cmems_mod_glo_phy_my_0.083deg_P1D-m"
  } else if(layer=="monthly"){
    layer <- "cmems_mod_glo_phy_my_0.083deg_P1M-m"
  } else{
    stop("Unrecognized `layer` specification!")
  }

  if(overlap_dateline){ # region crosses dateline
    region <- st_shift_longitude(region)
    bb_e <- bb_w <- st_bbox(region)
    bb_e['xmax'] <- 180 - 1/12
    bb_w['xmin'] <- -180
    bb_w['xmax'] <- bb_w['xmax'] - 360
    message("Downloading CMS data west of dateline...")
    cmsd_e <- cms_download_subset(
      product   = "GLOBAL_MULTIYEAR_PHY_001_030",
      layer     = layer,
      variable  = vars,
      region    = bb_e,
      timerange = timerange,
      asset = asset,
      progress  = progress,
      username = username,
      password = password
    ) |> adrop()
    attributes(cmsd_e)$dimensions$longitude$offset <- min(st_get_dimension_values(cmsd_e, "longitude")) - 1/24
    attributes(cmsd_e)$dimensions$longitude$delta <- 1/12
    attributes(cmsd_e)$dimensions$longitude["values"] <- list(NULL)
    attributes(cmsd_e)$dimensions$latitude$offset <- min(st_get_dimension_values(cmsd_e, "latitude")) -1/24
    attributes(cmsd_e)$dimensions$latitude$delta <- 1/12
    attributes(cmsd_e)$dimensions$latitude["values"] <- list(NULL)
    nms <- names(cmsd_e)
    cmsd_e <- lapply(nms, function(var) {
      r <- rast(cmsd_e[var])
      time(r) <- st_get_dimension_values(cmsd_e,"time")
      return(r)
    })
    names(cmsd_e) <- nms
    r_cmsd_e <- sds(cmsd_e)
    message("Downloading CMS data east of dateline...")
    cmsd_w <- cms_download_subset(
      product   = "GLOBAL_MULTIYEAR_PHY_001_030",
      layer     = layer,
      variable  = vars,
      region    = bb_w,
      timerange = timerange,
      asset = asset,
      progress  = progress,
      username = username,
      password = password
    ) |> adrop()
    attributes(cmsd_w)$dimensions$longitude$offset <- min(st_get_dimension_values(cmsd_w, "longitude"))-1/24
    attributes(cmsd_w)$dimensions$longitude$delta <- 1/12
    attributes(cmsd_w)$dimensions$longitude["values"] <- list(NULL)
    attributes(cmsd_w)$dimensions$latitude$offset <- min(st_get_dimension_values(cmsd_w, "latitude")) -1/24
    attributes(cmsd_w)$dimensions$latitude$delta <- 1/12
    attributes(cmsd_w)$dimensions$latitude["values"] <- list(NULL)
    cmsd_w <- lapply(nms, function(var) {
      r <- rast(cmsd_w[var])|> rotate()
      time(r) <- st_get_dimension_values(cmsd_w,"time")
      return(r)
    })
    names(cmsd_w) <- nms
    r_cmsd_w <- sds(cmsd_w)

    r <- lapply(nms, \(var) merge(r_cmsd_e[var], r_cmsd_w[var]))
    names(r) <- nms
    r <- sds(r)
    if(return_type=="stars"){
      r <- lapply(r, \(x) st_as_stars(x))
      r <- do.call(c, r)
      names(r) <- nms
    }
    return(r)
  }
  else{ # Region doesn't cross dateline
    bb <- st_bbox(region)
    if(bb['xmin']>180) bb['xmin'] <- bb['xmin'] - 360
    if(bb['xmax']>180) bb['xmax'] <- bb['xmax'] - 360
    message("Downloading CMS data...")
    cmsd <- cms_download_subset(
      product   = "GLOBAL_MULTIYEAR_PHY_001_030",
      layer     = "cmems_mod_glo_phy_my_0.083deg_P1D-m",
      variable  = vars,
      region    = bb,
      timerange = timerange,
      asset = asset,
      progress  = progress,
      username = username,
      password = password
    ) |> adrop()
    attributes(cmsd)$dimensions$longitude$offset <- min(st_get_dimension_values(cmsd, "longitude")) - 1/24
    attributes(cmsd)$dimensions$longitude$delta <- 1/12
    attributes(cmsd)$dimensions$longitude["values"] <- list(NULL)
    attributes(cmsd)$dimensions$latitude$offset <- min(st_get_dimension_values(cmsd, "latitude")) -1/24
    attributes(cmsd)$dimensions$latitude$delta <- 1/12
    attributes(cmsd)$dimensions$latitude["values"] <- list(NULL)
    nms <- names(cmsd)
    cmsd <- lapply(nms, function(var) {
      r <- rast(cmsd[var])
      time(r) <- st_get_dimension_values(cmsd,"time")
      return(r)
    })
    names(cmsd) <- nms
    r_cmsd <- sds(cmsd)
    if(return_type=="stars"){
      r <- lapply(nms, \(var) st_as_stars(r_cmsd[var]))
      r <- do.call(c, r)
      names(r) <- nms
      return(r)
    } else{
      return(r_cmsd)
    }
  }
}

