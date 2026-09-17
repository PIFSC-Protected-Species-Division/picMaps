#' @title Download Copernicus Marine Service physical oceanography data (3d variables)
#' @param region xxx
#' @param variable xxx
#' @param timerange xxx
#' @param verticalrange xxx
#' @param return_type One of `"stars"` or `"raster"`. A `stars` object is the default.
#' @param progress xxx
#' @param asset xxx
#' @param ... xxx
#' @param username xxx
#' @param password xxx
#' @import CopernicusMarine stars sf terra
#' @importFrom abind adrop
#' @author Devin S. Johnson
#' @export
fetch_cms_phys_3d <- function(
    region,
    variable,
    timerange,
    verticalrange = c(0,-0.5),
    return_type = "stars",
    progress = FALSE,
    asset = "ARCO",
    ...,
    username = NULL,
    password = NULL
){
  if(is.null(username)) username <- cms_get_username()
  if(is.null(password)) password <- cms_get_password()
  vars <- variable[variable %in% c("thetao","so","uo","vo")]
  if(length(vars)==0){
    message("There are no 3d variables to download.")
    return(NULL)
  }

  # if(st_is_longlat(region)){
  #   is_360 <- st_check_lon(region)=="0-360"
  # } else{
  #   is_360 <- FALSE
  # }
  region <- st_transform(region, 4326)
  bb <- st_bbox(region)
  if(bb$xmin>180) bb['xmin'] <- bb['xmin'] - 360
  if(bb$xmax>180) bb['xmax'] <- bb['xmax'] - 360

  # cms_lon <- seq(-180, 180-(1/12), 1/12)
  # cms_lat <- seq(-80, 90-(1/12), 1/12)

  if(bb['xmin']<180 & bb['xmax']<0){
    bb_e <- bb_w <- bb
    bb_e['xmax'] <- 180
    bb_w['xmin'] <- -180
    cmsd_e <- cms_download_subset(
      product   = "GLOBAL_MULTIYEAR_PHY_001_030",
      layer     = "cmems_mod_glo_phy_my_0.083deg_P1D-m",
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
      return(r)
    })
    names(cmsd_e) <- nms
    r_cmsd_e <- sds(cmsd_e)

    cmsd_w <- cms_download_subset(
      product   = "GLOBAL_MULTIYEAR_PHY_001_030",
      layer     = "cmems_mod_glo_phy_my_0.083deg_P1D-m",
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
    }
    return(r)

  }
  else{
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
  }
  attributes(cmsd)$dimensions$longitude$offset <- min(st_get_dimension_values(cmsd, "longitude")) - 1/24
  attributes(cmsd)$dimensions$longitude$delta <- 1/12
  attributes(cmsd)$dimensions$longitude["values"] <- list(NULL)
  attributes(cmsd)$dimensions$latitude$offset <- min(st_get_dimension_values(cmsd, "latitude")) -1/24
  attributes(cmsd)$dimensions$latitude$delta <- 1/12
  attributes(cmsd)$dimensions$latitude["values"] <- list(NULL)
  nms <- names(cmsd)
  cmsd <- lapply(nms, function(var) {
    r <- rast(cmsd[var])
    return(r)
  })
  names(cmsd) <- nms
  r_cmsd <- sds(cmsd)
  if(return_type=="stars"){
    r <- lapply(nms, \(var) st_as_stars(r_cmsd[var]))
    r <- do.call(c, r)
    names(r) <- nms
  } else{
    r <- r_cmsd
  }
  return(r)
}
