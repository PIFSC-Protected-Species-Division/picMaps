#' @title Download Open Street Map Coastline Shapefile from Daylight Map Distribution
#' @description Downloads OSM data for plotting telemetry data and use distributions.
#' @param force Logical. If data has previously been downloaded, it will force a new
#' download and update of the OSM data.
#' @param clean_shp Logical. If `TRUE` the original shape file will be deleted and only the `.gpkg` file will be retained.
#' @references https://osmdata.openstreetmap.de/data/land-polygons.html
#' @author Devin S. Johnson and Josh M. London
#' @importFrom curl multi_download
#' @importFrom utils unzip
#' @importFrom sf st_read st_write st_geometry st_is_valid st_make_valid
#' @export

osm_download <- function(force = FALSE, clean_shp = TRUE) {
  data_loc <- get_data_loc()
  dir <- file.path(data_loc, "osm_coast")
  gpkg_path <- file.path(dir, "osm_coast.gpkg")
  zip_name <- "land-polygons-complete-4326.zip"
  zip_dest <- file.path(dir, zip_name)
  external_zip <- file.path(data_loc, zip_name)

  if (file.exists(gpkg_path) && !force) {
    stop("OSM data has already been downloaded. Use 'force = TRUE' to update.", call. = FALSE)
  }

  use_zip <- file.exists(external_zip)
  if (interactive()) {
    prompt_msg <- if (use_zip) {
      paste0(
        "'", zip_name, "' file detected. This file will be used.\n",
        "If this is not desired, please delete or move it to another directory.\n",
        "Continue? [y/n]: "
      )
    } else {
      "This function will download a considerable amount of coastline data (~925 MB).\nAre you sure you want to proceed? [y/n]: "
    }
    inp <- readline(prompt = prompt_msg)
    if (tolower(trimws(inp)) %in% c("n", "no")) {
      stop("OSM download canceled.", call. = FALSE)
    }
  }

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  if (use_zip) {
    # Instant move on the same drive; fall back to copy+remove across drives
    moved <- suppressWarnings(file.rename(from = external_zip, to = zip_dest))
    if (!moved) {
      cp <- file.copy(from = external_zip, to = zip_dest, overwrite = TRUE)
      if (cp) file.remove(external_zip)
    }
  } else {
    if (force && file.exists(zip_dest)) {
      unlink(zip_dest)
    }
    message("Downloading land polygons (resumes automatically if interrupted) ...")
    url <- "https://osmdata.openstreetmap.de/download/land-polygons-complete-4326.zip"
    res <- curl::multi_download(
      urls = url,
      destfiles = zip_dest,
      resume = TRUE,
      progress = TRUE
    )
    if (!isTRUE(res$success) || (!is.na(res$status_code) && res$status_code >= 400)) {
      stop(
        "Download failed (HTTP ", res$status_code, "): ", res$error,
        "\nRe-run `osm_download()` to resume from the partial download.",
        call. = FALSE
      )
    }
  }

  message("Unpacking polygons ...")
  utils::unzip(zip_dest, exdir = dir)
  unlink(zip_dest)

  shp_dir <- file.path(dir, "land-polygons-complete-4326")
  if (clean_shp) {
    on.exit(unlink(shp_dir, recursive = TRUE), add = TRUE)
  }

  message("Checking polygons for validity ...")
  land_path <- file.path(shp_dir, "land_polygons.shp")
  geom <- sf::st_geometry(sf::st_read(land_path, quiet = TRUE))

  chk <- sf::st_is_valid(geom)
  ind <- which(!chk | is.na(chk))
  if (length(ind) > 0) {
    message("Repairing ", length(ind), " invalid polygon(s) ...")
    geom[ind] <- sf::st_make_valid(geom[ind])
  }

  message("Saving coast data to .gpkg database ...")
  sf::st_write(
    geom,
    dsn = gpkg_path,
    layer = "osm_coast",
    driver = "GPKG",
    delete_dsn = TRUE,
    quiet = TRUE
  )

  readme_src <- file.path(shp_dir, "README.txt")
  readme_dst <- file.path(dir, "README.txt")
  if (file.exists(readme_src)) {
    file.rename(from = readme_src, to = readme_dst)
    message(paste(readLines(readme_dst, warn = FALSE), collapse = "\n"))
  }

  invisible(gpkg_path)
}
