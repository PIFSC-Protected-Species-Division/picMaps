#' @title Download Open Street Map Coastline Shapefile from Daylight Map Distribution
#' @description Downloads OSM data for plotting telemetry data and use distributions.
#' @param force Logical. If data has previously been downloaded, it will force a new
#' download and update of the OSM data.
#' @param clean_shp Logical. If `TRUE` the original shape file will be deleted and only the `.gpkg` file will be retained.
#' @references https://osmdata.openstreetmap.de/data/land-polygons.html
#' @author Devin S. Johnson and Josh M. London
#' @importFrom utils download.file unzip
#' @importFrom sf st_read st_write st_is_valid st_make_valid
#' @export


osm_download <- function(force = FALSE, clean_shp=TRUE) {

  dir <- file.path(get_data_loc(), "osm_coast")
  if(file.exists(file.path(dir,'osm_coast.gpkg')) & !force) {
    stop("OSM data has already been downloaded. Use 'force=TRUE' to update.")
  } else {
    inp <- readline(prompt = "This function will download a considerable amount of coastline data.\nAre you sure you want to proceed? [y/n]: \n")
    if(tolower(inp)%in%c("n","no")) stop("OSM download crisis averted, phewww!")
    options(timeout = max(1000, getOption("timeout")))
    # dir <- file.path(dir,"osm")
    dir.create(dir, recursive=TRUE)

    message("Downloading land polygons ...")
    download.file("https://osmdata.openstreetmap.de/download/land-polygons-complete-4326.zip",
                  destfile = file.path(dir,"land-polygons-complete-4326.zip"),
                  method = "auto")

    message("Unpacking polygons ...")
    unzip(file.path(dir,"land-polygons-complete-4326.zip"), exdir = dir)
    file.remove(file.path(dir,"land-polygons-complete-4326.zip"))

    message("Checking polygons for validity ...")
    land_path <- file.path(dir, "land-polygons-complete-4326", "land_polygons.shp")
    land <- sf::st_read(land_path)
    chk <- sf::st_is_valid(land)
    # all(chk)
    if(!all(chk)){
      message("Repairing invalid polygons ...")
      ind <- which(!chk)
      # mapview(land[ind,])
      land[ind,] <- sf::st_make_valid(land[ind,])
    }
    message("Saving coast data to .gpkg database...")
    if(file.exists(file.path(dir,'osm_coast.gpkg')) & force) file.remove(file.path(dir,'osm_coast.gpkg'))
    sf::st_write(sf::st_geometry(land), file.path(dir,'osm_coast.gpkg'), driver="GPKG")
    file.rename(from=file.path(dir, "land-polygons-complete-4326","README.txt"), to=file.path(dir,"README.txt"))
    m <- paste(readLines(file.path(dir, "README.txt")),"\n")
    message(m)
    if(clean_shp){
      unlink(file.path(dir,"land-polygons-complete-4326"), recursive = TRUE)
    }
  }
}

