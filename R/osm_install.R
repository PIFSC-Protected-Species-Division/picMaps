#' @title Download Open Street Map Coastline Shapefile from Daylight Map Distribution
#' @description Downloads OSM data for plotting telemetry data and use distributions.
#' @param zip_file path to the OSM data .zip file `land-polygons-complete-4326.zip`. If left unspecified, the user will see a directory browser to navigate to it.
#' See the url in the references to download it by hand. Also, see `\link{osm_download}` to download and install it.
#' @param force Logical. If data has previously been downloaded or installed, it will force a new
#' install and update of the OSM data.
#' @param clean_shp Logical. If `TRUE` the original shape file will be deleted and only the `.gpkg` file will be retained.
#' @references Get information here: https://osmdata.openstreetmap.de/data/land-polygons.html and download with this link
#'  https://osmdata.openstreetmap.de/download/land-polygons-complete-4326.zip
#' @author Devin S. Johnson and Josh M. London
#' @importFrom utils download.file unzip tail
#' @importFrom sf st_read st_write st_is_valid st_make_valid
#' @export

osm_install <- function(zip_file, force = FALSE, clean_shp=TRUE) {
  if(missing(zip_file)) zip_file <- file.choose()
  dir <- file.path(get_data_loc(), "osm_coast")
  if(file.exists(file.path(dir,'osm_coast.gpkg')) & !force) {
    stop("OSM data has already been downloaded. Use 'force=TRUE' to update.")
  } else {
    if(tail(strsplit(zip_file,"/")[[1]],1) != "land-polygons-complete-4326.zip") stop("This does not appear to be the correct file: 'land-polygons-complete-4326.zip'")
    dir.create(dir, recursive=TRUE, showWarnings = FALSE)
    file.copy(from=zip_file, file.path(dir,"land-polygons-complete-4326.zip"))

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

