#' @title Set Data Storage Directory
#' @param path A directory path specifying where spatial data will be stored for the package
#' @description
#' The directory specified by \code{path} will be used to store downloaded spatial data. By storing the
#' data in a separate directory from the installed package it will not need to be re-downloaded when the package
#' is updated. You simply need to rerun this function to let the package functions know where the data is located.
#' @export
set_data_storage <- function(path=NULL){
  if(is.null(path)) path <- "~/.picmaps_data"
  if(!dir.exists(file.path(system.file(package="picMaps"), "inst"))){
    dir.create(file.path(system.file(package="picMaps"), "inst"))
  }
  saveRDS(path, file.path(system.file(package="picMaps"), "inst", "data_loc.rds"))
}


#' @title Show picMaps Data Storage Directory
#' @description
#' This function provides the location where `picMaps` is storing and retrieving spatial data.
#' @export

get_data_loc <- function(){
  x <- file.path(system.file(package="picMaps"), "inst", "data_loc.rds")
  if(!file.exists(x)){
    stop("The data storage location needs to be specified! See ?set_data_storage()")
  }
  out <- readRDS(file.path(system.file(package="picMaps"), "inst", "data_loc.rds"))
  return(out)
}
