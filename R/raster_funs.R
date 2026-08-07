#' @title Z transform a raster
#' @param r A spatRaster object from the terra package
#' @importFrom terra global
#' @export
raster_z <- function(r){
  return(
  (r - global(r, "mean", na.rm=TRUE)[1,1]) / global(r, "sd", na.rm=TRUE)[1,1]
  )
}

#' @title Bhattacharyya distance for 2 raster probability densities
#' @param r SpatRaster object
#' @param s SpatRaster object
#' @details
#' Computes Bhattacharyya distance for 2 raster UDs.The 2 rasters are first normalized to
#' sum to 1.
#' @importFrom terra global
#' @export
bc_raster <- function(r,s){
  r <- r/(global(r, "sum", na.rm=T)[1,1])
  s <- s/(global(s, "sum", na.rm=T)[1,1])
  xx <- sqrt(r*s)
  bc <- global(xx,"sum",na.rm=TRUE)[1,1]
  return(bc)
}

#' @title Find gradient field of a elevation raster
#' @param r SpatRaster object
#' @param scale Logical, Should the gradient be scaled by resolution of the coordinates. Defaults
#' to `scale = TRUE`
#' @importFrom terra terrain res
#' @export
get_grad <- function(r, scale=TRUE){
  h <- res(r)
  sa <- terra::terrain(r, v=c('slope', 'aspect'), unit="radians")
  if(scale){
    sa[["dx"]] <- h[1]*sa[['slope']]*sin(sa[['aspect']])
    sa[["dy"]] <- h[2]*sa[['slope']]*cos(sa[['aspect']])
  } else{
    sa[["dx"]] <- sa[['slope']]*sin(sa[['aspect']])
    sa[["dy"]] <- sa[['slope']]*cos(sa[['aspect']])
  }
  sa <- sa[[-c(1:2)]]
  return(sa)
}

# #' @title Reconstruct elevation from gradient field
# #' @param sa SpatRaster with 'dx' and 'dy' layers
# #' @param template The original SpatRaster (to get CRS/Extent)
# #' @importFrom terra values res rast as.matrix
# #' @importFrom Matrix sparseMatrix solve
# #' @export
# reverse_grad_final <- function(sa, template) {
#   # 1. Extract values and flip the Y gradient to match raster indexing
#   # (Since rows increase Southward, but 'dy' usually assumes North is positive)
#   dx_vals <- values(sa[["dx"]])
#   dy_vals <- -values(sa[["dy"]]) # The "Magic" Minus Sign
#
#   n <- ncell(template)
#   nc <- ncol(template)
#   nr <- nrow(template)
#
#   # 2. Identify where we actually have data
#   valid_idx <- which(!is.na(dx_vals) & !is.na(dy_vals))
#
#   # 3. Create the Gradient Operator (Matrix A)
#   # We want: Elevation[neighbor] - Elevation[current] = Gradient
#
#   # Horizontal differences (X)
#   rows_x <- 1:n
#   cols_x_curr <- 1:n
#   cols_x_next <- pmin(1:n + 1, n)
#   # Avoid wrapping around edges of the raster
#   edge_x <- which(1:n %% nc == 0)
#   cols_x_next[edge_x] <- cols_x_curr[edge_x]
#
#   Dx <- sparseMatrix(i = rep(rows_x, 2),
#                      j = c(cols_x_curr, cols_x_next),
#                      x = rep(c(-1, 1), each = n))
#
#   # Vertical differences (Y)
#   rows_y <- 1:n
#   cols_y_curr <- 1:n
#   cols_y_next <- pmin(1:n + nc, n)
#   # Avoid wrapping off the bottom
#   edge_y <- which(1:n > (n - nc))
#   cols_y_next[edge_y] <- cols_y_curr[edge_y]
#
#   Dy <- sparseMatrix(i = rep(rows_y, 2),
#                      j = c(cols_y_curr, cols_y_next),
#                      x = rep(c(-1, 1), each = n))
#
#   # 4. Combine and Filter for NAs
#   A <- rbind(Dx[valid_idx, ], Dy[valid_idx, ])
#   b <- c(dx_vals[valid_idx], -dy_vals[valid_idx])
#
#   # 5. Solve using Least Squares
#   # We use the QR decomposition which handles the NAs by finding the best fit
#   message("Solving large linear system... this may take a moment.")
#   z_vec <- solve(qr(A), b)
#
#   # 6. Reconstruct the SpatRaster
#   res_rast <- rast(template)
#   values(res_rast) <- as.numeric(z_vec)
#
#   return(res_rast)
# }


fill_in <- function(r){
  holes <- any(is.na(values(r)))
  while(holes){
    r <- focal(r, w=3, fun=mean, na.policy="only", na.rm=TRUE)
    holes <- any(is.na(values(r)))
  }
  return(r)
}


#' @title Fill raster holes and NAs with local interpolation
#' @param r SpatRaster object
#' @details
#' Uses focal means to infill holes and NAs
#' @importFrom terra focal
#' @export
raster_fill <- function(r){
  . <- NULL
  nms <- names(r)
  r <- as.list(r)
  r <- sapply(r, \(x) fill_in(x)) %>% do.call(c,.)
  names(r) <- nms
  return(r)
}


