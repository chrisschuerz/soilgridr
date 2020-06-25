# Conditinal-Concatenate function
#
# Concatenates the data `dat` with whatever is provided by `...` if the
#    `condition` is TRUE.
c_if <- function(dat, condition, ...) {
  if(condition) {dat <- c(dat, ...)}
  return(dat)
}

## Set the nodata values in the loaded raster files
## should be defined as seperate function outside :(
set_nodata <- function(rst) {
  if (dataType(rst) == "INT1U") {
    rst@file@nodatavalue <- 255
  } else if(dataType(rst) == "INT2S") {
    rst@file@nodatavalue <- -32768
  }
  return(rst)
}

## Function to mask raster only when shape file is available
mask_if <- function(rst, shp, shp_from_ext){
  if(!shp_from_ext) rst <- mask(rst, shp)
  return(rst)
}

## Function to project raster only when crs(shp) and crs(rst) differ.
project_if <- function(rst, shp) {
  if(crs(shp)@projargs != crs(rst)@projargs) {
    rst <- projectRaster(rst, crs = crs(shp))
  }
  return(rst)
}


## Convert vectors with soil data values to raster layer
rasterize <- function(soil_vct, meta) {
  # Assign the cluster group indices to vector with length of the final raster map.
  out_rst <- rep(NA, meta$len_rst)
  out_rst[which(!is.na(meta$has_value))] <- soil_vct

  # Reshape the vector and create raster map.
  out_rst %<>%
    matrix(ncol = meta$dim_rst[1],
           nrow = meta$dim_rst[2]) %>%
    t() %>%
    raster(crs = meta$crs)

  # Assign the shape files' extent and provide a nodata value
  extent(out_rst) <- meta$extent
  out_rst@file@nodatavalue <- -32768

  out_rst <- as(out_rst, 'SpatialGridDataFrame')

  return(out_rst)
}

#' Concatenate with an underscore
#'
#' \%_\% pastes two strings by "_".
#' @keywords internal
"%_%" <- function(a, b) paste(a, b, sep = "_")

#' Concatenate with a hyphen
#'
#' \%-\% pastes two strings by "-".
#' @keywords internal
'%-%' <- function(a, b) paste(a, b, sep = "-")

#' Concatenate with a dot
#'
#' \%.\% pastes two strings by ".".
#' @keywords internal
'%.%' <- function(a, b) paste(a, b, sep = ".")

#' Paste slash function
#'
#' \%//\% pastes two strings by "/".
#' @keywords internal
'%//%' <- function(a, b) paste(a, b, sep = "/")

#' Concatenate without separator
#'
#' \%&\% pastes two strings by "".
#' @keywords internal
'%&%' <- function(a, b) paste0(a, b)

#' Concatenate with space
#'
#' \%&&\% pastes two strings by " ".
#' @keywords internal
'%&&%' <- function(a, b) paste(a, b, sep = " ")

#' Concatenate with double colon
#'
#' \%&&\% pastes two strings by " ".
#' @keywords internal
'%:%' <- function(a, b) paste(a, b, sep = " ")


#' Display the progress if iterative processes
#'
#' @param n Iteration step
#' @param nmax Number of iterations
#' @param t0 initial time step
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate as.period interval now seconds
#' @keywords internal
#'
display_progress <- function(n, nmax, t0, word){
  t1 <- now()
  time_elaps  <- interval(t0,t1) %>%
    round(.) %>%
    as.period(.)
  time_remain <- (as.numeric(time_elaps, "seconds")*(nmax-n)/n) %>%
    round(.) %>%
    seconds(.) %>%
    as.period(., unit = "days")

  cat("\r", word, n, "of", nmax,
      "  Time elapsed:", as.character(time_elaps),
      "  Time remaining:", as.character(time_remain),
      "   ")
}

#' Print message for completed process
#'
#' @param nmax Number of iterations
#' @param t0 initial time step
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate as.period interval now
#' @keywords internal
#'
finish_progress <- function(nmax, t0, word1, word2) {
  cat("\r", paste0(rep(" ", 75), collapse = ""))
  interval(t0,now()) %>%
    round(.) %>%
    as.period(.) %>%
    as.character(.) %>%
    cat("\r",word1, nmax, word2%&%plural(nmax), "in", ., "\n")
}

#' Add plural 's' to the written message if multiple operations done
#'
#' @param n Interger number of operations
#' @keywords internal
#'
plural <- function(n) {
  ifelse(n == 1, "", "s")
}
