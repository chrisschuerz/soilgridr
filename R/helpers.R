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
  if(crs(shp)@projargs != crs(rst)@projargs) projectRaster(rst, crs = crs(shp))
  return(rst)
}


## Convert voctors with soil data values to raster layer
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
