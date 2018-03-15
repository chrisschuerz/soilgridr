#' Create clustered soilmap from soilgrids data
#'
#' @param project_path Path to the SWAT project / Path where downloaded
#'   soilgrids folder is located.
#' @param shp_file Shape file (or path to shape file) for which an
#'   aggregated soil map should be generated. If none is provided (\code{NULL})
#'   the shape file from the SWAT project will be used.
#' @param lower_bound Vector defining the lower bounds of the aggregated
#'   soil layers (depths in cm).
#' @param n_class Vector of number of soil classes that should be generated
#'   with k-means clustering.

#' @importFrom rgdal readOGR
#' @importFrom raster raster projectRaster crop mask crs extent
#' @importFrom euptf predict.ptf psd2classUS
#' @importFrom tibble as_tibble
#' @importFrom dplyr select filter mutate rename bind_cols
#'   progress_estimated ends_with
#' @importFrom tibble as_tibble tibble
#' @importFrom purrr map map2 map_at
#' @importFrom magrittr %>% set_colnames set_rownames set_names

aggregate_layer <- function(soil_list, lower_bound) {

  # Checking if the same layers are available for all seven soil depts.
  depth_avail <- "sl"%&%1:7 %in% names(soil_list)
  if(!all(depth_avail)) stop("Soil aggregation only allowed when all 7 soil depths are available.")

  unique_lyr <- soil_list["sl"%&%1:7] %>%
    map(., colnames) %>%
    unlist(.) %>%
    unique(.)

  lyr_missing <- soil_list["sl"%&%1:7] %>%
    map(., function(x){!all(unique_lyr %in% colnames(x))}) %>%
    unlist(.) %>%
    any(.)

  if(lyr_missing) stop("For depth aggregation respecktive layers must be available for all soil depths!")

  soil_list %<>% map_at(., "sl"%&%1:7, function(x){x[unique_lyr]})

  # Aggregate soil layers over depth ---------------------------------------------
  upper_bound <- c(0,lower_bound[1:length(lower_bound) - 1])

  # Function to calculate the weights of each soil layer for the respective
  # upper and lower boundaries in the soil depth aggregation.
  calc_weights <- function(up_bnd, lw_bnd) {
    sl_depth <- c(2.5, 7.5, 12.5, 22.5, 35, 70, 50)
    sl_depth_cum <- cumsum(sl_depth)
    lw_wgt <- (sl_depth_cum - up_bnd)
    lw_pos <- which(lw_wgt >= 0)[1]
    lw_wgt <- (lw_wgt == lw_wgt[lw_pos]) * lw_wgt
    up_wgt <- lw_bnd - sl_depth_cum
    up_pos <- which(up_wgt <= 0)[1]
    up_wgt <- ((up_wgt == up_wgt[up_pos - 1]) * up_wgt)
    up_wgt <- c(0, up_wgt)[1:7]
    if((up_pos - 1) >= (lw_pos + 1)){
      md_wgt <- sl_depth %in% sl_depth[(lw_pos + 1):(up_pos - 1)] * sl_depth
    } else {
      md_wgt <- rep(0, 7)
    }
    (lw_wgt + md_wgt + up_wgt)/(lw_bnd - up_bnd)
  }

  # Calculate the layer weights for each aggregated soil layer
  lyr_weight <- map2(upper_bound, lower_bound, calc_weights)

  # Calculate the aggregated soil layers by summing up the weighted layers
  soil_aggr <- map(lyr_weight, function(weight, soil_list){
    soil_list$sl1*weight[1] +
      soil_list$sl2*weight[2] +
      soil_list$sl3*weight[3] +
      soil_list$sl4*weight[4] +
      soil_list$sl5*weight[5] +
      soil_list$sl6*weight[6] +
      soil_list$sl7*weight[7]},
    soil_list) %>%
    map(., as_tibble) %>%
    set_names("sl"%&%1:length(upper_bound))

  #Concatinate aggregated soil layer and layers where no aggregation was applied
  soil_return <- c(soil_aggr, soil_list[!(names(soil_list) %in% ("sl"%&%1:100))])

  return(soil_return)
}
