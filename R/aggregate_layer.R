#' Aggregate soilgrids Layers over Depth.
#'
#' Aggregate the \href{https://soilgrids.org/}{soilgrids} layers within the
#'  \strong{soil project} over their respective depth. By convention most
#'  layers are divided into 7 depth-classes.This function allows to aggregated
#'  them over chosen depths.
#'
#' @param lower_bound Vector defining the lower bounds of the aggregated soil
#'   layers (depths in cm).

#' @importFrom magrittr %>% set_names
#' @importFrom purrr map map2 map_at reduce
#' @keywords internal
aggregate_layer <- function(soil_data, lower_bound) {

  # Checking if all depth for intended aggregation are available:
  depth_soilgrids <- c(0, 2.5, 10, 22.5, 45, 80, 150)
  depth_required  <- ("sl"%&%1:7)[depth_soilgrids <= max(lower_bound)]
  depth_avail <- depth_required %in% names(soil_data)
  if(!all(depth_avail)) stop("Required soil depths for intended aggregation missing!")

  soil_data_aggr <- soil_data[depth_required]

  unique_lyr <- soil_data_aggr %>%
    map(., colnames) %>%
    unlist(.) %>%
    unique(.)

  layer_missing <- soil_data_aggr %>%
    map(., function(x){!all(unique_lyr %in% colnames(x))}) %>%
    unlist(.) %>%
    any(.)

  if(layer_missing) stop("For depth aggregation respective layers must be available for all soil depths!")

  soil_data_aggr %<>% map(., function(x){x[unique_lyr]})

  # Aggregate soil layers over depth: --------------------------------------------
  upper_bound <- c(0,lower_bound[1:length(lower_bound) - 1])

  # Function - calc weights of each layer for respective lower/upper bounds:
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

  # Calc the layer weights for each aggregated soil layer
  layer_weight <- map2(upper_bound, lower_bound, calc_weights) %>%
    map(., ~.x[1:sum(depth_avail)])

  # Calc the aggregated soil layers by summing up the weighted layers
  soil_aggr <- layer_weight %>%
    map(., function(weight, layer_list){
      map2(weight, layer_list, ~.x*.y) %>%
      reduce(`+`)},
    soil_data_aggr) %>%
    map(., as_tibble) %>%
    set_names("sl"%&%1:length(upper_bound))

  #Concatenate aggregated soil layer and layers where no aggregation was applied
  soil_return <- c(soil_aggr, soil_data[!(names(soil_data) %in% ("sl"%&%1:100))])

  return(soil_return)
}
