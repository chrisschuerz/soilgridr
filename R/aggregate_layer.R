aggregate_layer <- function(soil_list) {

  # Derive the available soil depths
  sl_lbl <- names(soil_list) %>%
    substr(., 10,10) %>%
    unique() %>%
    .[which(nchar(.) == 1)]

  # Group soil layers according to layer depth and calculate further parameters
  soil_list %<>%
    bind_cols(.) %>%
    as_tibble(.) %>%
    set_colnames(tolower(colnames(.))) %>%
    map(sl_lbl, function(x, tbl){select(tbl,ends_with(x))}, .) %>%
    set_names(c("sl"%&%sl_lbl)) %>%
    map_at(., "sl"%&%1:7, function(tbl){names(tbl) <- substr(names(tbl), 1, 6)
    return(tbl)})

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
  sol_aggr <- map(lyr_weight, function(weight, sol_list){
    sol_list$sl1*weight[1] +
      sol_list$sl2*weight[2] +
      sol_list$sl3*weight[3] +
      sol_list$sl4*weight[4] +
      sol_list$sl5*weight[5] +
      sol_list$sl6*weight[6] +
      sol_list$sl7*weight[7]},
    sol_list) %>%
    set_names("lyr"%_%1:length(upper_bound))
}
