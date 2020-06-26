#' Aggregate soilgrids Layers over Depth.
#'
#' Aggregate the \href{https://soilgrids.org/}{soilgrids} layers within the
#'  \strong{soil project} over their respective depth. By convention most
#'  layers are divided into seven depth-classes (and \code{aggregate_layer}
#'  function allows to aggregate them over chosen depths.
#'
#' @param bottom_depths Vector defining the lower bounds of the aggregated soil
#'   layers (depths, given in cm).

#' @importFrom dplyr filter mutate %>%
#' @importFrom purrr map map2 reduce set_names
#' @importFrom tibble tibble
#' @keywords internal
aggregate_layer <- function(soil_data, bottom_depths, depth_table) {

  # Checking if all depth for intended aggregation are available:
  max_depth <- max(bottom_depths)
  if(max_depth > max(depth_table$bottom)) {
    stop("The maximum 'bottom_depths' is greater than the maximum available depth.")
  }

  depth_table <- filter(depth_table, bottom <= max_depth)

  if(any(!(depth_table$top[2:nrow(depth_table)] %in% depth_table$bottom))) {
    stop("A soil depth interval is missing to perform the aggregation over depth!\n",
         "Solving this issue requires to start a new project and to download all",
         "soil depth \nintervals that are within the aggregation range.")
  }

  soil_data <- soil_data[depth_table$sl]

  unique_lyr <- soil_data %>%
    map(., colnames) %>%
    unlist(.) %>%
    unique(.)

  layer_missing <- soil_data %>%
    map(., function(x){!all(unique_lyr %in% colnames(x))}) %>%
    unlist(.) %>%
    any(.)

  if(layer_missing) stop("For depth aggregation respective layers must be available for all soil depths!")

  soil_data <- map(soil_data, function(x){x[unique_lyr]})


  # Aggregate soil layers over depth: --------------------------------------------
  top_depths <- c(0,bottom_depths[1:length(bottom_depths) - 1])

  # Function - calc weights of each layer for respective lower/upper bounds:
  calculate_weights <- function(top_dpt, bot_dpt, depth_table) {
    mutate(depth_table,
           top = ifelse(top > top_dpt, top, top_dpt),
           bottom = ifelse(bottom < bot_dpt, bottom, bot_dpt),
           int = bottom - top,
           int = ifelse(int >0, int, 0),
           wgt = int/sum(int)) %>%
      .$wgt
  }

  # Calc the layer weights for each aggregated soil layer
  layer_weight <- map2(top_depths, bottom_depths, ~ calculate_weights(.x, .y, depth_table))

  depth_table_aggr <- tibble(label = "sl"%&%1:length(top_depths)%&%
                                    ": "%&%top_depths%-%bottom_depths%&%"cm",
                            sl = 1:length(top_depths),
                            top = top_depths,
                            bottom = bottom_depths
                            )

  # Calc the aggregated soil layers by summing up the weighted layers
  soil_data_aggr <- layer_weight %>%
    map(., function(weight, layer_list){
      map2(weight, layer_list, ~.x*.y) %>%
      reduce(`+`)},
    soil_data) %>%
    map(., as_tibble) %>%
    set_names(depth_table_aggr$label)


  #Concatenate aggregated soil layer and layers where no aggregation was applied
  out_list <- list(soil_data = soil_data_aggr,
                   depth_table = depth_table_aggr)

  return(out_list)
}
