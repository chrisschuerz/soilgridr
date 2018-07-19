#' Calculate new soil properties.
#'
#' Calculate new soil properties from the \href{https://soilgrids.org/}{soilgrids}}
#' layers within the \strong{soil project}
#'
#' @param soil_list List of tibbles holding soil parameters for the respective
#'   soil layers.
#' @param sl Vector defining the soil layers for which the properties are calculated.

#' @importFrom dplyr quo mutate
#' @importFrom magrittr %<>%
#' @importFrom purrr map map2 map_at



calculate_soilproperty <- function(soil_list, sl = NULL, ...) {

  fun_list <- as.list(match.call(expand.dots = FALSE))$...

  if(is.numeric(sl)) sl <- "sl"%&%sl
  if(is.null(sl)) sl <- names(soil_list) %>% .[grepl("sl",.)]

  layer_avail <- sl %in% names(soil_list)
  if(!all(layer_avail)) stop("Soil layer given with 'sl' are missing!")

  calc_prop_i <- function(soil_list, sl, var_i, fun_i) {
    map_at(soil_list, .at = sl, ~ mutate(.x, !! var_i := !! fun_i))
  }

  new_var <- names(fun_list)

  for(var_i in new_var) {
    fun_list[[var_i]] <- quo(!!fun_list[[var_i]])
    soil_list  %<>% calc_prop_i(., sl, var_i, fun_list[[var_i]])
  }

  return(soil_list)
}
