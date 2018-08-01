#' Calculate new soil properties
#'
#' Calculate new soil properties from the \href{https://soilgrids.org/}{soilgrids}
#' layers within the \strong{soil project}.
#'
#' @param soil_list List of tibbles holding soil parameters for the respective
#'   soil layers.
#' @param sl Vector defining the soil layers for which the properties are calculated.
#' @param ... Further arguments provide functions to calculate new soil porperties (in \code{dplyr::mutate} style).
#'
#' @importFrom dplyr quo mutate
#' @importFrom magrittr %<>% %>%
#' @importFrom purrr map map2 map_at
calculate_soilproperty <- function(soil_list, sl = NULL, fun_list) {

  if(is.numeric(sl)) sl <- "sl"%&%sl
  if(is.null(sl)) sl <- names(soil_list) %>% .[grepl("sl",.)]

  layer_avail <- sl %in% names(soil_list)
  if(!all(layer_avail)) stop("Soil layer given with 'sl' are missing!")

  map_at(soil_list, .at = sl, ~ mutate(.x, !!!fun_list))
}

#' Select soil properties.
#'
#' Select soil properties provided in the \strong{soil projects'} soil layer tables.
#'
#' @param soil_list List of tibbles holding soil parameters for the respective
#'   soil layers.
#' @param sl Vector defining the soil layers for which the properties are calculated.
#' @param ... Further arguments provide soil porperties to select in \code{dplyr::select} style.
#'
#' @importFrom dplyr quo select
#' @importFrom magrittr %>%
#' @importFrom purrr map_at

select_soilproperty <- function(soil_list, sl = NULL, sel_expr) {

  if(is.numeric(sl)) sl <- "sl"%&%sl
  if(is.null(sl)) sl <- names(soil_list) %>% .[grepl("sl",.)]

  layer_avail <- sl %in% names(soil_list)
  if(!all(layer_avail)) stop("Soil layer given with 'sl' are missing!")

  map_at(soil_list, .at = sl, ~ select(.x, !!sel_expr))

}
