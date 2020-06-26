#' Calculate new soil properties
#'
#' Calculate new soil properties from the \href{https://soilgrids.org/}{soilgrids}
#' layers within the \strong{soil project}.
#'
#' @param soil_data List of tibbles holding soil parameters for the respective
#'   soil layers.
#' @param sl Vector defining the soil layers for which the properties are calculated.
#' @param ... Further arguments provide functions to calculate new soil porperties (in \code{dplyr::mutate} style).
#'
#' @importFrom dplyr quo mutate %>%
#' @importFrom purrr map_at
calculate_soilproperty <- function(soil_data, sl = NULL, fun_list, depth_table) {

  if(is.null(sl)) sl <- depth_table$sl
  stopifnot(is.numeric(sl))
  if(any(!(sl %in% depth_table$sl))) {
    stop("Values for 'sl' must be between 1 and ", length(soil_data),
         " (the number of available soil layers)!")
  }

  map_at(soil_data, .at = sl, ~ mutate(.x, !!!fun_list))
}

#' Select soil properties.
#'
#' Select soil properties provided in the \strong{soil projects'} soil layer tables.
#'
#' @param soil_data List of tibbles holding soil parameters for the respective
#'   soil layers.
#' @param sl Vector defining the soil layers for which the properties are calculated.
#' @param ... Further arguments provide soil porperties to select in \code{dplyr::select} style.
#'
#' @importFrom dplyr quo select %>%
#' @importFrom purrr map_at

select_soilproperty <- function(soil_data, sl = NULL, sel_expr, depth_table) {

  if(is.null(sl)) sl <- depth_table$sl
  stopifnot(is.numeric(sl))
  if(any(!(sl %in% depth_table$sl))) {
    stop("Values for 'sl' must be between 1 and ", length(soil_data),
         " (the number of available soil layers)!")
  }

  map_at(soil_data, .at = sl, ~ select(.x, !!sel_expr))

}
