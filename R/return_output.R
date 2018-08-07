#' Return a Soil Project Output in R
#'
#' Write the modified \href{https://soilgrids.org/}{soilgrids} layer to a file.
#' @param soil_data The cluster results output from \code{cluster_soilgrids()}.
#' @param variable Charcacter vector providing variable names.
#' @param sl Vector providing soil layers to be plotted
#' @param format Output format to be returned (either tibble or raster).
#' @name write_output
NULL

#' Write Soil Project Output
#'
#' Internal/Actual soilgrids-writing function.
#'
#' @param soil_data The cluster results output from \code{cluster_soilgrids()}.
#' @param variable Charcacter vector providing variable names.
#' @param sl Vector providing soil layers to be plotted
#' @param format Output format to be returned (either tibble or raster).
#'
#'
#' @importFrom dplyr bind_cols filter funs group_by left_join mutate mutate_all
#'    one_of select starts_with summarize
#' @importFrom magrittr %>% %<>%
#' @importFrom pasta %&%
#' @importFrom purrr map2_dfc
#' @importFrom raster extent raster rasterToPoints
#' @importFrom RColorBrewer brewer.pal
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#'
#' @return Writes the modified soilgrids layers in the defined format.
#' @keywords internal

write_out <- function(soil_data, variable, sl, format) {
  layer_suffix <- "_"%&%names(soil_data$data_processed)
  layer_suffix <- layer_suffix[layer_suffix != "_soil_class"]
  layer_suffix[!grepl("_sl", layer_suffix)] <- ""


  plot_data <- soil_data$data_processed[names(soil_data$data_processed)!="soil_class"] %>%
    map2_dfc(., layer_suffix, function(df, suf){
      names(df) <- names(df)%&%suf
      return(df)
    }) %>%
    select(-starts_with("soil_class"))

  if("soil_class" %in% names(soil_data$data_processed)) {
    plot_data %<>% mutate(soil_class = 1:nrow(plot_data))
    plot_data <- left_join(soil_data$data_processed$soil_class, plot_data, by = "soil_class") %>%
      select(-soil_class)
  }

  if(is.null(variable)) {
    variable <- map(soil_data$data_processed, names) %>%
      unlist() %>%
      unique()
    variable <- variable[variable != "soil_class"]
  }

  if(is.null(sl)) sl <- layer_suffix
  if(is.numeric(sl)) sl <- "_sl"%&%sl
  if(substr(sl[1], 1, 2) == "sl") "_"%&%sl

  name_comb <- expand.grid(var = variable, sl = sl) %>%
    mutate(name_comb = var%&%sl) %>%
    .$name_comb

  name_comb <- name_comb[name_comb %in% names(plot_data)]
  plot_data %<>% select(., one_of(name_comb))
