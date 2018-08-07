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
#' @importFrom purrr map2_dfc
#' @importFrom raster extent extent<- raster rasterToPoints stack values values<-
#' @importMethodsFrom raster extent values
#' @importFrom RColorBrewer brewer.pal
#' @importFrom sp SpatialGridDataFrame
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#'
#' @return Writes the modified soilgrids layers in the defined format.
#' @keywords internal

return_out <- function(soil_data, variable, sl, format) {
  layer_suffix <- "_"%&%names(soil_data$data_processed)
  layer_suffix <- layer_suffix[layer_suffix != "_soil_class"]
  layer_suffix[!grepl("_sl", layer_suffix)] <- ""


  return_data <- soil_data$data_processed[names(soil_data$data_processed)!="soil_class"] %>%
    map2_dfc(., layer_suffix, function(df, suf){
      names(df) <- names(df)%&%suf
      return(df)
    }) %>%
    select(-starts_with("soil_class"))

  if("soil_class" %in% names(soil_data$data_processed)) {
    return_data %<>% mutate(soil_class = 1:nrow(return_data))
    return_data <- left_join(soil_data$data_processed$soil_class, return_data, by = "soil_class")
  }

  if(is.null(variable)) {
    variable <- map(soil_data$data_processed, names) %>%
      unlist() %>%
      unique()
  }

  if(is.null(sl)) sl <- layer_suffix
  if(is.numeric(sl)) sl <- "_sl"%&%sl
  if(substr(sl[1], 1, 2) == "sl") "_"%&%sl

  name_comb <- expand.grid(var = variable, sl = sl) %>%
    mutate(name_comb = var%&%sl) %>%
    .$name_comb

  name_comb <- name_comb[name_comb %in% names(return_data)]
  return_data %<>% select(., one_of(name_comb))

  if(format == "tibble") {
    # Create raster dummy for x/y coordinates.
    index <- soil_data$soilgrids$meta$layer$has_value
    index[!is.na(index)] <- 1:length(index[!is.na(index)])

    rst_dummy <- index %>%
      matrix(ncol = soil_data$soilgrids$meta$layer$dim_rst[1],
             nrow = soil_data$soilgrids$meta$layer$dim_rst[2]) %>%
      t() %>%
      raster(crs = soil_data$soilgrids$meta$layer$crs)

    # Assign the shape files' extent and provide a nodata value
    extent(rst_dummy) <- soil_data$soilgrids$meta$layer$extent

    rst_point <- rst_dummy
    values(rst_point) <- 1

    return_data <- rst_point %>%
      rasterToPoints(.) %>%
      as_tibble(.) %>%
      mutate(layer = values(rst_dummy)) %>%
      left_join(., return_data %>% mutate(., layer = 1:nrow(.)), by = "layer") %>%
      select(-layer)
  } else if (format == "raster") {
    return_data <- map(return_data, rasterize, soil_data$soilgrids$meta$layer) %>%
      map(.,raster) %>%
      stack(.)
  }

  return(return_data)
}
