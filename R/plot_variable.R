#' Plot soil variables from processed data
#'
#' Internal/Actual general-function to plot the clustered soil maps.
#'
#' @param soil_data The cluster results output from \code{cluster_soilgrids()}.
#' @param variable Charcacter vector providing variable names.
#' @param sl Vector providing soil layers to be plotted
#'
#' @importFrom dplyr bind_cols filter mutate one_of select
#' @importFrom ggplot2 aes coord_equal facet_wrap geom_raster ggplot
#'   scale_fill_gradientn theme theme_bw element_text
#' @importFrom magrittr %>% %<>%
#' @importFrom pasta %&%
#' @importFrom purrr map2_dfc
#' @importFrom raster extent raster rasterToPoints
#' @importFrom RColorBrewer brewer.pal
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#'
#' @return Plots the final soilmap with the selected number of soil classes.
#'
#' @keywords internal


plot_variable <- function(soil_data, variable, sl) {

  layer_suffix <- "_"%&%names(soil_data$data_processed)
  layer_suffix[!grepl("_sl", layer_suffix)] <- ""

  plot_data <- soil_data$data_processed %>%
    map2_dfc(., layer_suffix, function(df, suf){
      names(df) <- names(df)%&%suf
      return(df)
    })

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

  name_comb <- name_comb[name_comb %in% names(plot_data)]
  plot_data %<>% select(., one_of(name_comb))


  # Create raster dummy for x/y coordinates.
  rst_dummy <- soil_data$soilgrids$meta$layer$has_value %>%
    matrix(ncol = soil_data$soilgrids$meta$layer$dim_rst[1],
           nrow = soil_data$soilgrids$meta$layer$dim_rst[2]) %>%
    t() %>%
    raster(crs = soil_data$soilgrids$meta$layer$crs)

  # Assign the shape files' extent and provide a nodata value
  extent(rst_dummy) <- soil_data$soilgrids$meta$layer$extent

  rst_tbl <- rst_dummy %>%
    rasterToPoints(.) %>%
    as_tibble(.) %>%
    filter(!is.na(layer)) %>%
    select(x,y)

  plot_data <- bind_cols(plot_data, rst_tbl) %>%
    gather(., key = "variable", value = "value", -x, -y)

  layer_plot <- ggplot() +
    geom_raster(data = plot_data, aes(x = x, y = y, fill = value)) +
    scale_fill_gradientn(colours = brewer.pal(11, "Spectral")) +
    # geom_polygon(data = shape_df, aes(x = long, y = lat, group = Subbasin), col = "black", fill = NA) +
    coord_equal() +
    theme_bw() +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
    facet_wrap(~variable)

  return(layer_plot)
}
