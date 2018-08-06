#' Plot soil variables from processed data
#'
#' Internal/Actual general-function to plot the clustered soil maps.
#'
#' @param soil_data The cluster results output from \code{cluster_soilgrids()}.
#' @param variable Charcacter vector providing variable names.
#' @param sl Vector providing soil layers to be plotted
#'
#' @importFrom dplyr bind_cols filter left_join mutate one_of select starts_with
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


plot_variable <- function(soil_data, variable, sl, normalize) {

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


  # Create raster dummy for x/y coordinates.
  rst_dummy <- soil_data$soilgrids$meta$layer$has_value %>%
    matrix(ncol = soil_data$soilgrids$meta$layer$dim_rst[1],
           nrow = soil_data$soilgrids$meta$layer$dim_rst[2]) %>%
    t() %>%
    raster::raster(crs = soil_data$soilgrids$meta$layer$crs)

  # Assign the shape files' extent and provide a nodata value
  raster::extent(rst_dummy) <- soil_data$soilgrids$meta$layer$extent

  rst_tbl <- rst_dummy %>%
    raster::rasterToPoints(.) %>%
    as_tibble(.) %>%
    filter(!is.na(layer)) %>%
    select(x,y)

  if(normalize) {
    normalize <- function(x) {(x - min(x)) / (max(x) - min(x))}
    select_min <- function(x) {
      w_min <- which(x == min(x))
      w_min[ceiling(length(w_min)/2)]}
    select_max <- function(x) {
      w_max <- which(x == max(x))
      w_max[ceiling(length(w_max)/2)]}
    range_data <- plot_data %>%
      gather(key = "variable") %>%
      group_by(variable) %>%
      summarize(min = min(value), max = max(value),
                which_min = select_min(value), which_max = select_max(value)) %>%
      mutate(x_min = rst_tbl$x[which_min],
             y_min = rst_tbl$y[which_min],
             x_max = rst_tbl$x[which_max],
             y_max = rst_tbl$y[which_max]) %>%
      select(-which_min, -which_max)


    plot_data %<>%  mutate_all(funs(normalize))
    }

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

  if(normalize) {
    layer_plot <- layer_plot +
      geom_point(data = range_data, aes(x = x_min, y = y_min), pch = 3) +
      geom_text(data = range_data, aes(x = x_min, y = y_min, label = min),
                hjust = 0, vjust = 0, nudge_x = 0.02) +
      geom_point(data = range_data, aes(x = x_max, y = y_max), pch = 3) +
      geom_text(data = range_data, aes(x = x_max, y = y_max, label = max),
                hjust = 0, vjust = 0, nudge_x = 0.02)
  }

  return(layer_plot)
}
