#' Plot clustered soil map
#'
#' @param soil_data The cluster results output from \code{cluster_soilgrids()}
#' @param n_class Numeric. Number of soil classes to plot in the soil map.
#'
#' @importFrom dplyr mutate full_join
#' @importFrom ggplot2 aes coord_equal geom_raster ggplot guide_legend
#'   scale_fill_manual theme theme_bw element_text
#' @importFrom magrittr %>% %<>% set_colnames
#' @importFrom pasta %//% %_%
#' @importFrom raster extent raster rasterToPoints
#' @importFrom RColorBrewer brewer.pal
#' @importFrom tibble as_tibble enframe
#'
#' @return Plots the final soilmap with the selected number of soil classes.
#' @keywords internal
plot_soilmap <- function(soil_data, n_class){

  #Checking of number of soil classes
  if(is.null(n_class)){
    stop("No number of soil classes defined!\n"%&%
           "Either set final number of classes with set_n_class()"%&&%
           "or define the number of classes in the function!")
  }
  if(!("n"%_%n_class %in% names(soil_data$soil_cluster))){
    stop("Selected number of classes not available!")
  }

  # Clustering results for the selected number of soil classes in a tibble
  clust_sel <- soil_data$soil_cluster[["n"%_%n_class]]$cluster %>%
    enframe()

  # Assign the cluster group indices to vector with length of the final raster map.
  clust_rst <- rep(NA, soil_data$soilgrids$meta$layer$len_rst)
  clust_rst[which(!is.na(soil_data$soilgrids$meta$layer$has_value))] <- clust_sel$value

  # Reshape the vector and create raster map.
  clust_rst %<>%
    matrix(ncol = soil_data$soilgrids$meta$layer$dim_rst[1],
           nrow = soil_data$soilgrids$meta$layer$dim_rst[2]) %>%
    t() %>%
    raster(crs = soil_data$soilgrids$meta$layer$crs)

  # Assign the shape files' extent and provide a nodata value
  extent(clust_rst) <- soil_data$soilgrids$meta$layer$extent
  clust_rst@file@nodatavalue <- -32768

  clust_tbl <- clust_rst %>%
    rasterToPoints(.) %>%
    as_tibble(.) %>%
    set_colnames(c("x", "y", "soil_class")) %>%
    mutate(soil_class = as.factor(soil_class))

  fill_col <- clust_tbl$soil_class %>%
    unique(.) %>%
    length(.) %>%
    colorRampPalette(brewer.pal(8, "Paired"))(.)

  n_col <- ceiling(length(fill_col) / 15)

  # shape <- soil_data$shape_file
  # shape@data$id <- rownames(shape@data)
  # shape_points <- fortify(shape, region = "id")
  # shape_df <- full_join(shape_points, shape@data, by = "id")

  clust_plot <- ggplot() +
    geom_raster(data = clust_tbl, aes(x = x, y = y, fill = soil_class)) +
    scale_fill_manual(values = fill_col, guide = guide_legend(ncol = n_col)) +
    # geom_polygon(data = shape_df, aes(x = long, y = lat, group = Subbasin), col = "black", fill = NA) +
    coord_equal() +
    theme_bw() +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5))

  return(clust_plot)
}
