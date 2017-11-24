#' Plot clustered soil map
#'
#' @param sg_cluster The cluster results output from \code{cluster_soilgrids()}
#' @param n_class Numeric. Number of soil classes to plot in the soil map.
#'
#' @importMethodsFrom  raster plot
#' @importFrom tibble enframe
#' @importFrom dplyr mutate
#' @importFrom magrittr %>% %<>%
#' @importFrom pasta %//% %_%

#'
#' @return Plots the final soilmap with the selected number of soil classes.
#' @export

plot_soilmap <- function(sg_cluster, n_class){

  # Clustering results for the selected number of soil classes in a tibble
  clust_sel <- sg_cluster$soil_cluster[["n"%_%n_class]]$cluster %>%
    enframe() %>%
    mutate(name = name %>% gsub("cell_", "", .) %>% as.numeric(.))

  # Assign the cluster group indices to vector with length of the final raster map.
  clust_rst <- rep(NA, sg_cluster$layer_meta$len_rst)
  clust_rst[which(!is.na(sg_cluster$layer_meta$has_value))] <- clust_sel$value

  # Reshape the vector and create raster map.
  clust_rst %<>%
    matrix(ncol = sg_cluster$layer_meta$dim_rst[1],
           nrow = sg_cluster$layer_meta$dim_rst[2]) %>%
    t() %>%
    raster(crs = sg_cluster$layer_meta$crs)

  # Assign the shape files' extent and provide a nodata value
  extent(clust_rst) <- sg_cluster$layer_meta$extent
  clust_rst@file@nodatavalue <- -32768

  plot(clust_rst)
}
