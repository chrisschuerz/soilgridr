#' Plot clustered soil map
#'
#' @param cluster_result The cluster results output from \code{cluster_soilgrids()}
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

  plot(clust_rst)
}
