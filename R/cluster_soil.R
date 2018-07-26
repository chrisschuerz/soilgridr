#' Cluster Layers
#'
#' Cluster the layers within the \strong{soil project} by using kmeans
#'   clustering.
#'
#' @param soil_list List of tibbles holding soil parameters for the respective
#'   soil layers.
#' @param n_class Vector of number of soil classes that should be generated
#'   with k-means clustering.

#' @importFrom dplyr bind_cols progress_estimated
#' @importFrom magrittr %>%
#' @importFrom purrr map2
cluster_soil <- function(soil_list, n_class){
  # Soil group clustering using kmeans ------------------------------------
  # Create scaled table to apply kmeans on
  suffix <- "_"%&%names(soil_list)
  suffix[!(suffix %in% ("_sl"%&%1:100))] <- ""

  clst_tbl <- soil_list %>%
    purrr::map2(., suffix, function(tbl, nm) {
      names(tbl) <- names(tbl)%&%nm
      return(tbl)}) %>%
    dplyr::bind_cols(.) %>%
    base::scale(., scale = TRUE, center = TRUE) %>%
    as.data.frame(.)

  # Empty list that further stores the clustering results
  soil_km <- list()

  cat("\nCluster soil data:\n")
  pb <- progress_estimated(length(n_class))
  # Loop over all defined number of classes and apply kmeans to the soilgrids data.
  for(i_clust in n_class) {
    soil_km[["n"%_%i_clust]] <- kmeans(x = clst_tbl,centers = i_clust, iter.max = 100)
    pb$tick()$print()
  }
  pb$stop()

  return(soil_km)
}
