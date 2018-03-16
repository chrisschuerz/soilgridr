#' Create clustered soilmap from soilgrids data
#'
#' @param project_path Path to the SWAT project / Path where downloaded
#'   soilgrids folder is located.
#' @param shp_file Shape file (or path to shape file) for which an
#'   aggregated soil map should be generated. If none is provided (\code{NULL})
#'   the shape file from the SWAT project will be used.
#' @param lower_bound Vector defining the lower bounds of the aggregated
#'   soil layers (depths in cm).
#' @param n_class Vector of number of soil classes that should be generated
#'   with k-means clustering.

#' @importFrom rgdal readOGR
#' @importFrom raster raster projectRaster crop mask crs extent
#' @importFrom euptf predict.ptf psd2classUS
#' @importFrom tibble as_tibble
#' @importFrom dplyr select filter mutate rename bind_cols
#'   progress_estimated ends_with
#' @importFrom tibble as_tibble tibble
#' @importFrom purrr map map2 map_at
#' @importFrom magrittr %>% set_colnames set_rownames set_names
#'
#' @return List that holds the soilgrids data and the clustering
#'   results for further processing with \code{write_SWATsoil()}.
#' @export


cluster_soil <- function(soil_list, n_class){
  # Soil group clustering using kmeans -------------------------------------------
  # Create scaled table to apply kmeans on
  suffix <- "_"%&%names(soil_list)
  suffix[!(suffix %in% ("_sl"%&%1:100))] <- ""

  clst_tbl <- soil_list %>%
    map2(., suffix, function(tbl, nm) {
      names(tbl) <- names(tbl)%&%nm
      return(tbl)}) %>%
    bind_cols(.) %>%
    scale(., scale = TRUE, center = TRUE) %>%
    as.data.frame(.)

  # Empty list that furhter stores the clustering results
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
