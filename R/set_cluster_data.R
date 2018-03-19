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
#'


set_cluster_data <- function(soil_data, n_class) {

  soil_clust <- soil_data$soil_cluster[["n"%_%n_class]]$cluster

  data_proc <- soil_data$data_processed %>%
    map(., function(tbl, add_col){ add_column(tbl, soil_class = add_col) %>%
                                     group_by(soil_class) %>%
                                     summarise_all(mean)}, soil_clust)

  data_proc$soil_class <- tibble(soil_class = soil_clust)

  return(data_proc)
}
