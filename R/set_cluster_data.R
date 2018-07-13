#' Set Final Cluster Number
#'
#' Define the final number of clusters and set the respective clustered soil data.
#'
#' @param soil_data Saved soil information in the soil_object.
#' @param n_class Numeric value of final number of classes.

#' @importFrom tibble add_column tibble
#' @importFrom dplyr group_by summarise_all
#' @importFrom purrr map
#' @importFrom magrittr %>%
#'
#' @keywords internal

set_cluster_data <- function(soil_data, n_class) {

  soil_clust <- soil_data$soil_cluster[["n"%_%n_class]]$cluster

  data_proc <- soil_data$data_processed %>%
    map(., function(tbl, add_col){ add_column(tbl, soil_class = add_col) %>%
                                     group_by(soil_class) %>%
                                     summarise_all(mean)}, soil_clust)

  data_proc$soil_class <- tibble(soil_class = soil_clust)

  return(data_proc)
}
