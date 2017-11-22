#' Plot clustered soil map
#'
#' @param soilgrids_cluster
#' @param n_class
#'
#' @import raster
#' @importFrom tibble enframe
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom pasta %//% %_%

#'
#' @return
#' @export
#'
#' @examples
plot_clustersoil <- function(soilgrids_cluster, n_class){

  clust_sel <- soilgrids_cluster$soil_cluster[["n"%_%n_class]]$cluster %>%
    enframe() %>%
    mutate(name = name %>% gsub("cell_", "", .) %>% as.numeric(.))

  clust_vct <- rep(NA, soilgrids_cluster$layer_meta$len_rst)
  clust_vct[which(!is.na(soilgrids_cluster$layer_meta$has_value))] <- clust_sel$value

  clust_rst <- clust_vct %>%
    matrix(ncol = soilgrids_cluster$layer_meta$dim_rst[1],
           nrow = soilgrids_cluster$layer_meta$dim_rst[2]) %>%
    t() %>%
    raster(crs = soilgrids_cluster$layer_meta$crs)

  extent(clust_rst) <- soilgrids_cluster$layer_meta$extent
  clust_rst@file@nodatavalue <- -32768

  plot(clust_rst)
}
