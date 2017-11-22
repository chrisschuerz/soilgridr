#' Plot clustered soil map
#'
#' @param sg_cluster
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
plot_soilmap <- function(sg_cluster, n_class){

  clust_sel <- sg_cluster$soil_cluster[["n"%_%n_class]]$cluster %>%
    enframe() %>%
    mutate(name = name %>% gsub("cell_", "", .) %>% as.numeric(.))

  clust_vct <- rep(NA, sg_cluster$layer_meta$len_rst)
  clust_vct[which(!is.na(sg_cluster$layer_meta$has_value))] <- clust_sel$value

  clust_rst <- clust_vct %>%
    matrix(ncol = sg_cluster$layer_meta$dim_rst[1],
           nrow = sg_cluster$layer_meta$dim_rst[2]) %>%
    t() %>%
    raster(crs = sg_cluster$layer_meta$crs)

  extent(clust_rst) <- sg_cluster$layer_meta$extent
  clust_rst@file@nodatavalue <- -32768

  plot(clust_rst)
}
