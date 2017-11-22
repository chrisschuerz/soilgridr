#' Title
#'
#' @param project_path
#' @param sg_cluster
#' @param n_class
#'
#' @importFrom tibble enframe
#' @importFrom dplyr mutate
#' @importFrom raster writeRaster extent<-
#' @importMethodsFrom raster extent
#' @importFrom rgdal writeGDAL
#' @importFrom magrittr %>%
#' @importFrom pasta %//%
#' @import sp
#'
#' @return
#' @export
#'
#' @examples
write_SWATsoil <- function(project_path, sg_cluster, n_class,
                           overwrite = FALSE){

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

  clust_g <- as(clust_rst, 'SpatialGridDataFrame')

  write_path <- project_path%//%"soil_out"
  if(!dir.exists(project_path%//%"soil_out")) {
    dir.create(write_path)
  } else if(!overwrite) {
      stop("Folder 'soil_out' already exists and overwrite is set FALSE!")
    }
  # writeRaster(clust_rst, filename = write_path%//%"soil.tif",
  #             datatype = "INT4S", format = "GTiff")

  writeGDAL(dataset = clust_g, fname =  write_path%//%"soil.tif",
               drivername = "GTiff", type = "Int16", mvFlag = -32768)

  return(clust_rst)

}
