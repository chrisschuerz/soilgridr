#' Title
#'
#' @param project_path
#' @param shp_file

#' @importFrom rgdal readOGR
#' @importFrom raster raster projectRaster crop mask crs extent
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter
#' @importFrom magrittr %>% set_colnames
#'
#'
#'
#'
#' @return
#' @export
#'
#' @examples
cluster_soilgrids <- function(project_path, shp_file = NULL ) {

  # Define path where soilgrids layer are located
  lyr_dir <- project_path%//%"soilgrids"

  # if no shp file provided subs1 shape frome SWAT watershed delineation used.
  if(is.null(shp_file)) {
    shp_file <- readOGR(dsn = project_path%//%"Watershed/shapes"%//%
                          "subs1.shp",
                        layer = "subs1")
  } else if(is.character(shp_file)){
    lyr <- strsplit(shp_file, "\\/|\\\\|\\.")[[1]]
    lyr <- lyr[length(lyr) - 1]
    shp_file <- readOGR(dsn = shp_file,
                        layer = lyr)
  }


  # Read all obtained soilgrids raster and arrange them in a data frame
  lyr_list <-  list.files(lyr_dir, pattern = "_250m.tif$")
  soil_tbl <- list()

  ## Helper function to extract all wanted text strings
  select_label <- function(text) {
    text[text %in% c("BDRICM", "BLDFIE", "CECSOL", "CLYPPT",
                     "CRFVOL", "ORCDRC", "PHIHOX", "SLTPPT",
                     "SNDPPT", "sl"%&%1:7)]
  }

  ## Helper function to set the nodata values in the loaded raster files
  set_nodata <- function(rst) {
    if (rst@data@min == 0 & rst@data@max == 255) {
      rst@file@nodatavalue <- 255
    } else {
      rst@file@nodatavalue <- -32768
    }
    return(rst)
  }

  # Loop over all layers and arrange them in tibble
  for(lyr_i in lyr_list) {
    name_i <- lyr_i %>%
      strsplit(., "_") %>%
      unlist() %>%
      select_label(.) %>%
      substr(., 1, 3) %>%
      paste(., collapse = "_")
    lyr_tmp <- raster(lyr_dir%//%lyr_i) %>%
      set_nodata(.) %>%
      projectRaster(., crs = crs(shp_file)) %>%
      crop(., extent(shp_file)) %>%
      mask(., shp_file) %>%
      as.vector(.) %>%
      as_tibble() %>%
      filter(!is.na(.[,1])) %>%
      set_colnames(name_i)
    soil_tbl[[name_i]] <- lyr_tmp
  }

  soil_tbl %<>%
    bind_cols() %>%
    as_tibble() %>%
    set_colnames(tolower(colnames(.)))


}
