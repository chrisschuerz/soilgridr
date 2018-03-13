#' Create clustered soilmap from soilgrids data
#'
#' @param project_path Path to the SWAT project / Path where downloaded
#'   soilgrids folder is located.
#' @param shape_file Shape file (or path to shape file) for which an
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


load_soilgrids <- function(project_path, shape_file, layer_names) {
  # Reading soilgrids layer and arranging them in list ---------------------------
  # Define path to soilgrids layer
  lyr_dir <- project_path%//%"soil_layer"

  # List all layer names including file sufix
  lyr_list <-  layer_names%_%"250m.tif"

  ## Helper function to extract all wanted text strings
  select_label <- function(text, layer_names) {
    layer_names <- substr(layer_names, 1, 6)
    text[text %in% c(layer_names, "sl"%&%1:7, "sd"%&%1:7)]
  }

  ## Helper function to set the nodata values in the loaded raster files
  # should be defined as seperate function outside :(
  set_nodata <- function(rst) {
    if (rst@data@min == 0 & rst@data@max == 255) {
      rst@file@nodatavalue <- 255
    } else {
      rst@file@nodatavalue <- -32768
    }
    return(rst)
  }

  # Initiate list with soil data
  sol_val_list <- list()
  sol_lyr_list <- list()

  ## Add progress bar to loop
  pb <- progress_estimated(length(lyr_list))

  # Loop over all layers and arrange them in list
  for(lyr_i in lyr_list) {
    ## Extract label for the respective layer in final table
    name_i <- lyr_i %>%
      strsplit(., "_") %>%
      unlist(.) %>%
      # select_label(.) %>%
      substr(., 1, 6) %>%
      paste(., collapse = "_")

    ## Read layer, clip with shape file and convert values from raster into table
    lyr_tmp <- raster(lyr_dir%//%lyr_i) %>%
      set_nodata(.) %>%
      projectRaster(., crs = crs(shape_file)) %>%
      crop(., extent(shape_file)) %>%
      mask(., shape_file)

    # In first loop-run create meta data to save in final output list
    if(length(sol_list) == 0){
      lyr_meta <- list(has_value = lyr_tmp@data@values,
                       len_rst   = length(lyr_tmp),
                       dim_rst   = dim(lyr_tmp),
                       extent    = extent(shape_file),
                       crs       = crs(shape_file))
      lyr_meta$has_value[!is.na(lyr_meta$has_value)] <- TRUE
    }
    # Convert raster layer to tibble with one named column for further merging.
    tbl_tmp <- lyr_tmp %>%
                 as.vector(.) %>%
                 as_tibble(.) %>%
                 filter(!is.na(.[,1])) %>%
                 set_colnames(.,name_i)

    ## Add the extracted columns into list
    sol_val_list[[name_i]] <- tbl_tmp
    sol_lyr_list[[name_i]] <- lyr_tmp

    pb$tick()$print()
  }
  pb$stop()

  # Group soil layers according to layer depth and calculate further parameters
  sol_tbl <- sol_val_list %>%
    bind_cols(.) %>%
    as_tibble(.) %>%
    set_colnames(tolower(colnames(.)))


  # Output with soil layer data, cluster results and spatial meta data
  out_list <- list(soil_table  = sol_tbl,
                   soil_list   = sol_val_list,
                   soil_raster = sol_lyr_list,
                   layer_meta  = lyr_meta)
  return(out_list)
}
