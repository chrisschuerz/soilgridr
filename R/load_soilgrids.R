#' Load soilgrids
#'
#' Loads a set of layers (defined by \code{layer_names} character-vector) from
#'  \href{https://soilgrids.org/}{soilgrids} into the \strong{soil project}
#'  (this includes files in the defined path and the R environment, see:
#'  \code{\link{new_soil_project}}).
#'
#' @param project_path Path to the soil project.
#' @param shape_file Shape file of the study area.
#' @param layer_names Character vector for defining the soilgrids layers to load, e.g "BDRICM_M_250m".

#' @importFrom dplyr bind_cols ends_with filter select %>%
#' @importFrom lubridate now
#' @importFrom purrr map map_at set_names
#' @importFrom raster raster projectRaster crop mask dataType getValues
#' @importFrom tibble as_tibble
#'
#' @keywords internal

load_soilgrids <- function(project_path,
                           shape_file,
                           layer_table) {
  # Reading soilgrids layer and arranging them in list --------------------
  # Define path to soilgrids layer
  tif_path <- project_path%//%"soil_layer"

  layer_names <- apply(layer_table, 1, paste, collapse = "_")

  # Initiate list with soil data
  soil_value_list <- list()
  soil_layer_list <- list()

  t0 <- now()
  # Loop over all layers and arrange them in list
  for(i_layer in 1:length(layer_names)) {
    layer_i <- layer_names[i_layer]
    ## Read layer, clip with shape file and convert values from raster into table
    layer_tmp <- raster(tif_path%//%layer_i%.%"tif") %>% # add data-suffix (assumed .tif)
      set_nodata(.) %>%
      project_if(., shape_file$shape) %>%
      mask_if(., shape_file$shape, shape_file$shape_from_extent)

    # In first loop-run create meta data to save in final output list
    if(length(soil_value_list) == 0){
      layer_meta <- list(has_value = getValues(layer_tmp),
                         len_rst   = length(layer_tmp),
                         dim_rst   = dim(layer_tmp),
                         extent    = shape_file$extent,
                         crs       = shape_file$crs)
      layer_meta$has_value[!is.na(layer_meta$has_value)] <- TRUE
    }
    # Convert raster layer to tibble with one named column for further merging.
    tbl_tmp <- layer_tmp %>%
                 as.vector(.) %>%
                 as_tibble(.) %>%
                 filter(!is.na(.[,1])) %>%
                 set_names(.,layer_table$variable[i_layer]%_%layer_table$quantile[i_layer])

    ## Add the extracted columns into list
    soil_value_list[[layer_i]] <- tbl_tmp
    soil_layer_list[[layer_i]] <- layer_tmp

    display_progress(i_layer, length(layer_names), t0, "Layer")
  }
  finish_progress(length(layer_names), t0, "Finished reading", "layer")

  depth_lbl <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")
  layer_nr <- map_int(layer_table$depth, ~which(depth_lbl == .x))
  unique_layer_nr <- unique(layer_nr)

  soil_data <- map(unique_layer_nr,
                   ~bind_cols(soil_value_list[which(layer_number == .x)])) %>%
    set_names(paste0("sl",1:length(unique_layer_nr), ": ", depth_lbl[unique_layer_nr]))




  # Output with soil layer data, cluster results and spatial meta data
  out_list <- list(soil_data   = soil_data,
                   soil_raster = soil_layer_list,
                   layer_meta  = layer_meta)

  return(out_list)
}

# Deprecated used for mixed variables (with and without soil depths)

  # # Derive the available soil depths
  # names_soil_value <- names(soil_value_list)
  # sl_label <- gsub(".*sl","",names_soil_value) %>%
  #   gsub("\\D","",.) %>% # make sure empy char is returned if no soil-depth are available
  #   .[which(nchar(.) == 1)]
  # # Get names of layer that have no separation in depth
  # other_lgc <-  gsub(".*sl","",names_soil_value) %>%
  #   gsub("\\D","",.) %>%
  #   nchar(.) == 0
  #
  # # go through different options and compute soil_tbl_list:
  # if ( (sum(other_lgc) >= 1) & (length(sl_label) == 0) ) {
  #   other_lbl <- names(soil_value_list)[other_lgc]
  #
  #   soil_tbl_list <- soil_value_list[other_lbl] %>%
  #     map(., function(x){x %>% set_colnames(tolower(colnames(x)))}) %>%
  #     set_names(tolower(other_lbl))
  # } else if ( (sum(other_lgc) == 0) & (length(sl_label) >= 1) ) {
  #   soil_tbl_list <- soil_value_list %>%
  #     bind_cols(.) %>%
  #     as_tibble(.) %>%
  #     set_colnames(tolower(colnames(.))) %>%
  #     map(unique(sl_label), function(x, tbl){select(tbl,ends_with(x))}, .) %>%
  #     set_names(c("sl"%&%unique(sl_label))) %>%
  #     map_at(., "sl"%&%1:7, function(tbl){names(tbl) <- substr(names(tbl), 1, 6)
  #     return(tbl)})
  # } else if ( (sum(other_lgc) >= 1) & (length(sl_label) >= 1) ) {
  #   other_lbl <- names(soil_value_list)[other_lgc]
  #
  #   other_list <- soil_value_list[other_lbl] %>%
  #     map(., function(x){x %>% set_colnames(tolower(colnames(x)))}) %>%
  #     set_names(tolower(other_lbl))
  #
  #   # Group soil layers according to layer depth
  #   soil_tbl_list <- soil_value_list %>%
  #     bind_cols(.) %>%
  #     as_tibble(.) %>%
  #     set_colnames(tolower(colnames(.))) %>%
  #     map(unique(sl_label), function(x, tbl){select(tbl,ends_with(x))}, .) %>%
  #     set_names(c("sl"%&%unique(sl_label))) %>%
  #     map_at(., "sl"%&%1:7, function(tbl){names(tbl) <- substr(names(tbl), 1, 6)
  #     return(tbl)}) %>%
  #     c_if(., !is.null(other_list), other_list)
  # } else {
  #   stop("could not aggregate layers (with or withouth depth)")
  # }
  #
