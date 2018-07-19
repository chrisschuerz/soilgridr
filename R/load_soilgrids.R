#' Load soilgrids
#'
#' Loads a set of layers (defined by \code{layer_names} character-vector) from
#'  \href{https://soilgrids.org/}{soilgrids}} into the \strong{soil project}
#'  (this includes files in the defined path and the R environment, see:
#'  \code{\link{new_soil_project}}).
#'
#' @param project_path Path to the soil project.
#' @param shape_file Shape file of the study area.
#' @param layer_names Character vector for defining the soilgrids layers to load, e.g "BDRICM_M_250m".

#' @importFrom dplyr bind_cols ends_with filter progress_estimated select
#' @importFrom magrittr %>% set_colnames set_names
#' @importFrom purrr map map_at
#' @importFrom raster raster projectRaster crop mask
#' @importFrom tibble as_tibble

load_soilgrids <- function(project_path,
                           shape_file,
                           layer_names) {
  # Reading soilgrids layer and arranging them in list --------------------
  # Define path to soilgrids layer
  lyr_dir <- project_path%//%"soil_layer"

  ## Helper function to set the nodata values in the loaded raster files
  ## should be defined as seperate function outside :(
  set_nodata <- function(rst) {
    if (rst@data@min == 0 & rst@data@max == 255) {
      rst@file@nodatavalue <- 255
    } else {
      rst@file@nodatavalue <- -32768
    }
    return(rst)
  }

  ## Function to mask raster only when shape file is available
  mask_if <- function(rst, shp, shp_from_ext){
    if(!shp_from_ext) rst <- mask(rst, shp)
    return(rst)
  }

  # Initiate list with soil data
  sol_val_list <- list()
  sol_lyr_list <- list()

  ## Add progress bar to loop
  pb <- progress_estimated(length(layer_names))

  # Loop over all layers and arrange them in list
  for(lyr_i in layer_names) {
    ## Extract label for the respective layer in final table
    name_i <- lyr_i %>%
      gsub("M|.tif|250m|ll","",.) %>% # remove bad-listed characters
      gsub("_+$","",.) %>%            # remove _ at end of string
      gsub("_+","_",.)                # remove follow-up -

    ## Read layer, clip with shape file and convert values from raster into table
    lyr_tmp <- raster(lyr_dir%//%lyr_i%.%"tif") %>% # add data-suffix (assumed .tif)
      set_nodata(.) %>%
      projectRaster(., crs = shape_file$crs) %>%
      crop(., shape_file$extent) %>%
      mask_if(., shape_file$shape, shape_file$shape_from_extent)

    # In first loop-run create meta data to save in final output list
    if(length(sol_val_list) == 0){
      lyr_meta <- list(has_value = lyr_tmp@data@values,
                       len_rst   = length(lyr_tmp),
                       dim_rst   = dim(lyr_tmp),
                       extent    = shape_file$extent,
                       crs       = shape_file$crs)
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

  # Derive the available soil depths
  names_sol_val <- names(sol_val_list)
  sl_lbl <- gsub(".*sl","",names_sol_val) %>%
    gsub("\\D","",.) %>% # make sure empy char is returned if no soil-depth are available
    .[which(nchar(.) == 1)]
  # Get names of layer that have no separation in depth
  other_lgc <-  gsub(".*sl","",names_sol_val) %>%
    gsub("\\D","",.) %>%
    nchar(.) == 0

  # go through different options and compute sol_tbl_list:
  if ( (sum(other_lgc) >= 1) & (length(sl_lbl) == 0) ) {
    other_lbl <- names(sol_val_list)[other_lgc]

    sol_tbl_list <- sol_val_list[other_lbl] %>%
      map(., function(x){x %>% set_colnames(tolower(colnames(x)))}) %>%
      set_names(tolower(other_lbl))
  } else if ( (sum(other_lgc) == 0) & (length(sl_lbl) >= 1) ) {
    sol_tbl_list <- sol_val_list %>%
      bind_cols(.) %>%
      as_tibble(.) %>%
      set_colnames(tolower(colnames(.))) %>%
      map(sl_lbl, function(x, tbl){select(tbl,ends_with(x))}, .) %>%
      set_names(c("sl"%&%sl_lbl)) %>%
      map_at(., "sl"%&%1:7, function(tbl){names(tbl) <- substr(names(tbl), 1, 6)
      return(tbl)})
  } else if ( (sum(other_lgc) >= 1) & (length(sl_lbl) >= 1) ) {
    other_lbl <- names(sol_val_list)[other_lgc]

    other_list <- sol_val_list[other_lbl] %>%
      map(., function(x){x %>% set_colnames(tolower(colnames(x)))}) %>%
      set_names(tolower(other_lbl))

    # Group soil layers according to layer depth
    sol_tbl_list <- sol_val_list %>%
      bind_cols(.) %>%
      as_tibble(.) %>%
      set_colnames(tolower(colnames(.))) %>%
      map(sl_lbl, function(x, tbl){select(tbl,ends_with(x))}, .) %>%
      set_names(c("sl"%&%sl_lbl)) %>%
      map_at(., "sl"%&%1:7, function(tbl){names(tbl) <- substr(names(tbl), 1, 6)
      return(tbl)}) %>%
      c_if(., !is.null(other_list), other_list)
  } else {
    stop("could not aggregate layers (with or withouth depth)")
  }




  # Output with soil layer data, cluster results and spatial meta data
  out_list <- list(soil_list   = sol_tbl_list,
                   soil_raster = sol_lyr_list,
                   layer_meta  = lyr_meta)

  return(out_list)
}

