#' Create clustered soilmap from soilgrids data
#'
#' @param project_path Path to the SWAT project / Path where downloaded
#'   soilgrids folder is located.
#' @param shp_file Shape file (or path to shape file) for which an
#'   aggregated soil map should be generated. If \code{NULL} the shape file
#'   from the SWAT project will be used.
#' @param lower_bound A vector defining the lower bounds of the aggregated
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
#' @return A list that holds the soilgrids data and the clustering
#'   results for further processing with \code{write_SWATsoil()}.
#' @export

cluster_soilgrids <- function(project_path, shp_file = NULL,
                              lower_bound = c(30, 100, 200),
                              n_class = 1:20 ) {
# Reading shape file -----------------------------------------------------------
  # if no .shp file provided, use subs1.shp from SWAT watershed delineation
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


# Reading soilgrids layer and arranging them in list ---------------------------
  # Define path to soilgrids layer
  lyr_dir <- project_path%//%"soilgrids"

  # List all layer names
  lyr_list <-  list.files(lyr_dir, pattern = "_250m.tif$")

  ## Helper function to extract all wanted text strings
  select_label <- function(text) {
    text[text %in% c("BDRICM", "BLDFIE", "CECSOL", "CLYPPT",
                     "CRFVOL", "ORCDRC", "PHIHOX", "SLTPPT",
                     "SNDPPT", "sl"%&%1:7)]
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
  sol_list <- list()

  ## Add progress bar to loop
  cat("Read soilgrids layer:\n")
  pb <- progress_estimated(length(lyr_list))

  # Loop over all layers and arrange them in list
  for(lyr_i in lyr_list) {
    ## Extract label for the respective layer in final table
    name_i <- lyr_i %>%
      strsplit(., "_") %>%
      unlist(.) %>%
      select_label(.) %>%
      substr(., 1, 3) %>%
      paste(., collapse = "_")

    ## Read layer, clip with shape file and convert values from raster into table
    lyr_tmp <- raster(lyr_dir%//%lyr_i) %>%
      set_nodata(.) %>%
      projectRaster(., crs = crs(shp_file)) %>%
      crop(., extent(shp_file)) %>%
      mask(., shp_file)

    # In first loop-run create meta data to save in final output list
    if(length(sol_list) == 0){
      lyr_meta <- list(has_value = lyr_tmp@data@values,
                       len_rst   = length(lyr_tmp),
                       dim_rst   = dim(lyr_tmp),
                       extent    = extent(shp_file),
                       crs       = crs(shp_file),
                       depth     = lower_bound)
      lyr_meta$has_value[!is.na(lyr_meta$has_value)] <- TRUE
    }
    # Convert raster layer to tibble with one named column for further merging.
    lyr_tmp %<>%
      as.vector(.) %>%
      as_tibble(.) %>%
      filter(!is.na(.[,1])) %>%
      set_colnames(.,name_i)

    ## Add the extracted columns into list
    sol_list[[name_i]] <- lyr_tmp

    pb$tick()$print()
  }
  pb$stop()


# Calculate additional soil parameters using the euptf package -----------------
  ## Function to calculate additional parameters and return required ones
  calc_solpar <- function(tbl) {
    tbl %>%
      mutate(USSAND = snd,
             USSILT = slt,
             USCLAY  = cly,
             OC      = orc/10,
             BD      = bld/1000,
             CRF     = crf,
             PH_H2O  = phi/10,
             CEC     = cec) %>%
      select(TOPSOIL, USSAND, USSILT, USCLAY, OC, BD, CRF, PH_H2O, CEC) %>%
      mutate(th_s  = predict.ptf(., ptf = "PTF06") %>% as.numeric(.),
             th_fc = predict.ptf(., ptf = "PTF09") %>% as.numeric(.),
             th_wp = predict.ptf(., ptf = "PTF12") %>% as.numeric(.),
             k_s   = predict.ptf(., ptf = "PTF17") %>% as.numeric(.),
             awc   = th_fc - th_wp) %>%
      rename(snd = USSAND,
             slt = USSILT,
             cly = USCLAY,
             orc = OC,
             bld = BD,
             crf = CRF,
             phi = PH_H2O,
             cec = CEC) %>%
      select(-TOPSOIL)
  }

  # Group soil layers according to layer depth and calculate further parameters
  sol_list %<>%
    bind_cols(.) %>%
    as_tibble(.) %>%
    set_colnames(tolower(colnames(.))) %>%
    map(c("bdr","sl"%&%1:7), function(x, tbl){select(tbl,ends_with(x))}, .) %>%
    set_names(c("bdr","sl"%&%1:7)) %>%
    map_at(., "sl"%&%1:7, function(tbl){names(tbl) <- substr(names(tbl), 1, 3)
                                        return(tbl)}) %>%
    map_at(., "sl"%&%1:4, function(tbl){mutate(tbl, TOPSOIL = "top")}) %>%
    map_at(., "sl"%&%5:7, function(tbl){mutate(tbl, TOPSOIL = "sub")}) %>%
    map_at(., "sl"%&%1:7, calc_solpar)


# Aggregate soil layers over depth ---------------------------------------------
  upper_bound <- c(0,lower_bound[1:length(lower_bound) - 1])

  # Function to calculate the weights of each soil layer for the respective
  # upper and lower boundaries in the soil depth aggregation.
  calc_weights <- function(up_bnd, lw_bnd) {
    sl_depth <- c(2.5, 7.5, 12.5, 22.5, 35, 70, 50)
    sl_depth_cum <- cumsum(sl_depth)
    lw_wgt <- (sl_depth_cum - up_bnd)
    lw_pos <- which(lw_wgt >= 0)[1]
    lw_wgt <- (lw_wgt == lw_wgt[lw_pos]) * lw_wgt
    up_wgt <- lw_bnd - sl_depth_cum
    up_pos <- which(up_wgt <= 0)[1]
    up_wgt <- ((up_wgt == up_wgt[up_pos - 1]) * up_wgt)
    up_wgt <- c(0, up_wgt)[1:7]
    if((up_pos - 1) >= (lw_pos + 1)){
      md_wgt <- sl_depth %in% sl_depth[(lw_pos + 1):(up_pos - 1)] * sl_depth
    } else {
      md_wgt <- rep(0, 7)
    }
    (lw_wgt + md_wgt + up_wgt)/(lw_bnd - up_bnd)
  }

  # Calculate the layer weights for each aggregated soil layer
  lyr_weight <- map2(upper_bound, lower_bound, calc_weights)

  # Calculate the aggregated soil layers by summing up the weighted layers
  sol_aggr <- map(lyr_weight, function(weight, sol_list){
                                sol_list$sl1*weight[1] +
                                sol_list$sl2*weight[2] +
                                sol_list$sl3*weight[3] +
                                sol_list$sl4*weight[4] +
                                sol_list$sl5*weight[5] +
                                sol_list$sl6*weight[6] +
                                sol_list$sl7*weight[7]},
                  sol_list) %>%
    set_names("lyr"%_%1:length(upper_bound))

# Soil group clustering using kmeans -------------------------------------------
  # Create scaled table to apply kmeans on
  clst_tbl <- sol_aggr %>%
    map2(., 1:length(upper_bound), function(tbl, nm) {
                                     names(tbl) <- names(tbl)%_%nm
                                     return(tbl)}) %>%
    bind_cols(.) %>%
    bind_cols(., sol_list$bdr) %>%
    scale(., scale = TRUE, center = TRUE) %>%
    as.data.frame(.) %>%
    set_rownames("cell"%_%1:nrow(.))

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

  # Add depth to bedrock layer to the aggregated soil layers
  sol_aggr$bdr <- sol_list$bdr

  # Output with soil layer data, cluster results and spatial meta data
  out_list <- list(soil_layer = sol_aggr,
                   soil_cluster = soil_km,
                   layer_meta = lyr_meta)

  return(out_list)
}
