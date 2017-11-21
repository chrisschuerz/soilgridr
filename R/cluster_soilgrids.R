#' Title
#'
#' @param project_path
#' @param shp_file

#' @importFrom rgdal readOGR
#' @importFrom raster raster projectRaster crop mask crs extent
#' @importFrom tibble as_tibble
#' @importFrom dplyr select filter progress_estimated
#' @importFrom magrittr %>% set_colnames
#'
#'
#'
#'
#' @return
#' @export
#'
#' @examples
cluster_soilgrids <- function(project_path, shp_file = NULL,
                              lower_bound = c(30, 100, 200) ) {


# Reading shape file -----------------------------------------------------------
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


# Reading soilgrids layer and arranging them in list ---------------------------
  # Define path where soilgrids layer are located
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
  set_nodata <- function(rst) {
    if (rst@data@min == 0 & rst@data@max == 255) {
      rst@file@nodatavalue <- 255
    } else {
      rst@file@nodatavalue <- -32768
    }
    return(rst)
  }

  # Initiate list with soil data
  soil_list <- list()

  ## Add progress bar to loop
  pb <- progress_estimated(length(lyr_list))
  pb$print()

  # Loop over all layers and arrange them in list
  for(lyr_i in lyr_list) {
    ## Extract label for the respective layer in final table
    name_i <- lyr_i %>%
      strsplit(., "_") %>%
      unlist() %>%
      select_label(.) %>%
      substr(., 1, 3) %>%
      paste(., collapse = "_")

    ## Read layer, clip with shape file and convert values from raster into table
    lyr_tmp <- raster(lyr_dir%//%lyr_i) %>%
      set_nodata(.) %>%
      projectRaster(., crs = crs(shp_file)) %>%
      crop(., extent(shp_file)) %>%
      mask(., shp_file) %>%
      as.vector(.) %>%
      as_tibble() %>%
      filter(!is.na(.[,1])) %>%
      set_colnames(name_i)

    ## Add the extracted columns into list
    soil_list[[name_i]] <- lyr_tmp

    pb$tick()
  }


# Calculate additional soil parameters using the euptf package -----------------
  # Group soil layers according to layer depth and calculate further parameters
  soil_list <- soil_tbl %>%
    bind_cols() %>%
    as_tibble() %>%
    set_colnames(tolower(colnames(.))) %>%
    map(c("bdr","sl"%&%1:7), function(x, tbl){select(tbl,ends_with(x))}, .) %>%
    set_names(c("bdr","sl"%&%1:7)) %>%
    map_at(., "sl"%&%1:4, function(tbl){mutate(tbl, TOPSOIL = "top")}) %>%
    map_at(., "sl"%&%5:7, function(tbl){mutate(tbl, TOPSOIL = "sub")}) %>%



  sol_lyr <- list()

  for(i_lyr in 1:7){
    sol_lyr[["lyr"%_%i_lyr]] <- rst_tbl %>%
      bind_cols() %>%
      dplyr::select(ends_with(as.character(i_lyr))) %>%
      set_colnames(substr(colnames(.),1,3)) %>%
      mutate(TOPSOIL = ifelse(i_lyr <= 4, "top", "sub")) %>%
      as_tibble()
  }

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
      dplyr::select(TOPSOIL, USSAND, USSILT, USCLAY, OC, BD, CRF, PH_H2O, CEC) %>%
      mutate(th_s  = predict.ptf(., ptf = "PTF06"),
             th_fc = predict.ptf(., ptf = "PTF09"),
             th_wp = predict.ptf(., ptf = "PTF12"),
             k_s   = predict.ptf(., ptf = "PTF17"),
             awc   = th_fc - th_wp) %>%
      rename(snd = USSAND,
             slt = USSILT,
             cly = USCLAY,
             orc = OC,
             bld = BD,
             crf = CRF,
             phi = PH_H2O,
             cec = CEC) %>%
      dplyr::select(-TOPSOIL)
  }

  sol_lyr %<>% lapply(., calc_solpar)

  sol_lyr$lyr_0_30   <- ((sol_lyr$lyr_1*2.5 + sol_lyr$lyr_2*7.5 +
                            sol_lyr$lyr_3*12.5 + sol_lyr$lyr_4*7.5) / 30) %>%
    as_tibble()

  sol_lyr$lyr_30_100 <- ((sol_lyr$lyr_4*15 + sol_lyr$lyr_5*35 +
                            sol_lyr$lyr_6*20) / 70) %>%
    as_tibble()

  sol_lyr$lyr_100_200 <- ((sol_lyr$lyr_6*50 + sol_lyr$lyr_7*50) / 100) %>%
    as_tibble()


  sol_lyr <- sol_lyr[c("lyr_0_30", "lyr_30_100", "lyr_100_200")]

  sol_lyr %<>% lapply(., function(x){
    x %>%
      add_column(class = clust_14$value) %>%
      group_by(class) %>%
      summarise_all(funs(mean)) %>%
      mutate(tex = psd2classUS(snd, slt, cly, orc, option=TRUE))})

  sol_lyr$lyr_0_30    %<>% add_column(z = 300)
  sol_lyr$lyr_30_100  %<>% add_column(z = 1000)
  sol_lyr$lyr_100_200 %<>% add_column(z = rst_tbl$zmax %>%
                                        add_column(class = clust_14$value) %>%
                                        filter(!is.na(class)) %>%
                                        group_by(class) %>%
                                        summarise_all(funs(mean)) %>%
                                        .[[2]] %>%
                                        multiply_by(10))

  assign_hydgrp <- function(k_s){
    hyd_grp <- c("D","C","B","A")
    hyd_trs <- c(3.6,36,144,9999)
    lapply(k_s, function(x) hyd_grp[hyd_trs > x][1]) %>% unlist()
  }

  arrange_lyr_i <- function(lyr_tbl) {
    lyr_tbl %>%
      dplyr::select(z, bld, awc, k_s, orc, cly, slt, snd, crf) %>%
      mutate(alb    = 0.6/exp(0.4*orc),
             usle_k = ((0.2 + 0.3*exp(-0.256*snd * (1 - slt/100)))) *
               ((slt/(cly + slt))^0.3) *
               (1 - 0.0256*orc / (orc + exp(3.72 - 2.95*orc))) *
               ((1 - 0.7*(1 - snd/100) /
                   ((1 - snd/100) + exp(-5.51 + 22.9*(1 - snd/100))))),
             ec     = 0,
             k_s = (10^k_s)/2.4)
  }

}
