#' Title
#'
#' @param project_path
#' @param sg_cluster
#' @param n_class
#'
#' @importFrom tibble enframe add_column tibble
#' @importFrom dplyr mutate select filter group_by summarise_all funs bind_cols
#' @importFrom purrr map map2 map_df reduce
#' @importFrom raster writeRaster extent<-
#' @importMethodsFrom raster extent
#' @importFrom rgdal writeGDAL
#' @importFrom euptf psd2classUS
#' @importFrom magrittr %>% multiply_by set_names
#' @importFrom pasta %//% %_%
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

  sol_lyr <- sg_cluster$soil_layer
  bdr <-  sol_lyr$bdr %>%
    add_column(., class = clust_sel$value) %>%
    group_by(., class) %>%
    summarise_all(., funs(mean))

  sol_lyr$bdr <- NULL

  z_max <- sg_cluster$layer_meta$depth %>%
    set_names("lyr"%_%1:length(.)) %>%
    map_df(., ~rep(.x, nrow(bdr))) %>%
    multiply_by((. < bdr$bdr)) %>%
      apply(., 1, function(x){
                    if((sum(x != 0) + 2) <= length(x)){
                      x[(sum(x != 0) + 2):length(x)] <- NA
                    }
                    return(x)}) %>%
    t(.)
  z_max <- (z_max + ((!is.na(z_max) & z_max == 0) * bdr$bdr)) %>%
    as.data.frame(.) %>%
    as.list(.)


  sol_lyr  %<>%
    # map(., ~ as_tibble(.x)) %>%
    map(., ~ add_column(.x, class = clust_sel$value)) %>%
    map(., ~ group_by(.x, class)) %>%
    map(., ~ summarise_all(.x, funs(mean))) %>%
    map(., function(x){x %>%
             mutate(tex = psd2classUS(snd, slt, cly, orc, option=TRUE))}) %>%
    map2(., z_max, ~ add_column(.x, z = .y * 10))

  assign_hydgrp <- function(k_s){
    hyd_grp <- c("D","C","B","A")
    hyd_trs <- c(3.6,36,144,9999)
    lapply(k_s, function(x) hyd_grp[hyd_trs > x][1]) %>% unlist()
  }


arrange_lyr_i <- function(lyr_tbl) {
  lyr_tbl %>%
    select(class, z, bld, awc, k_s, orc, cly, slt, snd, crf) %>%
    mutate(alb    = 0.6/exp(0.4*orc),
           usle_k = ((0.2 + 0.3*exp(-0.256*snd * (1 - slt/100)))) *
             ((slt/(cly + slt))^0.3) *
             (1 - 0.0256*orc / (orc + exp(3.72 - 2.95*orc))) *
             ((1 - 0.7*(1 - snd/100) /
                 ((1 - snd/100) + exp(-5.51 + 22.9*(1 - snd/100))))),
           ec     = 0,
           k_s = (10^k_s)/2.4)
}

gen <- tibble(objectid  = 1:nrow(sol_lyr[[1]]),
              muid      = rep("", nrow(sol_lyr[[1]])),
              seqn      = rep("", nrow(sol_lyr[[1]])),
              snam      = "soilclass"%&%sol_lyr[[1]]$class,
              s5id      = rep("", nrow(sol_lyr[[1]])),
              cmppct    = rep("", nrow(sol_lyr[[1]])),
              nlayers   = length(sol_lyr),
              hydgrp    = assign_hydgrp(((10^sol_lyr$lyr_1$k_s)/2.4)),
              sol_zmx   = round(bdr$bdr, digits = 2),
              anion_exc = 0.5,
              sol_crk   = 0.5,
              texture   = map_dfc(sol_lyr, ~select(.x, tex)) %>%
                            apply(.,1, paste, collapse = "_"))

sol_lyr %<>%
  map(.,~ filter(.x,!is.na(z))) %>%
  map(.,  ~ arrange_lyr_i(.x)) %>%
  map(., round, digits = 2) %>%
  reduce(., full_join, by = "class") %>%
  select(- class)

sol_lyr[is.na(sol_lyr)] <- 0

sol_out <- bind_cols(gen, bind_cols(sol_lyr)) %>%
  bind_cols(., data.frame(matrix(0, nrow = nrow(.), ncol = 152 - ncol(.))))

write.table(sol_out, file = write_path%//%"usersoil.csv", quote = FALSE,
            row.names = FALSE, col.names = FALSE, sep = ",")

sol_lkp <- tibble(VALUE = substr(gen$snam, 10, 20),
                  NAME  = gen$snam)

write.table(sol_lkp, file = write_path%//%"sol_lkp.csv", quote = FALSE,
            row.names = FALSE, col.names = TRUE, sep = ",")
}
