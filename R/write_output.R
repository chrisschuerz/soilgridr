#' Write the required SWAT2012 soil inputs for clustered soilgrids
#'
#' @param soil_data soil data.
#' @param format tif, ascii etc...
#' @param overwrite Logical
#'
#' @importFrom tibble tibble add_column
#' @importFrom dplyr filter bind_cols
#' @importFrom purrr map map_chr map2 map2_dfc walk2
#' @importFrom raster raster extent extent<-
#' @importMethodsFrom raster extent
#' @importFrom rgdal writeGDAL
#' @importFrom magrittr %>% %<>% set_colnames
#' @importFrom pasta %//% %_% %&%
#' @importFrom sp SpatialGridDataFrame
#'
#' @return Writes required aggregated SWAT soil data to folder project_path/soil_out


write_output <- function(soil_data, format, overwrite = FALSE) {

  if(dir.exists(soil_data$meta$project_path%//%"output") & !overwrite){
    stop("Output allready written for this project. For overwriting set overwrite = TRUE.")
  }
  unlink(soil_data$meta$project_path%//%"output")
  dir.create(soil_data$meta$project_path%//%"output")

  file_suffix <- tibble(suffix = c("ascii", "tif"),
                        driver = c("AAIGrid", "GTiff"))


  # Create layer table that should be exported as raster layers
  suffix <- "_"%&%names(soil_data$data_processed)
  suffix[!(suffix %in% ("_sl"%&%1:100))] <- ""



  if("soil_class" %in% names(soil_data)){
    suffix <- suffix[1:(length(suffix) - 1)]
    soil_class <- soil_data$data_processed[[1]]$soil_class

    raster_tbl <- soil_data$data_processed$soil_class
    class_tbl  <- soil_data$data_processed[
      names(soil_data$data_processed) != "soil_class"] %>%
      map2_dfc(., suffix, function(tbl, sfx){
        tbl %<>%
          select(.,- soil_class) %>%
          set_colnames(., colnames(.)%&%sfx)}) %>%
      add_column(., soil_class = soil_class, .before = 1)

    write.csv(class_tbl,
              file = soil_data$meta$project_path%//%"output"%//%"soil_class.csv",
              quote = FALSE, row.names = FALSE)

  } else {
    raster_tbl <- soil_data$data_processed %>%
      map2(., suffix, function(tbl, nm) {
        names(tbl) <- names(tbl)%&%nm
        return(tbl)}) %>%
      bind_cols(.)
  }


  data_type <- map_chr(raster_tbl, typeof)

  rasterize <- function(soil_vct, meta) {
    # Assign the cluster group indices to vector with length of the final raster map.
    out_rst <- rep(NA, meta$len_rst)
    out_rst[which(!is.na(meta$has_value))] <- soil_vct

    # Reshape the vector and create raster map.
    out_rst %<>%
      matrix(ncol = meta$dim_rst[1],
             nrow = meta$dim_rst[2]) %>%
      t() %>%
      raster(crs = meta$crs)

    # Assign the shape files' extent and provide a nodata value
    extent(out_rst) <- meta$extent
    out_rst@file@nodatavalue <- -32768

    out_rst <- as(out_rst, 'SpatialGridDataFrame')

    return(out_rst)
  }

  write_out <- function(rst, layer_name, type, format, driver){
    type <- type[layer_name]
    flag <- ifelse(type == "integer", -32768, NA)
    out_type <- ifelse(type == "integer", "Int16", "Float32")

    writeGDAL(dataset = rst,
              fname = soil_data$meta$project_path%//%"output"%//%layer_name%.%format,
              drivername = driver,
              type = out_type,
              mvFlag = flag)
  }

  raster_list <- map(raster_tbl, rasterize, soil_data$soilgrids$meta$layer)

  driver_name <- file_suffix %>%
    filter(suffix == format) %>%
    .[[2]]
  walk2(raster_list, names(data_type), write_out,
        type = data_type, format = format, driver = driver_name)
}
