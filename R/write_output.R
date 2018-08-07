#' Write Soil Project Output
#'
#' Write the modified \href{https://soilgrids.org/}{soilgrids} layer to a file.
#'
#' @param format Output format provided to the \link[rgdal]{writeGDAL} function (tif, ascii, etc.).
#' @name write_output
NULL

#' Write Soil Project Output
#'
#' Internal/Actual soilgrids-writing function.
#'
#' @param soil_data Soil data.
#' @param format Output format provided to the \link[rgdal]{writeGDAL} function (e.g. tif, ascii, etc.).
#' @param overwrite Logical. Defines if potentially existing outputs should overwritten or not.
#'
#' @importFrom dplyr filter bind_cols
#' @importFrom magrittr %>% %<>% set_colnames
#' @importFrom pasta %//% %_% %&%
#' @importFrom purrr map map_chr map2 map2_dfc walk2
#' @importFrom raster raster extent extent<-
#' @importMethodsFrom raster extent
#' @importFrom rgdal writeGDAL
#' @importFrom sp SpatialGridDataFrame
#' @importFrom tibble tibble add_column
#'
#' @return Writes the modified soilgrids layers in the defined format.
#' @keywords internal

write_out <- function(soil_data, variable, sl, format, overwrite) {

  if(!dir.exists(soil_data$meta$project_path%//%"output")) {
    dir.create(soil_data$meta$project_path%//%"output", showWarnings = FALSE)
  }

  file_suffix <- tibble(suffix = c("ascii", "tif"),
                        driver = c("AAIGrid", "GTiff"))

  # Create layer table that should be exported as raster layers
  suffix <- "_"%&%names(soil_data$data_processed)
  suffix[!grepl("_sl", suffix)] <- ""

  if("soil_class" %in% names(soil_data$data_processed)){
    if(file.exists(soil_data$meta$project_path%//%"output/soil_class"%.%format) & !overwrite){
      stop("Soil_class layer was allready written for this project."%&%
             "To overwrite the ouputs set overwrite = TRUE.")
    }

    if(!is.null(variable)) warning("Input 'variable' will be ignored and clustered soilmap is written.\n")
    if(!is.null(sl)) warning("Input 'sl' will be ignored and clustered soilmap is written.\n")

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

    if(is.null(variable)) {
      variable <- map(soil_data$data_processed, names) %>%
        unlist() %>%
        unique()
    }

    if(is.null(sl)) sl <- suffix
    if(is.numeric(sl)) sl <- "_sl"%&%sl
    if(substr(sl[1], 1, 2) == "sl") "_"%&%sl

    name_comb <- expand.grid(var = variable, sl = sl) %>%
      mutate(name_comb = var%&%sl) %>%
      .$name_comb

    name_comb <- name_comb[name_comb %in% names(raster_tbl)]

    if(any(file.exists(soil_data$meta$project_path%//%"output"%//%name_comb%.%format)) & !overwrite){
      stop("At least one of the defined variables was allready written for this project."%&%
           "To overwrite the ouputs set overwrite = TRUE.")
    }

    raster_tbl %<>% select(., one_of(name_comb))
  }

  data_type <- map_chr(raster_tbl, typeof)

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
