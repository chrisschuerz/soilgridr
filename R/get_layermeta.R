#' Soilgrids Metainformation
#'
#' Get the required soilgrids meta-information.
#'
#' @param project_path Path where the soil_project is located.
#' @param wcs URL of the ISRIC soilgrids geoserver.
#'
#' @importFrom magrittr %>% %<>% set_names
#' @importFrom pasta %//% %.%
#' @importFrom purrr map
#' @importFrom XML newXMLNode saveXML
#'
#' @return Returns either a list with pixel size and extent of Tiffs as numerics
#'   or the raw text output.

#' @examples
#' # Generate the required input for `obtain_soilgrids()`:
#'   layer_meta <- get_layermeta()
#'   layer_meta
#'
#' # Check all meta information:
#' # (also for crosschecking that function works properly)
#'   get_layermeta(raw = TRUE)
#'
#' @keywords internal

get_layermeta <- function(project_path, wcs) {

  # first layer of the soilgrids data set for which to aqcuire info
  layer <- "BDRICM_M_250m"
  loc   <- newXMLNode("WCS_GDAL")
  loc.s <- newXMLNode("ServiceURL", wcs, parent = loc)
  loc.l <- newXMLNode("CoverageName", layer, parent = loc)
  xml_out <- project_path%//%"soil_layer"%//%layer%.%"xml"
  saveXML(loc, file = xml_out)

  # path to the gdalinfo executable
  path_gdal_info <- ifelse(.Platform$OS.type == "windows",
                           shortPathName("C:/Program files/GDAL")%//%"gdalinfo.exe",
                           "gdalinfo")

  # paste and run gdal command. Store text output in variable
  gdal_cmd <- paste(path_gdal_info, xml_out)
  meta <- system(gdal_cmd, intern = TRUE)

  # Remove loaded .xml file
  file.remove(xml_out)

  # Extract requiered output values
  meta_out <- meta[grepl("Pixel Size|Upper Left|Lower Left|Upper Right|Lower Right ",
                         meta)] %>%
    strsplit(., "\\(|\\)|\\,") %>%
    set_names(., map(., function(x){x[1] %>%
        gsub("=", "", .) %>%
        trimws(.) %>%
        gsub(" ", "_", .)})) %>%
    map(., function(x){x[2:3] %>% as.numeric(.)})

  # Meta out containts numeric pixel size and the numeric extent vector
  # required as inputs for obtain_soilgrids()
  meta_out$Pixel_Size <- abs(meta_out$Pixel_Size[1])
  meta_out$Extent <- c(meta_out$Lower_Left[1], meta_out$Upper_Right[1],
                       meta_out$Lower_Left[2], meta_out$Upper_Right[2])
  meta_out <- meta_out[c("Extent", "Pixel_Size")]
  names(meta_out) <- tolower(names(meta_out))

  return(meta_out)
}
