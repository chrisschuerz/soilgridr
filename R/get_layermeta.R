#' Soilgrids Metainformation
#'
#' Get the required soilgrids meta information.
#'
#' @param wcs URL of the ISRIC soilgrids geoserver
#' @param raw logic, if \code{TRUE}  raw text output is returned if \code{FALSE} (default) the
#'   function returns the extracted values of the Tiff tiles pixel size and the
#'   extent.
#'
#' @importFrom XML newXMLNode saveXML
#' @importFrom purrr map
#' @importFrom magrittr %>% %<>% set_names
#' @importFrom pasta %//% %.%
#' @return Returns either a list with pixel size and extent of Tiffs as numerics
#'   or the raw text output.
#'
#' @examples
#' # Generate the required input for `obtain_soilgrids()`:
#'   layer_meta <- get_layermeta()
#'   layer_meta
#'
#' # Check all meta information:
#' # (also for crosschecking that function works properly)
#'   get_layermeta(raw = TRUE)

get_layermeta <- function(wcs = "http://data.isric.org/geoserver/sg250m/wcs?",
                          raw = FALSE) {

  # first layer of the soilgrids data set for which to aqcuire info
  layer <- "BDRICM_M_250m"
  loc   <- newXMLNode("WCS_GDAL")
  loc.s <- newXMLNode("ServiceURL", wcs, parent = loc)
  loc.l <- newXMLNode("CoverageName", layer, parent = loc)
  xml_out <- self$data$project_path%//%"soilgrids"%//%layer%.%"xml"
  saveXML(loc, file = xml_out)

  # path to the gdalinfo executable
  path_gdal_info <- ifelse(.Platform$OS.type == "windows",
                           shortPathName("C:/Program files/GDAL")%//%"gdalinfo.exe",
                           "gdalinfo")

  # paste and run gdal command. Store text output in variable
  gdal_cmd <- paste(path_gdal_info, xml_out)
  meta <- system(gdal_cmd, intern = TRUE)

  # Remove loaded .xml file
  file.remove(layer%.%"xml")

  if(raw){
    return(meta)
  } else {
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
}
