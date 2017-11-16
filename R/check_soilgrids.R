
#' Check the soilgrids meta data
#' @param wcs URL of the ISRIC soilgrids geoserver
#'
#' @importFrom pasta %//% %.%
#' @importFrom XML newXMLNode saveXML
#'
#' @export

check_soilgrids <- function(wcs = "http://data.isric.org/geoserver/sg250m/wcs?") {



  # first layer of the soilgrids data set for which to aqcuire info
  layer <- "BDRICM_M_250m"
  loc   <- newXMLNode("WCS_GDAL")
  loc.s <- newXMLNode("ServiceURL", wcs, parent = loc)
  loc.l <- newXMLNode("CoverageName", layer, parent = loc)
  xml_out <- layer%.%"xml"
  saveXML(loc, file = xml_out)

  # path to the gdalinfo executable
  path_gdal_info <- ifelse(.Platform$OS.type == "windows",
                           shortPathName("C:/Program files/GDAL")%//%"gdalinfo.exe",
                           "gdalinfo")

  # paste and run gdal command
  gdal_cmd <- paste(path_gdal_info, xml_out)
  system(gdal_cmd)
  file.remove(layer%.%"xml")
}
