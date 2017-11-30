#' Obtain soilgrids layer from the ISRIC geoserver
#'
#' @param project_path Path to the SWAT project / Path where soilgrids layers
#'   are saved
#' @param shp_file Shape file (or path to shape file) that defines the extent of
#'   the soilgrids layers. If \code{NULL} the shape file from the SWAT project
#'   will be used.
#' @param wcs URL of the ISRIC soilgrids geoserver
#' @param layer_meta List with the entries extent (Vector of length 4 with the
#'   soilgrids extent) and the soilgrids pixel_size. Check if these values are
#'   correct with \code{get_layermeta()]}
#' @importFrom rgdal readOGR
#' @importFrom sp CRS spTransform
#' @importFrom raster extent
#' @importFrom XML newXMLNode saveXML
#' @importFrom pasta %//% %.% %_% %&%
#'
#' @return Writes the required soilgrids layer to project_path/soilgrids.
#' @export

obtain_soilgrids <- function(project_path, shp_file = NULL,
                             wcs = "http://data.isric.org/geoserver/sg250m/wcs?",
                             layer_meta = list(extent = c(-180, 180, -56.0008104, 83.9991672),
                                               pixel_size = 1/480)) {

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
  # soilgrids data uses the WGS84 reference system. Projection of shape file
  # required for the extent and further clipping.
  sg_crs <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  shp_file <- spTransform(x = shp_file, CRSobj = sg_crs)
  shp_ext <- extent(shp_file)

  # Calculate the indices of the soilgrids raster for wcs access.
  find_rasterindex <- function(shp_ext, layer_meta) {
    #Translation from shape extent to pixel indices.
    sg_ext <- layer_meta$extent
    sg_pxl <- layer_meta$pixel_size
    sg_dim <- c(round((sg_ext[2] - sg_ext[1])/sg_pxl),
                round((sg_ext[4] - sg_ext[3])/sg_pxl))
    ind <- floor(sg_dim[1]*(shp_ext[1] - sg_ext[1])/(sg_ext[2] - sg_ext[1]))
    ind <- c(ind,
             floor(sg_dim[2]*(sg_ext[4] - shp_ext[4])/(sg_ext[4] - sg_ext[3])))
    ind <- c(ind,
             ceiling(sg_dim[1]*(shp_ext[2] - shp_ext[1])/(sg_ext[2] - sg_ext[1])))
    ind <- c(ind,
             ceiling(sg_dim[2]*(shp_ext[4] - shp_ext[3])/(sg_ext[4] - sg_ext[3])))

    return(ind)
  }

  sg_ind <- find_rasterindex(shp_ext, layer_meta)

  #URL of the ISRIC Soilgrids WCS server
  wcs <- "http://data.isric.org/geoserver/sg250m/wcs?"

  #The following soil layers are required
  layer_names <- c("BDRICM_M_250m",
                   "BLDFIE_M_sl"%&%1:7%_%"250m",
                   "CLYPPT_M_sl"%&%1:7%_%"250m",
                   "CRFVOL_M_sl"%&%1:7%_%"250m",
                   "SLTPPT_M_sl"%&%1:7%_%"250m",
                   "SNDPPT_M_sl"%&%1:7%_%"250m",
                   "CECSOL_M_sl"%&%1:7%_%"250m",
                   "ORCDRC_M_sl"%&%1:7%_%"250m",
                   "PHIHOX_M_sl"%&%1:7%_%"250m")

  # Obtain the soilgrids layers from the ISRIC geoserver
  ## Most steps here modification from
  ## http://gsif.isric.org/doku.php/wiki:tutorial_soilgrids

  ## Path to the installed gdal distro
  path_gdal_translate <- ifelse(.Platform$OS.type == "windows",
                           shortPathName("C:/Program files/GDAL")%//%"gdal_translate.exe",
                           "gdal_translate")
  ## Pixel size x,y
  pxl_dim <- paste(layer_meta$pixel_size, layer_meta$pixel_size, collapse = " ")
  ## Creation option
  c_opt <- "\"COMPRESS=DEFLATE\""
  ## Subwindow dimensions of the shape file extent
  src_win <- paste(sg_ind, collapse=" ")

  ## Create soilgrids folder in Project directory
  dir.create(project_path%//%"soilgrids")

  ## Looping over all layer names to obtain them from the ISRIC WCS
  for (layer_i in layer_names) {
    loc   <- newXMLNode("WCS_GDAL")
    loc.s <- newXMLNode("ServiceURL", wcs, parent = loc)
    loc.l <- newXMLNode("CoverageName", layer_i, parent = loc)
    xml_out <- project_path%//%"soilgrids"%//%layer_i%.%"xml"
    saveXML(loc, file = xml_out)
    tif_out <- project_path%//%"soilgrids"%//%layer_i%.%"tif"

    ### Pasting and sourcing gdal_translate command
    gdal_cmd <- paste(path_gdal_translate, xml_out, tif_out, "-tr", pxl_dim,
                      "-co", c_opt, "-srcwin", src_win)
    system(gdal_cmd)

  }

  # Further steps only require .tif files, therefore all loaded .xml files are removed.
  xml_files <- list.files(path = project_path%//%"soilgrids", pattern = ".xml$",
                          full.names = TRUE)
  file.remove(xml_files)
}
