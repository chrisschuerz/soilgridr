#' Obtain Soilgrids
#'
#' Obtain soilgrids layer from the ISRIC geoserver.
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
#' @param layer_names Character vector of names of the soilgrids layers to be
#'   downloadad.
#'
#' @importFrom dplyr %>%
#' @importFrom gdalUtils gdal_translate
#' @importFrom raster extent crs extent<- crs<-
#' @importFrom sp spTransform SpatialPolygons
#' @importFrom XML newXMLNode saveXML
#'
#' @return Writes the required soilgrids layer to project_path/soilgrids.
#'
#' @keywords internal

obtain_soilgrids <- function(project_path, shp_file, variables, depths, quantiles, wcs) {

  variable_available <- c("bdod", "cec", "cfvo", "clay", "nitrogen", "phh2o", "sand", "silt", "soc", "ocd", "ocs")

  if(any(!(variables %in% variable_available))) {
    stop("Only the following 'variables' can be accessed:\n",
         paste(variable_available, collapse = ", "))
  }

  depth_lbl <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")
  depth <- c(1,3)
  if(is.numeric(depth)) {
    stopifnot(depth %in% 1:6)
    depth <- depth_lbl[depth]
  }

  if(any(!(quantile %in% c("Q0.05", "Q0.5", "mean", "Q0.95")))) {
    stop("For 'quantiles' only the inputs 'Q0.05', 'Q0.5', 'mean', and 'Q0.95'",
         " are allowed!")
  }

  layer_tbl <- expand.grid(quantiles, depths, variables) %>%
    as_tibble(.) %>%
    map_df(., as.character) %>%
    set_names(c("quantile", "depth", "variable")) %>%
    select(variable, depth, quantile)
  layer_names <- apply(layer_tbl, 1, paste, collapse = "_")

  # soilgrids data uses the Interrupted_Goode_Homolosine projection.
  # Projection of shape file required for the extent and further clipping.
  sg_crs <- crs(wcs$crs)

  shp_ext <- spTransform(x = shp_file$shape, CRSobj = sg_crs) %>%
    extent(.) %>%
    .[c(1,4,2,3)]

  ## Looping over all layer names to obtain them from the ISRIC WCS
  t0 <- now()

  for (i_layer in 1:length(layer_names)) {

    layer_i <- layer_names[i_layer]
    wcs_pth <- paste(wcs$path%//%layer_tbl[i_layer,1]%.%"map", wcs$service,wcs$version, sep = "&")

    loc   <- newXMLNode("WCS_GDAL")
    loc.s <- newXMLNode("ServiceURL", wcs_pth, parent = loc)
    loc.l <- newXMLNode("CoverageName", layer_i, parent = loc)
    xml_out <- project_path%//%"soil_layer"%//%layer_i%.%"xml"
    saveXML(loc, file = xml_out)
    tif_out <- project_path%//%"soil_layer"%//%layer_i%.%"tif"

    gdal_translate(src_dataset = xml_out,
                   dst_dataset = tif_out,
                   of = "GTiff",
                   tr = wcs$pixel,
                   projwin = shp_ext,
                   projwin_srs = wcs$crs,
                   co = c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"))

    # Further steps only require .tif files, therefore all loaded .xml files are removed.
    xml_files <- list.files(path = project_path%//%"soil_layer", pattern = ".xml$",
                            full.names = TRUE)
    file.remove(xml_files)
    display_progress(i_layer, length(layer_names), t0, "Layer")
  }
  finish_progress(length(layer_names), t0, "Finished downloading", "layer")
  return(layer_tbl)
}
