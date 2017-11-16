
###For testing
project_path <- "E:/R_project/SWAT_Raab"
library(pasta)
library(magrittr)

obtain_soilgrids <- function(project_path, shp_file = NULL,
                             sg_ext = c(-180, 180, -56.0008104, 83.9991672),
                             sg_dim = c(172800, 67200),
                             sg_pxl = 1/480) {

  # if no shp file provided subs1 shape frome SWAT watershed delineation used.
  if(is.null(shp_file)) {
    shp_file <- rgdal::readOGR(dsn = project_path%//%"Watershed/shapes"%//%
                                     "subs1.shp",
                               layer = "subs1")
  }
  # soilgrids data uses the WGS84 reference system. Projection of shape file
  # requiered for the extent and further clipping.
  sg_crs <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
  shp_file <- sp::spTransform(x = shp_file, CRSobj = sg_crs)
  shp_ext <- raster::extent(shp_file)

  # Calculate the indices of the soilgrids raster for wcs access.
  sg_ind <- find_rasterindex(shp_ext, sg_ext, sg_dim)

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
                   "PHIHOX_M_sl"%&%1:7%_%"250m",
                   "PHIKCL_M_sl"%&%1:7%_%"250m")

  # Obtain the soilgrids layers from the ISRIC geoserver
  ## Most steps here modification from http://gsif.isric.org/doku.php/wiki:tutorial_soilgrids
  ## Path to the installed gdal distro
  path_gdal_translate <- ifelse(.Platform$OS.type == "windows",
                           shortPathName("C:/Program files/GDAL")%//%"gdal_translate.exe",
                           "gdal_translate")
  ## Pixel size x,y
  pxl_dim <- paste(sg_pxl, sg_pxl, collapse = " ")
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
}

find_rasterindex <- function(shp_ext, sg_ext, sg_dim) {
  #Translation from shape extent to pixel indices.
  ind <- floor(sg_dim[1]*(shp_ext[1] - sg_ext[1])/(sg_ext[2] - sg_ext[1]))
  ind <- c(ind,
           floor(sg_dim[2]*(sg_ext[4] - shp_ext[4])/(sg_ext[4] - sg_ext[3])))
  ind <- c(ind,
           ceiling(sg_dim[1]*(shp_ext[2] - shp_ext[1])/(sg_ext[2] - sg_ext[1])))
  ind <- c(ind,
           ceiling(sg_dim[2]*(shp_ext[4] - shp_ext[3])/(sg_ext[4] - sg_ext[3])))

  return(ind)
}
