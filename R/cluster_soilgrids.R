cluster_soilgrids <- function(project_path, shp_file = NULL,  ) {

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


}
