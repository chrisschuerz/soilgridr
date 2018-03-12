#' soil_project
#'
#' @rdname soil_project
#'
#' @import R6
#' @import pasta
#' @importFrom tibble tibble as_tibble
#'
#' @export

soil_project <- R6::R6Class(
  "soil_project",
  cloneable=FALSE,
  public = list(
    data = list(),
    initialize = function(project_path, shape_file) {
      if(dir.exists(project_path%//%"soil_layer")){
        stop("Soil project allready exists in"%&&%project_path)
      } else {
        dir.create(project_path%//%"soil_layer", recursive = TRUE)
      }
      if(is.character(shape_file)){
        shape_file <- readOGR(shape_file, verbose = FALSE)
      } else if(class(shape_file)[1] != "SpatialPolygonsDataFrame"){
        stop("shape_file must be either a shape file of the path to the shape_file!")
        }

      dir.create(project_path%//%"shape_file", recursive = TRUE)
      writeOGR(obj = shp,
               dsn = project_path%//%"shape_file"%//%"shp_file.shp",
               driver = "ESRI Shapefile",
               layer = "shp_file")

      self$data$project_path <- project_path
      self$data$shape_file <- shape_file
    },

    # soilgrids_metadata = function(
    #   project_path = self$data$project_path,
    #   wcs = "http://data.isric.org/geoserver/sg250m/wcs?"){
    #     get_layermeta(project_path, wcs, raw)
    # },

    load_soilgrids = function(
      project_path = self$data$project_path,
      wcs = "http://data.isric.org/geoserver/sg250m/wcs?"){
      if(is.null(self$data$meta$soilgrids_extent)){
        self$data$meta$soilgrids_extent <- c(-180, 180, -56.0008104, 83.9991672)
        self$data$meta$soilgrids_pixel_size <- 1/480
         layer_meta <- list(extent = c(-180, 180, -56.0008104, 83.9991672),
                           pixel_size = 1/480)
      } else {
        print("works")
      }


      # Further steps only require .tif files, therefore all loaded .xml files are removed.
      xml_files <- list.files(path = project_path%//%"soil_layer", pattern = ".xml$",
                              full.names = TRUE)
      file.remove(xml_files)


    }
    # print = function(...) {
    #   print(tibble::as_tibble(self$sample_data))
    #   invisible(self)}
  )
)
