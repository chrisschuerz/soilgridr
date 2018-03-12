#' soil_project
#'
#' @rdname soil_project
#'
#' @import R6
#' @import pasta
#' @importFrom tibble tibble as_tibble
#' @importFrom rgdal readOGR writeOGR
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
      writeOGR(obj = shape_file,
               dsn = project_path%//%"shape_file"%//%"shp_file.shp",
               driver = "ESRI Shapefile",
               layer = "shp_file")

      self$data$project_path <- project_path
      self$data$shape_file <- shape_file
      self$data$wcs <- "http://data.isric.org/geoserver/sg250m/wcs?"
    },

    load_soilgrids = function(wcs = self$data$wcs){
      cat("Downloading soilgrids layer:\n\n")
      layer_meta <- get_layermeta(project_path = self$data$project_path,
                                  wcs =  self$data$wcs)

      self$data$soilgrids_pixel_size <- layer_meta$pixel_size
      self$data$soilgrids_extent <- layer_meta$Extent

      obtain_soilgrids(project_path = self$data$project_path,
                       shp_file = self$data$shape_file,
                       wcs = wcs,
                       layer_meta = layer_meta)

      cat("Loading soilgrids layer into R:\n\n")

    }
    # print = function(...) {
    #   print(tibble::as_tibble(self$sample_data))
    #   invisible(self)}
  )
)
