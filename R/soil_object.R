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

      self$data$project_path <- project_path
      self$data$shape_file <- shape_file
    },

    soilgrids_metadata = function(
      project_path = self$data$project_path,
      wcs = "http://data.isric.org/geoserver/sg250m/wcs?",
      raw = FALSE){
        meta <- get_layermeta(project_path, wcs, raw)
        if(raw){
          meta
        } else{
          meta
          self$data$meta$soilgrids_extent <- meta$extent
          self$data$meta$soilgrids_pixel_size <- meta$pixel_size
        }
    },

    download_soilgrids = function(
      #TO BE CONTINUED HERE...
    ){}
    # print = function(...) {
    #   print(tibble::as_tibble(self$sample_data))
    #   invisible(self)}
  )
)
