#' soil_project
#'
#' The `controls` is the hopefully new form of the `factor_set`. Instead of
#' using attributed we use an `R6` class. This should make it simpler for us
#' and the user. I am however not yet sure what the main attributes are and
#' what sub-class specific controls are? We can use inheritance to distinguish
#' sampling sets for specific sampling methods. Also, the construction and other
#' functions will later not be exported. I just want to have them exposed during
#' the development.
#' @name soil_project
#'
#' @import R6
#' @import pasta
#' @importFrom tibble tibble as_tibble
#'
#' @export
#'
#'
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
    }
    # soilgrids_metadata = get_layermeta(
    #   # project_path = self$data$project_path,
    #   wcs = "http://data.isric.org/geoserver/sg250m/wcs?",
    #   raw = FALSE)
    # print = function(...) {
    #   print(tibble::as_tibble(self$sample_data))
    #   invisible(self)}
  )
)
