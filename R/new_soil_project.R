#' Initiate a new soil project
#'
#' @param project_name the name of the soil project. This will be the name of
#'   the project folder and the soil project in the working environment. The
#'   name of this object should not be changed!
#' @param project_path Path to the SWAT project / Path where soilgrids layers
#'   are saved
#' @param shape_file Shape file (or path to shape file) that defines the extent
#'   of the soilgrids layers.

#' @importFrom pasta %//%
#'
#' @return Generates a new soil_project in the working environment (as an R6
#'   object) and generates the prject folder in the project path.
#' @export

new_soil_project <- function(project_name, project_path,
                             shape_file = NULL, extent = NULL, crs = NULL) {
  soil_obj <- soil_project$new(project_name = project_name,
                               project_path = project_path,
                               shape_file = shape_file,
                               ext = extent,
                               crs = crs)

  assign(project_name, soil_obj, envir = sys.frame(-1))
  save(list = project_name,
       file = project_path%//%project_name%//%"soil_project.RData",
       envir = sys.frame(-1))
}
