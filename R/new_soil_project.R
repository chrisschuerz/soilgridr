#' Initiate a Soil Project
#'
#' Generates a new soil project (see: \code{\link{soil_project}}), together with
#'  a fixed link to the project folder and project path (used to save the data).
#'
#' @param project_name The name of the soil project. This will be the name of
#'   the project folder and the soil project in the working environment.
#'   Once initialized \strong{the name should not be changed}!
#' @param project_path Path to the SWAT project / Path where soilgrids layers
#'   are saved. Once initialized \strong{the path should not be changed}!
#' @param shape_file Shape file (or path to shape file) that defines the extent
#'   of the soilgrids layers. Alternatively, the extend (\code{extend}) and
#'   coordinate-system (\code{crs}) of a spatial object can be used.
#' @importFrom pasta %//%
#'
#' @return New \code{\link{soil_project}} in the working environment (as an R6
#'   object), as well as the project folder in the project path.
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
