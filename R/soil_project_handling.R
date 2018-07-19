#' Initiate a Soil Project
#'
#' Generates a new \strong{soil project} (see: \code{\link{soil_project}}),
#' together with a fixed link to the project folder and project path (used to
#' save the data).
#'
#' @param project_name The name of the soil project. This will be the name of
#'   the project folder and the soil project in the working environment.
#' @param project_path Path where the soil project and its data are saved.
#' @param shape_file Shape file (or path to shape file) that defines the extent
#'   of the soilgrids layers. Alternatively, the extent (\code{extent}) and
#'   coordinate-system (\code{crs}) of a spatial object can be used.
#' @param extent Extent of the window for which SoilGrids data should be loaded.
#' @param crs Coordinate system of the provided extent.
#'
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
  soil_obj$save()
}

#' Load an existing Soil Project
#'
#' Loads an existing \strong{soil project} (see: \code{\link{soil_project}}),
#'   together with a fixed link to the project folder and project path
#'   (used to save the data).
#'
#' @param soil_project Character string providing path to the soil project.
#' @importFrom pasta %//%
#'
#' @return The loaded \code{\link{soil_project}} in the working environment (as an R6
#'   object).
#' @export

load_soil_project <- function(soil_project) {

  dir_name <-  dirname(soil_project)
  base_name <- basename(soil_project)

  if(base_name == "sol.proj") {
    soil_obj <- readRDS(soil_project)
    project_name <- basename(dir_name)
    soil_obj$.data$meta$project_path <- dir_name
    soil_obj$.data$meta$project_name <- project_name
  } else {
    soil_obj <- readRDS(soil_project%//%"sol.proj")
    soil_obj$.data$meta$project_path <- soil_project
    project_name <- base_name
    soil_obj$.data$meta$project_name <- project_name
  }

  assign(project_name, soil_obj, envir = sys.frame(-1))
  soil_obj$save()
}
