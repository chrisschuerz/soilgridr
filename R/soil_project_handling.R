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


#  Implicit Function Descriptions: ----------------------------------------


#' Start Soil Project from Scratch
#'
#' Provides a simple reset for the soil project, by restarting it from scratch.
#'
#' @name from_scratch
NULL

#' Plot clustered soil map
#'
#' Plot a clustered soilmap (where the number of clusters is defined by the
#' \code{\link{select_n_class}} function).
#'
#' @name plot_cluster
NULL

#' Select Number of Clusters
#'
#' Select the number of clusters (within kmeans) for further processing.
#' The function is meant to be used in conjunction with
#' \code{\link{cluster_soil}} and \code{\link{evaluate_cluster}}.
#'
#' @param n Number of clusters.
#'
#' @name select_n_class
NULL

#' Aggregate soilgrids Layers over Depth.
#'
#' Aggregate the \href{https://soilgrids.org/}{soilgrids} layers within the
#'  \strong{soil project} over their respective depth. By convention most
#'  layers are divided into 7 depth-classes.This function allows to aggregated
#'  them over chosen depths.
#'
#' @param lower_bound Vector defining the lower bounds of the aggregated soil
#'   layers (depths in cm).
#' @name aggregate_depth
NULL

