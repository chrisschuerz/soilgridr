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

#' Partition soilgrids Layers over Depth.
#'
#' Partition the \href{https://soilgrids.org/}{soilgrids} layers within the
#'  \strong{soil project} over their respective depth. By convention most
#'  layers are divided into 7 depth-classes.This function allows to newly
#'  partition them over chosen depths.
#'
#' @param lower_bound Vector defining the lower bounds of the aggregated soil
#'   layers (depths in cm).
#' @name partition_depth
#' @examples
#' # Not run
#' # Replace "my_project" with the actual project name
#'
#' # Partition the soil layers to three layers with 0-30cm 30-100cm and 100-200cm
#' # In this case only works if all 7 depths from soilgrids are loaded
#' my_project$partition_depth(lower_bound = c(30,100,200))
#'
#' # This works if for example only the first three layers are loaded:
#' my_project$partition_depth(lower_bound = 8)

NULL
