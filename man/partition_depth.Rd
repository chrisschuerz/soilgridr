% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function_manuals.R
\name{partition_depth}
\alias{partition_depth}
\title{Partition soilgrids Layers over Depth.}
\arguments{
\item{lower_bound}{Vector defining the lower bounds of the aggregated soil
layers (depths in cm).}
}
\description{
Partition the \href{https://soilgrids.org/}{soilgrids} layers within the
 \strong{soil project} over their respective depth. By convention most
 layers are divided into 7 depth-classes.This function allows to newly
 partition them over chosen depths.
}
\examples{
# Not run
# Replace "my_project" with the actual project name

# Partition the soil layers to three layers with 0-30cm 30-100cm and 100-200cm
# In this case only works if all 7 depths from soilgrids are loaded
my_project$partition_depth(lower_bound = c(30,100,200))

# This works if for example only the first three layers are loaded:
my_project$partition_depth(lower_bound = 8)
}
