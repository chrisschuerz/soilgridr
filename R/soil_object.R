#' Soil Project Class
#'
#' The \strong{soil project} class constitutes the basic building block of
#'   \code{solACE}. One can Generate a soil project with the
#'   \code{\link{new_project}} function. \cr
#'   In case you are interested in the technical details: The soil
#'   project class is realised by using object-oriented approach from the R6
#'   package. Read more at: \href{https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html}{Introduction to R6}.
#'
#'
#' @rdname soil_project
#'
#' @import R6
#' @importFrom dplyr quo
#' @importFrom raster crs extent shapefile
#' @importFrom sp SpatialPolygons
#' @importFrom tibble tibble as_tibble
#'
#' @export
soil_project <- R6::R6Class(
  "soil_project",
  cloneable = FALSE,
  lock_objects = FALSE,
  public = list(
    .data = list(),

    initialize = function(project_name, project_path, shape_file, ext, crs) {
      if(dir.exists(project_path%//%project_name%//%"soil_layer")){
        stop("Soil project allready exists in"%&&%project_path)
      } else {
        dir.create(project_path%//%project_name%//%"soil_layer", recursive = TRUE)
      }

      if(!is.null(shape_file) & is.null(ext) & is.null(crs)){
        if(is.character(shape_file)){
          shape_file <- shapefile(shape_file)
        } else if(class(shape_file)[1] != "SpatialPolygonsDataFrame"){
          stop("The variable 'shape_file' must be either a shape file or a character string providing the path to a shape file!")
          }
        dir.create(project_path%//%project_name%//%"shape_file", recursive = TRUE)
        shapefile(shape_file, project_path%//%project_name%//%"shape_file"%//%"shp_file.shp")
        shp_from_ext <- FALSE
      } else if(any(is.null(ext), is.null(crs))){
        stop("Either the variable'shape_file' or the variables 'extent' AND 'crs' must be defined!")
      } else {
        if(class(ext) != "Extent") ext <- extent(ext)
        if(class(crs) != "CRS")    crs <- crs(crs)
        dir.create(project_path%//%project_name%//%"shape_file", recursive = TRUE)
        shp_ext <- as(ext, 'SpatialPolygons')
        crs(shp_ext) <- crs
        shapefile(shp_ext, project_path%//%project_name%//%"shape_file"%//%"shp_file.shp")
        shape_file <- shapefile(project_path%//%project_name%//%"shape_file"%//%"shp_file.shp")
        shp_from_ext <- TRUE
      }


      self$.data$meta$project_name <- project_name
      self$.data$meta$project_path <- project_path%//%project_name

      self$.data$shape_file$shape <- shape_file
      self$.data$shape_file$extent <- extent(shape_file)
      self$.data$shape_file$crs <- crs(shape_file)
      self$.data$shape_file$shape_from_extent <- shp_from_ext

      self$.data$meta$wcs_server <- list(
        path = "https://maps.isric.org/mapserv?map=/map",
        service = "SERVICE=WCS",
        version = "VERSION=2.0.1",
        crs = '+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs',
        pixel = c(250, 250)
      )

    },

    print = function() {
      if(is.null(self$.data$data_processed)) {
        cat("New porject initiated. Load soilgrids layers with .$load_soilgrids()")
      } else {
        print(self$.data$data_processed)
      }
      invisible(self)
    },

    save = function(){
      obj_save <- get(x = self$.data$meta$project_name,
                      envir = sys.frame(-1))
      saveRDS(object = obj_save,
              file = self$.data$meta$project_path%//%"sol.proj")
    },

    load_soilgrids = function(valriables = c("sand", "silt", "clay", "bdod",
                                             "cfvo", "cec", "phh2o", "soc"),
                              depths = 1:6, quantiles = "mean"){
      cat("Load SoilGrids layers using Web Coverage Services: \n")
      # layer_meta <- get_layermeta(project_path = self$.data$meta$project_path,
      #                             wcs = self$.data$soilgrids$meta$wcs_server)
      #
      # self$.data$soilgrids$meta$pixel_size <- layer_meta$pixel_size
      # self$.data$soilgrids$meta$extent <- layer_meta$extent

      self$.data$soilgrids$meta$layer_table <-
        obtain_soilgrids(project_path = self$.data$meta$project_path,
                         shp_file = self$.data$shape_file,
                         layer_table = layer_table,
                         wcs = self$.data$meta$wcs_server)
                         # layer_meta = layer_meta,

      cat("\nLoading soilgrids layer into R:\n\n")
      soil_data <- load_soilgrids(project_path = self$.data$meta$project_path,
                                  shape_file   = self$.data$shape_file,
                                  layer_table  = self$.data$soilgrids$meta$layer_table)
      #
      #
      self$.data$soilgrids$raster     <- soil_data$soil_raster
      self$.data$soilgrids$data       <- soil_data$soil_data
      self$.data$soilgrids$meta$layer <- soil_data$layer_meta
      self$.data$data_processed       <- soil_data$soil_data

      self$from_scratch <- function(){
        self$.data$data_processed <- self$.data$soilgrids$data
        self$.data$soil_cluster <- NULL
        self$.data$soilgrids$meta$layer$depths_partition <- NULL
        self$evaluate_cluster <- function() cat("No clustering to evaluate! Redo clustering over area before.")
        self$select_cluster <-  function() cat("No cluster to select! Redo clustering over area before.")
        self$plot_cluster <- function() cat("No cluster to plot! Redo clustering over area before.")
      }

      self$mutate_variable <- function(..., sl = NULL) {
        fun_list <- as.list(match.call(expand.dots = FALSE))$...
        new_var <- names(fun_list)
        fun_list %<>% unname()
        for(i in 1:length(fun_list)) {fun_list[[new_var[i]]] <- quo(!!fun_list[[i]])}
        fun_list <- fun_list[new_var]

        self$.data$data_processed <-
          calculate_soilproperty(soil_data = self$.data$data_processed,
                                 sl = sl, fun_list = fun_list)
      }

      self$select_variable <- function(..., sl = NULL) {
        vars <- as.list(match.call(expand.dots = FALSE))$...
        sel_expr <- quo(c(!!!vars))

        self$.data$data_processed <-
          select_soilproperty(soil_data = self$.data$data_processed,
                              sl = sl, sel_expr = sel_expr)
      }

      self$plot_variable <- function(variable = NULL, sl = NULL, normalize = FALSE) {
        plot_variable(soil_data = self$.data, variable = variable, sl = sl,
                      normalize = normalize)
      }

      self$partition_depth <- function(lower_bound) {
        if(!is.null(self$.data$soil_cluster) &
           is.null(self$.data$soil_cluster$cluster_k)){
          stop("Set final number of soil classes before aggregating!")
        }

        if(!is.null(self$.data$soilgrids$meta$layer$depths)){
          stop("Aggregation of soil layers allready performed!\n"%&%
                 "Start from scratch with $from_scratch() if you want to redo an aggregation over depth.")
        }

        self$.data$data_processed <-
          aggregate_layer(soil_data = self$.data$data_processed,
                          lower_bound)

        self$.data$soilgrids$meta$layer$depths <- lower_bound
      }

      self$cluster_area <- function(clusters_k, auto_select = FALSE){
        if(!is.null(self$.data$soil_cluster$cluster_k)){
          stop("Clustering allready performed and final number of classes set!\n"%&%
               "Start from scratch with $from_scratch() if you want to redo the clustering.")
        }

        self$.data$data_processed$soil_class <- NULL

        self$.data$soil_cluster <-
          cluster_soil(soil_data = self$.data$data_processed,
                       clusters_k = clusters_k)
        self$.data$soil_cluster$cluster_summary <- calculate_max_dist(soil_data = self$.data)

        if(auto_select) {
          self$.data$soil_cluster$cluster_k <-
            self$.data$soil_cluster$cluster_summary$cluster_k[
              which.max(self$.data$soil_cluster$cluster_summary$max_diff)]
          self$.data$data_processed <-
            set_cluster_data(soil_data = self$.data,
                             cluster_k = self$.data$soil_cluster$cluster_k)
        }

        self$evaluate_cluster <- function(){
          evaluate_cluster(soil_data = self$.data)
        }

        self$select_cluster <-  function(cluster_k){
          if(!("n"%_%cluster_k %in% names(self$.data$soil_cluster))){
            stop("Selected number of classes not available!")
          }

          if(!is.null(self$.data$soil_cluster$cluster_k)){
            stop("Final number of soil classes allready set!\n"%&%
                   "Start from scratch with $from_scratch() if"%&%
                   " you want to define a different number of soil classes!")
          }

          self$.data$soil_cluster$cluster_k <- cluster_k
          self$.data$data_processed <-
            set_cluster_data(soil_data = self$.data, cluster_k = cluster_k)
        }

        self$plot_cluster <- function(cluster_k = self$.data$soil_cluster$cluster_k) {
          plot_soilmap(soil_data = self$.data, cluster_k = cluster_k)
        }
      }

      self$write_output <- function(variable = NULL, sl = NULL, format = "tif", overwrite = FALSE){
        if(!is.null(self$.data$soil_cluster) &
           is.null(self$.data$soil_cluster$cluster_k)){
          stop("Set final number of soil classes before writing outputs!")
        }
        if(format %in% c("tif", "ascii")) {
          write_out(soil_data = self$.data, variable = variable, sl = sl,
                    format = format, overwrite = overwrite)
        } else if (format %in% c("tibble", "raster")) {
          print(return_out(soil_data = self$.data, variable = variable, sl = sl,
                           format = format))
        } else {
          stop("Wrong format. Select one of: 'tif', 'ascii', 'tibble', 'raster'." )
        }
      }

      self$save()

    }

  )
)


