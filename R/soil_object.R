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
  private = list(data_hidden = list()),
  public = list(
    data = list(),

    initialize = function(project_name, project_path, shape_file) {
      if(dir.exists(project_path%//%project_name%//%"soil_layer")){
        stop("Soil project allready exists in"%&&%project_path)
      } else {
        dir.create(project_path%//%project_name%//%"soil_layer", recursive = TRUE)
      }
      if(is.character(shape_file)){
        shape_file <- readOGR(shape_file, verbose = FALSE)
      } else if(class(shape_file)[1] != "SpatialPolygonsDataFrame"){
        stop("shape_file must be either a shape file of the path to the shape_file!")
        }

      dir.create(project_path%//%project_name%//%"shape_file", recursive = TRUE)
      writeOGR(obj = shape_file,
               dsn = project_path%//%project_name%//%"shape_file"%//%"shp_file.shp",
               driver = "ESRI Shapefile",
               layer = "shp_file")

      self$data$meta$project_name <- project_name
      self$data$meta$project_path <- project_path%//%project_name

      self$data$shape_file <- shape_file
      self$data$meta$soilgrids$server_path <-
        "http://data.isric.org/geoserver/sg250m/wcs?"
    },

    load_soilgrids = function(soilgrids_server = self$data$meta$soilgrids$server_path,
                              layer_names = c("BDRICM_M_250m",
                                              "BLDFIE_M_sl"%&%1:7%_%"250m",
                                              "CLYPPT_M_sl"%&%1:7%_%"250m",
                                              "CRFVOL_M_sl"%&%1:7%_%"250m",
                                              "SLTPPT_M_sl"%&%1:7%_%"250m",
                                              "SNDPPT_M_sl"%&%1:7%_%"250m",
                                              "CECSOL_M_sl"%&%1:7%_%"250m",
                                              "ORCDRC_M_sl"%&%1:7%_%"250m",
                                              "PHIHOX_M_sl"%&%1:7%_%"250m")){
      cat("Downloading soilgrids layer:\n\n")
      layer_meta <- get_layermeta(project_path = self$data$meta$project_path,
                                  wcs = soilgrids_server)

      self$data$meta$soilgrids$pixel_size <- layer_meta$pixel_size
      self$data$meta$soilgrids$extent <- layer_meta$extent
      self$data$meta$soilgrids$layer_names <-
        obtain_soilgrids(project_path = self$data$meta$project_path,
                         shp_file = self$data$shape_file,
                         wcs = soilgrids_server,
                         layer_meta = layer_meta,
                         layer_names = layer_names)

      cat("\nLoading soilgrids layer into R:\n\n")
      soil_data <- load_soilgrids(project_path = self$data$meta$project_path,
                                  shape_file   = self$data$shape_file,
                                  layer_names  = self$data$meta$soilgrids$layer)

      private$data_hidden$soil_list <- soil_data$soil_list
      self$data$raster <- soil_data$soil_raster
      self$data$table  <- soil_data$soil_table

    },

    test = function(layer_names = c("BDRICM_M_250m",
                                    "BLDFIE_M_sl"%&%1:7%_%"250m",
                                    "CLYPPT_M_sl"%&%1:7%_%"250m",
                                    "CRFVOL_M_sl"%&%1:7%_%"250m",
                                    "SLTPPT_M_sl"%&%1:7%_%"250m",
                                    "SNDPPT_M_sl"%&%1:7%_%"250m",
                                    "CECSOL_M_sl"%&%1:7%_%"250m",
                                    "ORCDRC_M_sl"%&%1:7%_%"250m",
                                    "PHIHOX_M_sl"%&%1:7%_%"250m")){

      self$data$meta$soilgrids$pixel_size <- 1/480
      self$data$meta$soilgrids$extent <- c(-180, 179.99994, -56.00081, 83.99917)
      self$data$meta$soilgrids$layer_names <- c("BDRICM_M_250m",
                                                "BLDFIE_M_sl"%&%1:7%_%"250m",
                                                "CLYPPT_M_sl"%&%1:7%_%"250m",
                                                "CRFVOL_M_sl"%&%1:7%_%"250m",
                                                "SLTPPT_M_sl"%&%1:7%_%"250m",
                                                "SNDPPT_M_sl"%&%1:7%_%"250m",
                                                "CECSOL_M_sl"%&%1:7%_%"250m",
                                                "ORCDRC_M_sl"%&%1:7%_%"250m",
                                                "PHIHOX_M_sl"%&%1:7%_%"250m")

      soil_data <- load_soilgrids(project_path = self$data$meta$project_path,
                                  shape_file = self$data$shape_file,
                                  layer_names = self$data$meta$soilgrids$layer)
      private$data_hidden$soil_list <- soil_data$soil_list
      self$data$raster <- soil_data$soil_raster
      self$data$table <- soil_data$soil_table

    }

    aggregate_depth = function() {

    }
  )
)
