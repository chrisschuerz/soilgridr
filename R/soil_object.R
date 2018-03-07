#' #' soil_object
#' #'
#' #' The `controls` is the hopefully new form of the `factor_set`. Instead of
#' #' using attributed we use an `R6` class. This should make it simpler for us
#' #' and the user. I am however not yet sure what the main attributes are and
#' #' what sub-class specific controls are? We can use inheritance to distinguish
#' #' sampling sets for specific sampling methods. Also, the construction and other
#' #' functions will later not be exported. I just want to have them exposed during
#' #' the development.
#' #' @name soil_object
#' #'
#' NULL
#'
#' #' Sampling Set
#' #'
#' #' The most basic sampling set (used for sample_random). Internally we can
#' #' assign it with
#' #' `g <- SWATpasteR::controls$new(method = "test", sample_data = 4)`. This
#' #' creates a objected named `g` of class `controls` (see `class(g)`), with
#' #' the provided data. We can then extract the information as part of the object,
#' #' e.g. `g$method` will return the string "cool" and `g$sample_data` a tibble
#' #' with the column `data` and one entry, namely `4`.
#' #'
#' #' @rdname controls
#' #'
#' #' @import R6
#' #' @import pasta
#' #' @importFrom tibble tibble as_tibble
#' #'
#' #' @export
#' soil_object <- R6Class(
#'   "soil_object",
#'   public = list(
#'     soil_data   = tibble(idx = NA_integer_,
#'                          label   = NA_character_,
#'                            control = NA_character_,
#'                            type    = NA_character_,
#'                            domain  = list(NA),
#'                            domain_norm = list(NA_real_)),
#'     n_samples     = NA_integer_,
#'     method        = NA_character_,
#'     sample_data   = tibble(NA), # samplign data
#'
#'     initialize = function(.boundaries,
#'                           n_samples,
#'                           method,
#'                           sample_data) {
#'       self$.boundaries <- .boundaries
#'       self$n_samples <- n_samples
#'       self$method <- method
#'       self$sample_data <- sample_data
#'     },
#'     print = function(...) {
#'       print(tibble::as_tibble(self$sample_data))
#'       invisible(self)}
#'   )
#' )
