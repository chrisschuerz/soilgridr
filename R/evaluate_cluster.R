#' Evaluate Clustering Results
#'
#' Evaluate the results of the different kmeans clusters by calculating the
#'  sum of squared differences for each one.
#'
#' @param cluster_result Results of the soilgrids clustering stored in the \strong{soil project}.
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point theme_bw xlab ylab
#' @importFrom tibble tibble add_column
#' @importFrom purrr map_dfr
#'
#' @return A ggplot object that shows the the SSE (within a the given classes)
#'   over the number of classes

evaluate_cluster <- function(cluster_result) {
  cluster_result %>%
    map_dfr(., function(x){
                 tibble(norm_within_ssq  = x$tot.withinss/x$totss)}) %>%
    add_column(n_class = names(cluster_result) %>%
                 gsub("n_", "", .) %>%
                 as.numeric(.), .before = 1) %>%
    ggplot(data = ., aes(x = n_class, y = norm_within_ssq)) +
      geom_line() +
      geom_point() +
      theme_bw() +
      xlab("Number of soil classes") +
      ylab("within SSE / total SSE")
}
