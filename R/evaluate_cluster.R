#' Evaluate Clustering Results
#'
#' Evaluate the results of the different \link[=cluster_soil]{kmeans clusters} by calculating the
#'   sum of squared differences for each one (between the clustered and the raw data).
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
  n_class  <- names(cluster_result)[grepl("n_", names(cluster_result))]
  n_select <- cluster_result$cluster_k

  sse_dat <- cluster_result[n_class] %>%
    map_dfr(., function(x){
                 tibble(norm_within_ssq  = x$tot.withinss/x$totss)}) %>%
    add_column(n_class = n_class %>%
                 gsub("n_", "", .) %>%
                 as.numeric(.), .before = 1)

  int <- max(round(nrow(sse_plot)/10),1)
  x_breaks <- seq(min(sse_plot$n_class), max(sse_plot$n_class), int)

  sse_plot <- ggplot(data = sse_dat, aes(x = n_class, y = norm_within_ssq)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = x_breaks, minor_breaks = 1:max(sse_plot$n_class)) +
    theme_bw() +
    xlab("Number of soil classes") +
    ylab("within SSE / total SSE")

  if(!is.null(n_select)) {
    select_dat <- sse_dat %>% filter(n_class == n_select)
    sse_plot <- sse_plot +
      geom_point(data = select_dat, aes(x = n_class, y = norm_within_ssq), col = "red", size = 3)
  }
  return(sse_plot)
}
