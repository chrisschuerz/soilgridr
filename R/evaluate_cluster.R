#' Plotting of the cluster results 
#'
#' Creates a plot of the results from the soilgrids clustering (\code{sg_cluster}).
#' 
#' @param sg_cluster Results of the soilgrids clustering
#' @importFrom tibble tibble add_column
#' @importFrom purrr map_dfr
#' @importFrom ggplot2 ggplot aes geom_line geom_point theme_bw xlab ylab
#'
#' @return A ggplot object that shows the the SSE (within a the given classes) over the
#'   number of classes
#' @export

evaluate_cluster <- function(sg_cluster) {
  sg_cluster$soil_cluster %>%
    map_dfr(., function(x){
                 tibble(norm_within_ssq  = x$tot.withinss/x$totss)}) %>%
    add_column(n_class = names(sg_cluster$soil_cluster) %>%
                 gsub("n_", "", .) %>%
                 as.numeric(.), .before = 1) %>%
    ggplot(data = ., aes(x = n_class, y = norm_within_ssq)) +
      geom_line() +
      geom_point() +
      theme_bw() +
      xlab("Number of soil classes") +
      ylab("within SSE / total SSE")


    # Keep as idea in mind
    # spl <- smooth.spline(x = gg_tbl$n_class, y = gg_tbl$norm_within_ssq)
    # curv <- predict(spl, x = gg_tbl$n_class, deriv = 2) %>% as.data.frame(.)
    #
    # ggplot() +
    #   geom_bar(data = gg_tbl, aes(x = n_class, y = norm_within_ssq),
    #            stat = "identity", width = 0.3) +
    #   geom_line(data = curv, aes(x = x, y = y), ) +
    #   geom_point(data = curv, aes(x = x, y = y),) +
}
