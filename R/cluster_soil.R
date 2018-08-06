#' Cluster Layers
#'
#' Cluster the layers within the \strong{soil project} by using kmeans
#'   clustering.
#'
#' @param soil_data List of tibbles holding soil parameters for the respective
#'   soil layers.
#' @param clusters_k Vector of number of soil classes that should be generated
#'   with k-means clustering.

#' @importFrom dplyr bind_cols progress_estimated
#' @importFrom magrittr %>%
#' @importFrom purrr map2
cluster_soil <- function(soil_data, clusters_k){
  # Soil group clustering using kmeans
  # Create scaled table to apply kmeans on
  suffix <- "_"%&%names(soil_data)
  suffix[!(suffix %in% ("_sl"%&%1:100))] <- ""

  clst_tbl <- soil_data %>%
    purrr::map2(., suffix, function(tbl, nm) {
      names(tbl) <- names(tbl)%&%nm
      return(tbl)}) %>%
    dplyr::bind_cols(.) %>%
    base::scale(., scale = TRUE, center = TRUE) %>%
    as.data.frame(.)

  # Empty list that further stores the clustering results
  soil_km <- list()

  cat("\nCluster soil data:\n")
  pb <- progress_estimated(length(clusters_k))
  # Loop over all defined number of classes and apply kmeans to the soilgrids data.
  for(i_clust in clusters_k) {
    soil_km[["n"%_%i_clust]] <- kmeans(x = clst_tbl,centers = i_clust, iter.max = 100)
    pb$tick()$print()
  }
  pb$stop()

  return(soil_km)
}

#-------------------------------------------------------------------------------
#' Evaluate Clustering Results
#'
#' Evaluate the results of the different \link[=cluster_soil]{kmeans clusters} by calculating the
#'   sum of squared differences for each one (between the clustered and the raw data).
#'
#' @param cluster_result Results of the soilgrids clustering stored in the \strong{soil project}.
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_x_continuous
#' theme_bw xlab ylab
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

  int <- max(round(nrow(sse_dat)/10),1)
  x_breaks <- seq(int, max(sse_dat$n_class), int)

  sse_plot <- ggplot(data = sse_dat, aes(x = n_class, y = norm_within_ssq)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = x_breaks, minor_breaks = 1:max(sse_dat$n_class)) +
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


#-------------------------------------------------------------------------------
#' Plot clustered soil map
#'
#' Internal/Actual general-function to plot the clustered soil maps.
#'
#' @param soil_data The cluster results output from \code{cluster_soilgrids()}.
#' @param cluster_k Numeric. Number of soil classes to plot in the soil map.
#'
#' @importFrom dplyr mutate full_join
#' @importFrom ggplot2 aes coord_equal geom_raster ggplot guide_legend
#'   scale_fill_manual theme theme_bw element_text
#' @importFrom magrittr %>% %<>% set_colnames
#' @importFrom pasta %//% %_%
#' @importFrom raster extent raster rasterToPoints
#' @importFrom RColorBrewer brewer.pal
#' @importFrom tibble as_tibble enframe
#'
#' @return Plots the final soilmap with the selected number of soil classes.
#'
#' @keywords internal

plot_soilmap <- function(soil_data, cluster_k){

  #Checking of number of soil classes
  if(is.null(cluster_k)){
    stop("No number of soil classes defined!\n"%&%
           "Either set final number of classes with select_cluster()"%&&%
           "or define the number of classes in the function!")
  }
  if(!("n"%_%cluster_k %in% names(soil_data$soil_cluster))){
    stop("Selected number of classes not available!")
  }

  # Clustering results for the selected number of soil classes in a tibble
  clust_sel <- soil_data$soil_cluster[["n"%_%cluster_k]]$cluster %>%
    enframe()

  # Assign the cluster group indices to vector with length of the final raster map.
  clust_rst <- rep(NA, soil_data$soilgrids$meta$layer$len_rst)
  clust_rst[which(!is.na(soil_data$soilgrids$meta$layer$has_value))] <- clust_sel$value

  # Reshape the vector and create raster map.
  clust_rst %<>%
    matrix(ncol = soil_data$soilgrids$meta$layer$dim_rst[1],
           nrow = soil_data$soilgrids$meta$layer$dim_rst[2]) %>%
    t() %>%
    raster(crs = soil_data$soilgrids$meta$layer$crs)

  # Assign the shape files' extent and provide a nodata value
  extent(clust_rst) <- soil_data$soilgrids$meta$layer$extent
  clust_rst@file@nodatavalue <- -32768

  clust_tbl <- clust_rst %>%
    rasterToPoints(.) %>%
    as_tibble(.) %>%
    set_colnames(c("x", "y", "soil_class")) %>%
    mutate(soil_class = as.factor(soil_class))

  fill_col <- clust_tbl$soil_class %>%
    unique(.) %>%
    length(.) %>%
    colorRampPalette(brewer.pal(8, "Paired"))(.)

  n_col <- ceiling(length(fill_col) / 15)

  # shape <- soil_data$shape_file
  # shape@data$id <- rownames(shape@data)
  # shape_points <- fortify(shape, region = "id")
  # shape_df <- full_join(shape_points, shape@data, by = "id")

  clust_plot <- ggplot() +
    geom_raster(data = clust_tbl, aes(x = x, y = y, fill = soil_class)) +
    scale_fill_manual(values = fill_col, guide = guide_legend(ncol = n_col)) +
    # geom_polygon(data = shape_df, aes(x = long, y = lat, group = Subbasin), col = "black", fill = NA) +
    coord_equal() +
    theme_bw() +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5))

  return(clust_plot)
}

#-------------------------------------------------------------------------------
#' Set Final Cluster Number
#'
#' Define the final number of clusters within the \strong{soil project} and
#' set the respective clustered soil data.
#'
#' @param soil_data Saved soil information in the soil_object.
#' @param cluster_k Numeric value of final number of classes.

#' @importFrom tibble add_column tibble
#' @importFrom dplyr group_by summarise_all
#' @importFrom purrr map
#' @importFrom magrittr %>%
#'
#' @keywords internal

set_cluster_data <- function(soil_data, cluster_k) {

  soil_clust <- soil_data$soil_cluster[["n"%_%cluster_k]]$cluster

  data_proc <- soil_data$data_processed %>%
    map(., function(tbl, add_col){ add_column(tbl, soil_class = add_col) %>%
        group_by(soil_class) %>%
        summarise_all(mean)}, soil_clust)

  data_proc$soil_class <- tibble(soil_class = soil_clust)

  return(data_proc)
}
