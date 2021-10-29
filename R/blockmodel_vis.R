#' ggplot2-based plotting of blockmodel results
#' @name blockmodel_vis
#' @param x A blockmodel-class object.
#' @param ... Additional arguments passed on to ggplot2.
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot geom_tile aes scale_fill_gradient theme_grey labs theme scale_x_discrete scale_y_discrete geom_vline geom_hline element_blank element_text
#' @importFrom rlang .data
#' @examples 
#' usa_concor <- blockmodel_concor(mpn_elite_usa_advice)
#' plot(usa_concor)
#' @export
plot.blockmodel <- function(x, ...){
  plot_data <- x$blocked.data
  plot_data <- as.data.frame(plot_data) %>%
    tibble::rownames_to_column("Var1") %>%
    tidyr::pivot_longer(!.data$Var1, names_to = "Var2", values_to = "value")
  g <- ggplot2::ggplot(plot_data, ggplot2::aes(.data$Var2, .data$Var1)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$value), colour = "white") +
    ggplot2::scale_fill_gradient(low = "white", high = "black") +
    ggplot2::theme_grey(base_size = 9) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(legend.position = "none",
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 9 * 0.8,
                                                       colour = "grey50"),
                   axis.text.x = ggplot2::element_text(size = 9 * 0.8,
                                                       angle = 30, hjust = 0,
                                                       colour = "grey50"))
  
  if (x$modes == 1) {
    g <- g + ggplot2::scale_x_discrete(
                        expand = c(0, 0),
                        position = "top",
                        limits = colnames(x$blocked.data)[x$order.vector]) +
      ggplot2::scale_y_discrete(
                        expand = c(0, 0),
                        limits = rev(rownames(x$blocked.data)[x$order.vector])) + 
      ggplot2::geom_vline(
                        xintercept = c(1 + which(diff(x$block.membership) != 0))
                        - .5,
                        colour = "red") +
      ggplot2::geom_hline(yintercept = nrow(x$blocked.data) -
                            c(1 + which(diff(x$block.membership) != 0)) +
                            1.5, 
                          colour = "red")
  } else {
    g <- g + ggplot2::scale_y_discrete(
                          expand = c(0, 0),
                          limits = rev(rownames(x$blocked.data)[x$order.vector$nodes1])) +
      ggplot2::scale_x_discrete(expand = c(0, 0),
                                position = "top",
                                limits = colnames(x$blocked.data)[x$order.vector$nodes2]) +
      ggplot2::geom_vline(xintercept =
                            c(1 + which(diff(x$block.membership$nodes2) != 0))
                            - .5,
                          colour = "blue") +
      ggplot2::geom_hline(yintercept = nrow(x$blocked.data)
                          - c(1 + which(diff(x$block.membership$nodes1) != 0))
                          + 1.5,
                          colour = "red")
  }
  g
  
}

#' Plots for deciding on the number of network clusters
#' @param hc a hierarchical cluster object
#' @param k number of clusters. By default NULL,
#' but, if specified, `ggtree` will color branches and
#' add a line to indicate where the corresponding cluster
#' cut would be.
#' @param method only "elbow" is currently implemented.
#' @name blockmodel_vis
#' @importFrom ggdendro ggdendrogram
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats cutree
#' @examples
#' res <- cluster_regular_equivalence(mpn_elite_mex)
#' ggtree(res, 4)
#' @export
ggtree <- function(hc, k = NULL){
  if (is.null(k)) {
    ggdendro::ggdendrogram(hc, rotate = TRUE)
  } else {
    colors <- suppressWarnings(RColorBrewer::brewer.pal(k, "Set1"))
    colors <- (colors[stats::cutree(hc, k = k)])[hc$order]
    ggdendro::ggdendrogram(hc, rotate = TRUE) +
      ggplot2::geom_hline(yintercept = hc$height[length(hc$order) - k],
                          linetype = 2,
                          color = "#E20020") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(colour = "#E20020"),
                     axis.text.y = suppressWarnings(
                       ggplot2::element_text(colour = colors)))
  }
  
}

#' @rdname blockmodel_vis
#' @param census output from some node_*_census function
#' @importFrom sna gcor
#' @importFrom stats cutree coef
#' @examples
#' ggidentify_clusters(res, node_triad_census(mpn_elite_mex))
#' @export
ggidentify_clusters <- function(hc, census, method = c("elbow", "strict")){
  
  vertices <- nrow(census)
  observedcorrelation <- cor(t(census))
  method <- match.arg(method)
  
  resultlist <- list()
  correlations <- vector()
  for (i in 2:(vertices)) {
    cluster_result <- list(label = NA, clusters = NA, correlation = NA)
    cluster_result$label <- paste("number of clusters: ", 
                                  i)
    clusters <- stats::cutree(hc, k = i)
    cluster_result$clusters <- clusters
    cluster_cor_mat <- clusterCorr(observedcorrelation, clusters)
    clustered_observed_cors <- sna::gcor(cluster_cor_mat, observedcorrelation)
    cluster_result$correlation <- (clustered_observed_cors)
    resultlist <- c(resultlist, cluster_result)
    correlations <- c(correlations, clustered_observed_cors)
  }

  resultlist$correlations <- c(correlations)
  dafr <- data.frame(clusters = 2:vertices, correlations = c(correlations))
  # resultlist
  correct <- NULL # to satisfy the error god
  
  # k identification method
  if(method == "elbow"){
    dafr$correct <- ifelse(dafr$clusters == elbow_finder(dafr$clusters, dafr$correlations),
                           "#E20020", "#6f7072")
  } else if (method == "strict"){
    dafr$correct <- "#6f7072"
    dafr$correct[which(elementwise.all.equal(dafr$correlations, 1))[1]] <- "#E20020"
  } else stop("This k selection method is not recognised")

  # plotting
  ggplot2::ggplot(dafr, aes(x = clusters, y = correlations)) +
    ggplot2::geom_line(color = "#6f7072") +
    ggplot2::geom_point(aes(color = correct), size = 2) +
    ggplot2::scale_color_manual(values = c("#6f7072", "#E20020")) +
    # ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::theme_minimal() +
    ggplot2::guides(color = "none")
}

elbow_finder <- function(x_values, y_values) {
  # Max values to create line
  if(min(x_values)==1) x_values <- x_values[2:length(x_values)]
  if(min(y_values)==0) y_values <- y_values[2:length(y_values)]
  max_df <- data.frame(x = c(min(x_values), max(x_values)), 
                       y = c(min(y_values), max(y_values)))
  # Creating straight line between the max values
  fit <- lm(max_df$y ~ max_df$x)
  # Distance from point to line
  distances <- vector()
  for (i in seq_len(length(x_values))) {
    distances <- c(distances,
                   abs(stats::coef(fit)[2]*x_values[i] -
                         y_values[i] +
                         coef(fit)[1]) /
                     sqrt(stats::coef(fit)[2]^2 + 1^2))
  }
  # Max distance point
  x_max_dist <- x_values[which.max(distances)]
  x_max_dist
}

clusterCorr <- function(observed_cor_matrix, cluster_vector) {
  num_vertices = nrow(observed_cor_matrix)
  cluster_cor_mat <- observed_cor_matrix

  obycor <- function(i, j) mean(observed_cor_matrix[which(cluster_vector[row(observed_cor_matrix)] ==
                                         cluster_vector[i] &
                                         cluster_vector[col(observed_cor_matrix)] ==
                                         cluster_vector[j])])
  obycor_v <- Vectorize(obycor)
  cluster_cor_mat <- outer(1:num_vertices,
                           1:num_vertices,
                           obycor_v)
  dimnames(cluster_cor_mat) <- dimnames(observed_cor_matrix)
  cluster_cor_mat
}

elementwise.all.equal <- Vectorize(function(x, y) {isTRUE(all.equal(x, y))})

