#' Visualising graphs and identifying nodes with maximum values of the specified
#' measure.
#' @param object a migraph-consistent object
#' @param node_measure some arbitrary function that runs on the object and
#' returns a numeric vector that can be used to scale the nodes
#' @param identify_function a function for the identification of a single node,
#' e.g. max, min, mean, etc.
#' @examples 
#' ggidentify(brandes, node_degree)
#' ggidentify(brandes, node_betweenness)
#' ggidentify(brandes, node_closeness)
#' ggidentify(brandes, node_eigenvector)
#' @export
ggidentify <- function(object, node_measure, identify_function = max){

  measure <- node_measure(object)
  colord <- ifelse(measure == identify_function(measure),
               "max", "other")
  
  ggraph::ggraph(object) + 
    ggplot2::theme_void() +
    ggraph::geom_edge_link() +
    ggraph::geom_node_point(aes(size = measure,
                                colour = colord)) +
    ggplot2::scale_color_manual(breaks = c("max", "other"),
                                values = c("red", "blue")) + 
    ggplot2::theme(legend.position = "none")
}

#' Plots for deciding on the number of network clusters
#' @param hc a hierarchical cluster object
#' @param k number of clusters. By default NULL,
#' but, if specified, `ggtree` will color branches and
#' add a line to indicate where the corresponding cluster
#' cut would be.
#' @param mat the matrix
#' @param method only "elbow" is currently implemented.
#' @name identify_clusters
#' @importFrom ggdendro ggdendrogram
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats cutree
#' @examples
#' res <- cluster_regular_equivalence(mpn_elite_mex)
#' ggtree(res, 4)
#' @export
ggtree <- function(hc, k = NULL){
  
  if(is.null(k)){
    ggdendro::ggdendrogram(hc, rotate = TRUE)
  } else {
    
    colors <- RColorBrewer::brewer.pal(k, "Set3")
    colors <- (colors[stats::cutree(hc, k = k)])[hc$order]
    
    ggdendro::ggdendrogram(hc, rotate = TRUE) +
      ggplot2::geom_hline(yintercept = hc$height[length(hc$order) - k], linetype = 2,
                          color = "#E20020") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(colour = "#E20020"),
                     axis.text.y = suppressWarnings(ggplot2::element_text(colour = colors)))
  }
  
}

#' @rdname identify_clusters
#' @importFrom sna gcor
#' @importFrom stats cutree coef
#' @examples
#' ggidentify_clusters(res, mpn_mex_elite)
#' @export
ggidentify_clusters <- function(hc, mat, method = "elbow"){
  
  vertices <- ncol(mat)
  observedcorrelation <- cor(mat)
  
  clusterCorr <- function (observed_cor_matrix, cluster_vector){
    num_vertices = nrow(observed_cor_matrix)
    cluster_cor_mat <- observed_cor_matrix
    for (i in 1:num_vertices) {
      for (j in 1:num_vertices) {
        cluster_cor_mat[i, j] = mean(observed_cor_matrix[which(cluster_vector[row(observed_cor_matrix)] == 
                                                                 cluster_vector[i] & cluster_vector[col(observed_cor_matrix)] == 
                                                                 cluster_vector[j])])
      }
    }
    return(cluster_cor_mat)
  }
  
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
  resultlist$correlations <- c(0, correlations)
  
  dafr <- data.frame(clusters = 1:vertices, correlations = c(0, correlations))
  # resultlist
  
  correct <- NULL # to satisfy the error god
  elbow_finder <- function(x_values, y_values) {
    # Max values to create line
    max_df <- data.frame(x = c(min(x_values), max(x_values)), 
                         y = c(min(y_values), max(y_values)))
    
    # Creating straight line between the max values
    fit <- lm(max_df$y ~ max_df$x)
    
    # Distance from point to line
    distances <- c()
    for(i in 1:length(x_values)) {
      distances <- c(distances, 
                     abs(stats::coef(fit)[2]*x_values[i] - y_values[i] + coef(fit)[1]) / 
                       sqrt(stats::coef(fit)[2]^2 + 1^2))
    }
    
    # Max distance point
    x_max_dist <- x_values[which.max(distances)]
    # y_max_dist <- y_values[which.max(distances)]
    
    # return(c(x_max_dist, y_max_dist))
    x_max_dist
  }
  dafr$correct <- ifelse(dafr$clusters == elbow_finder(dafr$clusters, dafr$correlations),
                         "#E20020", "#6f7072")
  # dapr <- data.frame(clusters = 1:vertices,
  #                    correlations = c(0, correlations),
  #                    correct = ifelse(dafr$clusters == elbow_finder(dafr$clusters, dafr$correlations),
  #                                     "#E20020", "#6f7072"))
  # dapr[1,] <- NULL
  
  ggplot2::ggplot(dafr, aes(x = clusters, y = correlations)) + 
    ggplot2::geom_line(color = "#6f7072") +
    ggplot2::geom_point(aes(color = correct), size = 2) + 
    ggplot2::scale_color_manual(values = c("#6f7072", "#E20020")) +
    ggplot2::scale_y_continuous(limits = c(0,1)) +
    ggplot2::theme_minimal() + 
    ggplot2::guides(color = "none")
}

