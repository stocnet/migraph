#' Watts-Strogatz small-world model for two-mode networks
#' 
#' Calculates small-world metrics for one- and two-mode networks.
#' @param object A matrix, igraph graph, or tidygraph object
#' @param times Number of simulations
#' @family two-mode measures
#' @return Returns a table of small-world related metrics for each second-mode
#' node.
#' @details The first column of the returned table is simply the number of
#' the second-mode column. 
#' The next three columns report the observed and
#' expected clustering, and the ratio of the former to the latter.
#' The next three columns report the observed and expected path-length,
#' and the ratio of the former to the later.
#' The last column reports the ratio of the observed/expected clustering ratio
#' to the observed/expected path-length ratio, which is known as a small-world
#' metric.
#' Expected clustering and paths is the mean of twomode_clustering and
#' mean_distance over 100 random simulations with the same row and column sums.
#' @examples
#' graph_smallworld(ison_southern_women)
#' graph_smallworld(ison_brandes)
#' @seealso \code{\link{graph_transitivity}} and \code{\link{graph_equivalency}}
#' for how clustering is calculated
#' @references 
#' Watts, Duncan J., and Steven H. Strogatz. 1998. 
#' “Collective Dynamics of ‘Small-World’ Networks.” 
#' Nature 393(6684):440–42.
#' @export
graph_smallworld <- function(object, times = 100) {
  
  if(is_twomode(object)){
    obsclust <- graph_equivalency(object)
    expclust <- mean(vapply(1:times, 
                       function(x) graph_equivalency(generate_random(object)),
                       FUN.VALUE = numeric(1)))
  } else {
    obsclust <- graph_transitivity(object)
    expclust <- mean(vapply(1:times, 
                            function(x) graph_transitivity(generate_random(object)),
                            FUN.VALUE = numeric(1)))
  }
  
    obspath <- graph_length(object)
    exppath <- mean(vapply(1:times, 
                            function(x) graph_length(generate_random(object)),
                            FUN.VALUE = numeric(1)))

  
  (obsclust/expclust)/(obspath/exppath)
}

