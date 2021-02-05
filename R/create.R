#' Create networks that conform to particular structures
#' 
#' These functions create a host of different network objects.
#' Despite the common syntax, what distinguishes them from
#' those in other packages is that passing the `n` argument
#' a vector of \emph{two} integers will return a two-mode
#' network instead of a one-mode network.
#' 
#' @name create
#' @family creation
#' @param n Number of nodes. 
#' If a single integer is given, the function will create a one-mode network.
#' If a vector of two integers is given, e.g. `n = c(5,10)`,
#' the function will create a two-mode network.
#' @return By default an igraph object will be returned,
#' but this can be coerced into other types of objects
#' using `as_matrix()` or `as_tidygraph()`.
#' @importFrom tidygraph as_tbl_graph
#' @importFrom igraph graph_from_incidence_matrix
#' @seealso as_matrix as_tidygraph as_network
#' @details `create_empty()` creates an empty graph of the given dimensions.
#' @examples
#' g <- create_empty(c(8,6))
#' plot(g)
#' @export
create_empty <- function(n){
  
  if(length(n)==1){
    out <- matrix(0, n, n)
    out <- igraph::graph_from_adjacency_matrix(out)
  } else if (length(n)==2){
    out <- matrix(0, n[1], n[2])
    out <- igraph::graph_from_incidence_matrix(out)
  } else stop("`n` should be a single integer for a one-mode network or a vector of two integers for a two-mode network.")
  
  out
}

#' @rdname create
#' @details `create_complete()` creates a filled graph of the given dimensions.
#' @examples
#' g <- create_complete(c(8,6))
#' plot(g)
#' @export
create_complete <- function(n){
  
  if(length(n)==1){
    out <- matrix(1, n, n)
    out <- igraph::graph_from_adjacency_matrix(out)
  } else if (length(n)==2){
    out <- matrix(1, n[1], n[2])
    out <- igraph::graph_from_incidence_matrix(out)
  } else stop("`n` should be a single integer for a one-mode network or a vector of two integers for a two-mode network.")
  
  out
}

#' @rdname create
#' @param width The width or breadth of the ring. This is typically double the degree.
#' @param directed Whether the graph should be directed. By default FALSE.
#' @param ... Additional arguments passed on to igraph.
#' @details `create_ring()` creates a ring or chord graph of the given dimensions
#' that loops around is of a certain width or thickness.
#' @examples
#' g <- create_ring(c(8,6), width = 2)
#' plot(g)
#' @export
create_ring <- function(n, width = 1, directed = FALSE, ...) {
  
  roll_over <- function(w){
    cbind(w[,ncol(w)], w[,1:(ncol(w)-1)])
  }
  
  if(length(n)==1){
    if(width==1){
     out <- igraph::make_ring(n, directed, ...) 
    } else {
      out <- w <- as_matrix(igraph::make_ring(n, directed, ...))
      for(i in 1:(width-1)){
        w <- roll_over(w)
        out <- out + w
      }
      diag(out) <- 0
      out[out > 1] <- 1
      if(directed){
        out <- igraph::graph_from_adjacency_matrix(out, mode = "directed")
      } else out <- igraph::graph_from_adjacency_matrix(out, mode = "undirected")
    }
  } else if (length(n)==2){
    n1 <- n[1]
    n2 <- n[2]
    mat <- matrix(0, n1, n2)
    diag(mat) <- 1
    while(any(rowSums(mat)==0)){
      top <- mat[rowSums(mat)==1,]
      bot <- mat[rowSums(mat)==0,]
      diag(bot) <- 1
      mat <- rbind(top, bot)
    }
    while(any(colSums(mat)==0)){
      left <- mat[,colSums(mat)==1]
      right <- mat[,colSums(mat)==0]
      diag(right) <- 1
      mat <- cbind(left, right)
    }
    if(width!=0) mat <- mat + roll_over(mat)
    if(width > 1){
      for(i in 1:(width-1)){
        w <- roll_over(mat)
        mat <- mat + w
      }
    }
    out <- igraph::graph_from_incidence_matrix(mat)
  } else stop("`n` should be a single integer for a one-mode network or a vector of two integers for a two-mode network.")

  out
}

#' @rdname create
#' @details Creates a two-component two-mode network.
#' Will construct an affiliation matrix,
#' with full component diagonal.
#' TODO: Allow specfication of how many silos/components to create
#' @importFrom tidygraph as_tbl_graph
#' @importFrom igraph graph_from_incidence_matrix
#' @examples
#' create_components(c(10, 12))
#' @export
create_components <- function(n) {
  n1 <- n[1]
  n2 <- n[2]
  mat <- matrix(0, n1, n2)
  mat[1:round(n1 / 2), 1:round(n2 / 2)] <- 1
  mat[rowSums(mat) < 1, colSums(mat) < 1] <- 1
  mat
}

# #' @rdname create
#' #' @details Creates a nested two-mode network.
#' #' Will construct an affiliation matrix,
#' #' with decreasing fill across n2.
#' #' @importFrom tidygraph as_tbl_graph
#' #' @importFrom igraph graph_from_incidence_matrix
#' #' @examples
#' #' \dontrun{
#' #' create_nest(10, 12)
#' #' }
#' #' @export
#' create_nest <- function(n1, n2,
#'                         as = c("tidygraph", "igraph", "matrix")) {
#' 
#'   as <- match.arg(as)
#' 
#'   out <- matrix(0, n1, n2)
#'   out[(row(out) - col(out)) >= 0] <- 1
#' 
#'   if(as == "tidygraph") out <- tidygraph::as_tbl_graph(out)
#'   if(as == "igraph") out <- igraph::graph_from_incidence_matrix(out)
#'   out
#' }
#' 
#' #' @rdname create
#' #' @details Will create a complete bipartite start graph
#' #' @importFrom tidygraph as_tbl_graph
#' #' @importFrom igraph graph_from_incidence_matrix
#' #' @examples
#' #' \dontrun{
#' #' create_star(1, 12)
#' #'}
#' #' @export
#' create_star <- function(n1 = 1, n2,
#'                         as = c("tidygraph", "igraph", "matrix")){
#'   as <- match.arg(as)
#' 
#'   out <- matrix(0, n1, n2)
#'   out[1,] <- 1
#' 
#'   if(as == "tidygraph") out <- tidygraph::as_tbl_graph(out)
#'   if(as == "igraph") out <- igraph::graph_from_incidence_matrix(out)
#'   out
#' }
#' 
#' # mat.dist <- matrix(0,5,3)
#' # mat.dist[1:2,1] <- 1
#' # mat.dist[,2] <- 1
#' # mat.dist[4:5,3] <- 1
#' #
#' # # mat.part <- matrix(0,5,5)
#' # # mat.part[1:3,1] <- 1
#' # # mat.part[1:2,2] <- 1
#' # # mat.part[4:5,3] <- 1
#' # # mat.part[4:5,4] <- 1
#' # # mat.part[3,5] <- 1
#' # #
#' # mat.part <- mat.dist
#' # mat.part[2,1] <- 0
#' # mat.part[1,2] <- 0
#' # mat.part[4,3] <- 0
#' #
#' # mat.side <- matrix(0,4,4)
#' # mat.side[1:4,1] <- 1
#' # mat.side[1,2] <- 1
#' # mat.side[2,3] <- 1
#' # mat.side[3,4] <- 1
#' #
#' # mat.core <- matrix(0,4,4)
#' # mat.core[1:4,1] <- 1
#' # mat.core[1:2,2] <- 1
#' # mat.core[3:4,3] <- 1
#' # mat.core[1:2,4] <- 1
#' #
#' # mat.hier <- matrix(0,4,4)
#' # mat.hier[1:4,1] <- 1
#' # mat.hier[1:2,2] <- 1
#' # mat.hier[1:2,3] <- 1
#' # mat.hier[3:4,4] <- 1
