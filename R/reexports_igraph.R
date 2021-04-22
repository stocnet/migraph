#' @importFrom igraph is.igraph
#' @export
igraph::is.igraph

#' @importFrom igraph is_bipartite
#' @export
igraph::is_bipartite

#' @importFrom igraph is.bipartite
#' @return A logical vector whether object is two-mode (TRUE) or not (FALSE)
#' @examples
#' is_twomode(southern_women)
#' @export
is_twomode <- function(object){
  object <- as_igraph(object)
  igraph::is.bipartite(object)
}
