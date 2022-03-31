#' Tools for reformatting networks, graphs, and matrices
#' 
#' These functions offer tools for transforming certain properties 
#' of migraph-consistent objects
#' (that is, matrices, igraph, tidygraph, or network objects).
#' Unlike the `as_*()` group of functions,
#' these functions always return the same object type as they are given,
#' only transforming these objects' properties.
#' 
#' @details 
#' Since some modifications are easier to implement for some objects than others,
#' here are the currently implemented modifications:
#' 
#' |  to_      | edgelists | matrices  |igraph  |tidygraph  |network  |
#' | ------------- |:-----:|:-----:|:-----:|:-----:|:-----:|
#' | unweighted  | X | X | X | X | X |
#' | undirected  |  | X | X | X | X |
#' | unsigned  | X | X | X | X |   |
#' | uniplex  |  |   | X | X |   |
#' | unnamed  | X | X | X | X | X |
#' | named  | X | X | X | X | X |
#' | simplex  |  | X | X | X |   |
#' | main_component  |  |   | X | X | X |
#' | onemode  |  |   | X | X |   |
#' | multilevel  |  | X | X | X |   |
#' | mode1 | | X | X | X | |
#' | mode2 | | X | X | X | |
#' @name to
#' @family manipulation
#' @inheritParams is
#' @param edge Character string naming an edge attribute to retain from a graph.
#' @param keep In the case of a signed network, whether to retain
#' the "positive" or "negative" ties.
#' @param threshold For a matrix, the threshold to binarise/dichotomise at.
#' @param names Character vector of the node names. NULL by default.
#' @returns
#' All `to_` functions return an object of the same class as that provided. 
#' So passing it an igraph object will return an igraph object
#' and passing it a network object will return a network object,
#' with certain modifications as outlined for each function.
NULL

#' @describeIn to Returns an object that includes only a single type of tie
#' @importFrom igraph delete_edges edge_attr_names delete_edge_attr
#' @importFrom igraph E get.edge.attribute edge_attr_names
#' @examples
#' autographr(ison_algebra)
#' a <- to_uniplex(ison_algebra, "friend_tie")
#' autographr(a)
#' a <- to_main_component(a)
#' autographr(a)
#' a <- to_undirected(a)
#' autographr(a)
#' a <- to_unweighted(a)
#' autographr(a)
#' a <- to_named(a)
#' autographr(a)
#' @export
to_uniplex <- function(object, edge) UseMethod("to_uniplex")

#' @export
to_uniplex.tbl_graph <- function(object, edge){
  as_tidygraph(to_uniplex(as_igraph(object), edge))
}

#' @export
to_uniplex.igraph <- function(object, edge){
  out <- igraph::delete_edges(object,
                              igraph::E(object)[igraph::get.edge.attribute(object, edge) == 0])
  edge_names <- igraph::edge_attr_names(object)
  if (length(edge_names) > 1) {
    for (e in setdiff(edge_names, edge)) {
      out <- igraph::delete_edge_attr(out, e) 
    }
  }
  if (is.numeric(igraph::get.edge.attribute(object, edge))) 
    names(igraph::edge_attr(out)) <- "weight"
  out
}

#' @describeIn to Returns an object that includes only the main component
#' without any smaller components or isolates
#' @export
to_main_component <- function(object) UseMethod("to_main_component")

#' @export
to_main_component.tbl_graph <- function(object) {
  as_tidygraph(to_main_component(as_igraph(object)))
}

#' @export
to_main_component.igraph <- function(object) {
  comps <- igraph::components(object)
  max.comp <- which.max(comps$csize)
  igraph::delete.vertices(object, comps$membership != max.comp)
}

#' @export
to_main_component.network <- function(object) {
  network::delete.vertices(object, 
                           which(!sna::component.largest(object,
                                                         result = "membership")))
}

#' @describeIn to Returns an object that has any edge direction removed,
#'   so that any pair of nodes with at least one directed edge will be
#'   connected by an undirected edge in the new network.
#'   This is equivalent to the "collapse" mode in `{igraph}`.
#' @export
to_undirected <- function(object) UseMethod("to_undirected")

#' @importFrom igraph as.undirected
#' @export
to_undirected.igraph <- function(object) {
  igraph::as.undirected(object, edge.attr.comb = "first")
}

#' @export
to_undirected.tbl_graph <- function(object) {
  as_tidygraph(igraph::as.undirected(object, edge.attr.comb = "first"))
}

#' @export
to_undirected.network <- function(object) {
  object$gal$directed <- FALSE
  object
}

#' @export
to_undirected.matrix <- function(object) {
  if (is_twomode(object)) {
    object
  } else ((object + t(object)) > 0) * 1
}

#' @describeIn to Returns an object that has all edge weights removed
#' @export
to_unweighted <- function(object, threshold = 1) UseMethod("to_unweighted")

#' @export
to_unweighted.tbl_graph <- function(object, threshold = 1) {
  if ("weight" %in% igraph::edge_attr_names(object)) 
    object <- igraph::delete_edge_attr(object, "weight")
  tidygraph::as_tbl_graph(object)
}

#' @export
to_unweighted.igraph <- function(object, threshold = 1) {
    if ("weight" %in% igraph::edge_attr_names(object)) {
      igraph::delete_edge_attr(object, "weight")
    } else object
}

#' @export
to_unweighted.network <- function(object, threshold = 1) {
    out <- network::delete.edge.attribute(object,
                                          attrname = "weight")
    out
}

#' @export
to_unweighted.matrix <- function(object, threshold = 1) {
  object <- (object >= threshold)*1
  object
}

#' @export
to_unweighted.data.frame <- function(object, threshold = 1) {
  if(is_edgelist(object)) object[,1:2]
  else stop("Not an edgelist")
}

#' @describeIn to Returns a network with either just the "positive" ties
#'   or just the "negative" ties
#' @examples
#' autographr(to_unsigned(ison_marvel_relationships, "positive")) /
#' autographr(to_unsigned(ison_marvel_relationships, "negative"))
#' @export
to_unsigned <- function(object, 
                        keep = c("positive", "negative")) UseMethod("to_unsigned")

#' @export
to_unsigned.matrix <- function(object, 
                               keep = c("positive", "negative")){
  keep <- match.arg(keep)
  out <- object
  if(keep == "positive"){
    out[out < 0] <- 0
  } else if (keep == "negative"){
    out[out > 0] <- 0
    out <- abs(out)
  } else stop("Indicate whether 'positive' or 'negative' ties should be kept.")
  out
}

#' @export
to_unsigned.data.frame <- function(object, 
                               keep = c("positive", "negative")){
  keep <- match.arg(keep)
  out <- object
  if(is_signed(object)){
    if(keep == "positive"){
      out$sign[out$sign < 0] <- 0
    } else if (keep == "negative"){
      out$sign[out$sign > 0] <- 0
      out$sign <- out$sign(out)
    } else stop("Indicate whether 'positive' or 'negative' ties should be kept.")
  }
  out
}

#' @export
to_unsigned.tbl_graph <- function(object, 
                                  keep = c("positive", "negative")){
  keep <- match.arg(keep)
  out <- to_unsigned(as_igraph(object), keep = keep)
  as_tidygraph(out)
}

#' @export
to_unsigned.igraph <- function(object, 
                               keep = c("positive", "negative")){
  if (is_signed(object)) {
    keep <- match.arg(keep)
    if (keep == "positive") {
      out <- igraph::delete_edges(object, 
                                  which(igraph::E(object)$sign < 0))
    } else {
      out <- igraph::delete_edges(object, 
                                  which(igraph::E(object)$sign > 0))
    }
    out <- igraph::delete_edge_attr(out, "sign")
    out
  } else object
}

#' @describeIn to Returns an object with all vertex names removed
#' @export
to_unnamed <- function(object) UseMethod("to_unnamed")

#' @export
to_unnamed.igraph <- function(object) {
  if ("name" %in% igraph::vertex_attr_names(object)) {
    igraph::delete_vertex_attr(object, "name")
  } else object
}

#' @export
to_unnamed.tbl_graph <- function(object) {
  out <- igraph::delete_vertex_attr(object, "name")
  tidygraph::as_tbl_graph(out)
}

#' @export
to_unnamed.network <- function(object) {
  out <- network::delete.vertex.attribute(object, "vertex.names")
  out
}

#' @export
to_unnamed.matrix <- function(object) {
  out <- object
  rownames(out) <- NULL
  colnames(out) <- NULL
  out
}

#' @export
to_unnamed.data.frame <- function(object) {
  out <- object
  names <- unique(unlist(c(out[,1],out[,2])))
  out[,1] <- match(unlist(object[,1]), names)
  out[,2] <- match(unlist(object[,2]), names)
  tibble::as_tibble(out)
}

#' @describeIn to Returns an object that has random vertex names added
#' @export
to_named <- function(object, names = NULL) UseMethod("to_named")

#' @export
to_named.tbl_graph <- function(object, names = NULL) {
  if (!is.null(names)) {
    object <- object %>% mutate(name = names)
  } else {
    object <- object %>% mutate(name = sample(baby_names,
                                              graph_nodes(object)))
  }
  object
}

#' @export
to_named.igraph <- function(object, names = NULL) {
  if (!is.null(names)) {
    igraph::V(object)$name  <- names
  } else {
    igraph::V(object)$name  <- sample(baby_names,
                                      graph_nodes(object))
  }
  object
}

#' @export
to_named.data.frame <- function(object, names = NULL) {
  if (!is.null(names)) {
    object[,1]  <- names[as.numeric(object[,1])]
    object[,2]  <- names[as.numeric(object[,2])]
  } else {
    object[,1]  <- sample(baby_names, 
                          graph_nodes(object))[as.numeric(object[,1])]
    object[,2]  <- sample(baby_names, 
                          graph_nodes(object))[as.numeric(object[,2])]
  }
  object
}

#' @export
to_named.matrix <- function(object, names = NULL) {
  if(is.null(names)) names <- sample(baby_names, 
                                     graph_nodes(object))
  if(is_twomode(object)){
    rownames(object)  <- names[1:nrow(object)]
    colnames(object)  <- names[(nrow(object)+1):length(names)]
  } else {
    rownames(object)  <- names
    colnames(object)  <- names
  }
  object
}

#' @describeIn to Returns an object that has all loops or self-ties removed
#' @importFrom igraph simplify
#' @export
to_simplex <- function(object) UseMethod("to_simplex")

#' @export
to_simplex.tbl_graph <- function(object) {
  as_tidygraph(to_simplex(as_igraph(object)))
}

#' @export
to_simplex.igraph <- function(object) {
  igraph::simplify(object)
}

#' @export
to_simplex.matrix <- function(object) {
  out <- object
  diag(out) <- 0
  out
}

#' @describeIn to Results in a weighted one-mode object
#' that retains the row nodes from a two-mode object,
#' and weights the ties between them on the basis of
#' their joint ties to nodes in the second mode (columns)
#' @importFrom igraph bipartite.projection
#' @examples
#' autographr(ison_southern_women) /
#' (autographr(to_mode1(ison_southern_women)) |
#' autographr(to_mode2(ison_southern_women)))
#' @export
to_mode1 <- function(object) UseMethod("to_mode1")

#' @export
to_mode1.matrix <- function(object) {
  object %*% t(object)
}

#' @export
to_mode1.igraph <- function(object) {
  igraph::bipartite.projection(object)$proj1
}

#' @export
to_mode1.tbl_graph <- function(object) {
  as_tidygraph(igraph::bipartite.projection(object)$proj1)
}

#' @describeIn to Results in a weighted one-mode object
#' that retains the column nodes from a two-mode object,
#' and weights the ties between them on the basis of
#' their joint ties to nodes in the first mode (rows).
#' @export
to_mode2 <- function(object) UseMethod("to_mode2")

#' @export
to_mode2.matrix <- function(object) {
  t(object) %*% object
}

#' @export
to_mode2.igraph <- function(object) {
  igraph::bipartite.projection(object)$proj2
}

#' @export
to_mode2.tbl_graph <- function(object) {
  as_tidygraph(igraph::bipartite.projection(object)$proj2)
}

#' @describeIn to Returns an object that has any type/mode attributes removed,
#'   but otherwise includes all the same nodes and ties.
#'   Note that this is not the same as `to_mode1()` or `to_mode2()`,
#'   which return only some of the nodes and new ties established by coincidence.
#' @importFrom igraph delete_vertex_attr
#' @export
to_onemode <- function(object) UseMethod("to_onemode")

#' @export
to_onemode.tbl_graph <- function(object) {
  as_tidygraph(to_onemode(as_igraph(object)))
}

#' @export
to_onemode.igraph <- function(object) {
  if ("type" %in% igraph::vertex_attr_names(object)) object <- igraph::delete_vertex_attr(object, "type")
  object
}

#' @describeIn to Returns a network that is not divided into two mode types
#'   but embeds two or more modes into a multimodal network structure.
#' @export
to_multilevel <- function(object) UseMethod("to_multilevel")

#' @export
to_multilevel.tbl_graph <- function(object) {
  as_tidygraph(to_multilevel(as_igraph(object)))
}

#' @export
to_multilevel.igraph <- function(object) {
  igraph::V(object)$lvl <- ifelse(igraph::V(object)$type, 2, 1)
  object <- igraph::delete_vertex_attr(object, "type")
  object
}

#' @export
to_multilevel.matrix <- function(object) {
  top <- cbind(matrix(0, nrow(object), nrow(object)), object)
  bottom <- cbind(t(object), matrix(0, ncol(object), ncol(object)))
  out <- rbind(top, bottom)
  colnames(out) <- rownames(out)
  out
}

#' @describeIn to Returns a matrix (named if possible) 
#'   where the edges are the nodes
#' @examples
#' autographr(ison_adolescents) +  
#' autographr(to_edges(ison_adolescents))
#' @export
to_edges <- function(object){
  edges <- as_edgelist(object)
  edges <- paste(edges$from, edges$to, sep = "-")
  edges <- expand.grid(edges, edges)
  edges$value <- (stringr::str_remove(edges$Var1, "-.*$") ==
                    stringr::str_remove(edges$Var2, "-.*$") |
                  stringr::str_remove(edges$Var1, "^.*-") ==
                    stringr::str_remove(edges$Var2, "-.*$") |
                  stringr::str_remove(edges$Var1, "-.*$") ==
                    stringr::str_remove(edges$Var2, "^.*-") |
                  stringr::str_remove(edges$Var1, "^.*-") ==
                    stringr::str_remove(edges$Var2, "^.*-"))*1
  edges <- dplyr::filter(edges, .data$value == 1)
  edges$value <- NULL
  names(edges) <- c("from","to")
  as_matrix(edges)
}


#' @describeIn to Returns a network subgraph filtered
#'   on the basis of some node-related logical statement.
#' @param ... Arguments passed on to dplyr::filter
#' @importFrom dplyr filter
#' @export
to_subgraph <- function(object, ...){
  dplyr::filter(.data = object, ..., .preserve = FALSE)
}
