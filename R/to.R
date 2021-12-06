#' Tools for reformatting networks, graphs, and matrices
#' 
#' These functions offer tools for transforming certain properties of migraph-consistent objects 
#' (that is, matrices, igraph, tidygraph, or network objects).
#' 
#' @details 
#' Since some modifications are easier to implement for some objects than others,
#' here are the currently implemented modifications:
#' 
#' |  to_      | edgelists | matrices  |igraph  |tidygraph  |network  |
#' | ------------- |:-----:|:-----:|:-----:|:-----:|:-----:|
#' | unweighted  |  | X | X | X | X |
#' | undirected  |  | X | X | X | X |
#' | unsigned  |  |  | X | X |   |
#' | uniplex  |  |   | X | X |   |
#' | unnamed  | X | X | X | X | X |
#' | named  |  | X | X | X | X |
#' | simplex  |  | X | X | X |   |
#' | main_component  |  |   | X | X | X |
#' | onemode  |  |   | X | X |   |
#' | multilevel  |  | X | X | X |   |
#' @name to
#' @family manipulation
#' @param object A matrix, `{igraph}` graph, `{tidygraph}` tbl_graph, or
#' `{network}` object.
#' @param edge the name of an edge attribute to retain from a graph
#' @param keep in the case of a signed network, whether to retain
#' the "positive" or "negative" ties
#' @param threshold For a matrix, the threshold to binarise/dichotomise at.
#' @returns
#' All `to_` functions return an object of the same class as that provided. 
#' So passing it an igraph object will return an igraph object
#' and passing it a network object will return a network object,
#' with certain modifications as outlined below:
#' - `to_unweighted()` returns an object that has all edge weights removed
#' - `to_unnamed()` returns an object that has all vertex names removed
#' - `to_named()` returns an object that has random vertex names added
#' - `to_undirected()` returns an object that has any edge direction removed
#' - `to_onemode()` returns an object that has any type/mode attributes removed,
#' but otherwise includes all the same nodes and ties.
#' Note that this is not the same as `project_rows()` or `project_cols()`,
#' which return only some of the nodes and new ties established by coincidence.
#' - `to_main_component()` returns an object that includes only the main component
#' and not any smaller components or isolates
#' - `to_uniplex()` returns an object that includes only a single type of tie
#' - `to_simplex()` returns an object that has all loops or self-ties removed
#' - `to_unsigned()` returns an object that has 
#' @examples
#' to_unweighted(project_rows(southern_women))
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

#' @rdname to
#' @examples
#' to_unnamed(project_rows(southern_women))
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

#' @rdname to
#' @examples
#' to_undirected(ison_coleman)
#' @export
to_undirected <- function(object) UseMethod("to_undirected")

#' @importFrom igraph as.undirected
#' @export
to_undirected.igraph <- function(object) {
  igraph::as.undirected(object)
}

#' @importFrom igraph as.undirected
#' @export
to_undirected.tbl_graph <- function(object) {
  as_tidygraph(igraph::as.undirected(object))
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

#' @rdname to
#' @importFrom igraph delete_vertex_attr
#' @examples
#' to_onemode(ison_marvel_teams)
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

#' @rdname to
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
                           which(!sna::component.largest(object, result = "membership")))
}

#' @rdname to
#' @importFrom igraph delete_edges edge_attr_names delete_edge_attr
#' @importFrom igraph E get.edge.attribute edge_attr_names
#' @examples
#' to_uniplex(ison_m182, "friend_tie")
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
  if (is.numeric(igraph::get.edge.attribute(object, edge))) names(igraph::edge_attr(out)) <- "weight"
  out
}

#' @rdname to
#' @examples
#' to_unsigned(ison_marvel_relationships, "positive")
#' to_unsigned(ison_marvel_relationships, "negative")
#' @export
to_unsigned <- function(object, keep = c("positive", "negative")) UseMethod("to_unsigned")

#' @export
to_unsigned.tbl_graph <- function(object, keep = c("positive", "negative")){
  out <- to_unsigned(as_igraph(object))
  as_tidygraph(out)
}

#' @export
to_unsigned.igraph <- function(object, keep = c("positive", "negative")){
  if (is_signed(object)) {
    keep <- match.arg(keep)
    if (keep == "positive") {
      out <- igraph::delete_edges(object, which(igraph::E(object)$sign < 0))
    } else {
      out <- igraph::delete_edges(object, which(igraph::E(object)$sign > 0))
    }
    out <- igraph::delete_edge_attr(out, "sign")
    out
  } else object
}

#' @rdname to
#' @importFrom igraph simplify
#' @examples
#' to_simplex(ison_m182)
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

#' @rdname to
#' @examples
#' to_named(ison_m182)
#' @export
to_named <- function(object) UseMethod("to_named")

#' @export
to_named.tbl_graph <- function(object) {
  object %>% mutate(name = sample(baby_names,
                                  igraph::vcount(object)))
}

#' @export
to_named.igraph <- function(object) {
  igraph::V(object)$name  <- sample(baby_names,
                                    igraph::vcount(object))
  object
}

#' @rdname to
#' @examples
#' to_multilevel(mpn_elite_usa_advice)
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

