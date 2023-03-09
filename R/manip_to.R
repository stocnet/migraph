# Reformatting ####

#' Tools for reformatting networks, graphs, and matrices
#' 
#' @description
#' These functions offer tools for reformatting migraph-consistent objects
#' (matrices, igraph, tidygraph, or network objects).
#' Unlike the `as_*()` group of functions,
#' these functions always return the same object type as they are given,
#' only transforming these objects' properties.
#' @details
#' Since some modifications are easier to implement for some objects than others,
#' here are the currently implemented modifications:
#' 
#' |  to_      | edgelists | matrices  |igraph  |tidygraph  |network  |
#' | ------------- |:-----:|:-----:|:-----:|:-----:|:-----:|
#' | unweighted  | X | X | X | X | X |
#' | undirected  |  | X | X | X | X |
#' | redirected  | X | X | X | X |  |
#' | unsigned  | X | X | X | X |   |
#' | uniplex  |  |   | X | X |   |
#' | unnamed  | X | X | X | X | X |
#' | named  | X | X | X | X | X |
#' | simplex  |  | X | X | X |   |
#' | onemode  |  |   | X | X |   |
#' | multilevel  |  | X | X | X |   |
#' @name reformat
#' @family manipulations
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

#' @describeIn reformat Returns an object that includes only a single type of tie
#' @importFrom igraph delete_edges edge_attr_names delete_edge_attr
#' @importFrom igraph E get.edge.attribute edge_attr_names
#' @examples
#' autographr(ison_algebra)
#' a <- to_uniplex(ison_algebra, "friends")
#' autographr(a)
#' a <- to_giant(a)
#' autographr(a)
#' a <- to_undirected(a)
#' autographr(a)
#' a <- to_unweighted(a)
#' autographr(a)
#' @export
to_uniplex <- function(object, edge) UseMethod("to_uniplex")

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

#' @export
to_uniplex.tbl_graph <- function(object, edge){
  as_tidygraph(to_uniplex(as_igraph(object), edge))
}

#' @export
to_uniplex.network <- function(object, edge){
  as_network(to_uniplex(as_igraph(object), edge))
}

#' @export
to_uniplex.data.frame <- function(object, edge){
  as_edgelist(to_uniplex(as_igraph(object), edge))
}

#' @export
to_uniplex.matrix <- function(object, edge){
  as_matrix(to_uniplex(as_igraph(object), edge))
}

#' @describeIn reformat Returns an object that has any edge direction removed,
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

#' @export
to_undirected.data.frame <- function(object) {
  as_edgelist(to_undirected(as_igraph(object)))
}

#' @describeIn reformat Returns a directed object.
#'   Note that ties' direction will be randomly assigned.
#'   To flip the direction, use `to_redirected()`.
#'   To match the direction, use `to_reciprocated()`.
#' @export
to_directed <- function(.data) UseMethod("to_directed")

#' @export
to_directed.igraph <- function(.data) {
  if(!is_directed.igraph(.data))
    igraph::as.directed(.data, mode = "random")
  else .data
}

#' @describeIn reformat Returns an object that has any edge direction transposed,
#'   or flipped, so that senders become receivers and receivers become senders.
#'   This essentially has no effect on undirected networks or reciprocated ties.
#' @export
to_redirected <- function(.data) UseMethod("to_redirected")

#' @export
to_redirected.tbl_graph <- function(.data) {
  nodes <- NULL
  edges <- NULL
  out <- .data %>% activate(edges)
  out$from <- .data$to
  out$to <- .data$from
  out %>% activate(nodes)
}

#' @export
to_redirected.igraph <- function(.data) {
  igraph::reverse_edges(.data)
}

#' @export
to_redirected.data.frame <- function(.data) {
  out <- .data
  out$from <- .data$to
  out$to <- .data$from
  out
}

#' @export
to_redirected.matrix <- function(.data) {
  t(.data)
}

#' @export
to_redirected.network <- function(.data) {
  as_network(to_redirected(as_igraph(.data)))
}

#' @describeIn reformat Returns an object where all ties are reciprocated.
#' @export
to_reciprocated <- function(.data) UseMethod("to_reciprocated")

#' @export
to_reciprocated.igraph <- function(.data) {
  igraph::as.directed(.data, mode = "mutual")
}

#' @describeIn reformat Returns an object where all ties are acyclic.
#' @export
to_acyclic <- function(.data) UseMethod("to_acyclic")

#' @export
to_acyclic.igraph <- function(.data) {
  igraph::as.directed(.data, mode = "acyclic")
}

#' @describeIn reformat Returns an object that has all edge weights removed.
#' @export
to_unweighted <- function(object, threshold = 1) UseMethod("to_unweighted")

#' @export
to_unweighted.tbl_graph <- function(object, threshold = 1) {
  edges <- NULL
  weight <- NULL
  object %>% activate(edges) %>% 
    filter(weight >= threshold) %>% 
    select(-c(weight))
}

#' @export
to_unweighted.igraph <- function(object, threshold = 1) {
    as_igraph(to_unweighted(as_tidygraph(object), threshold))
}

#' @export
to_unweighted.network <- function(object, threshold = 1) {
  as_network(to_unweighted(as_tidygraph(object), threshold))
}

#' @export
to_unweighted.matrix <- function(object, threshold = 1) {
  (object >= threshold)*1
}

#' @export
to_unweighted.data.frame <- function(object, threshold = 1) {
  if(is_edgelist(object)) object[,1:2]
  else stop("Not an edgelist")
}

#' @describeIn reformat Returns a network with either just the "positive" ties
#'   or just the "negative" ties
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

#' @export
to_unsigned.network <- function(object, 
                               keep = c("positive", "negative")){
  as_network(to_unsigned(as_igraph(object)))
}

#' @describeIn reformat Returns an object with all vertex names removed
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
  dplyr::as_tibble(out)
}

#' @describeIn reformat Returns an object that has random vertex names added
#' @export
to_named <- function(object, names = NULL) UseMethod("to_named")

#' @export
to_named.tbl_graph <- function(object, names = NULL) {
  if (!is.null(names)) {
    object <- object %>% mutate(name = names)
  } else {
    object <- object %>% mutate(name = sample(baby_names,
                                              network_nodes(object)))
  }
  object
}

#' @export
to_named.igraph <- function(object, names = NULL) {
  if (!is.null(names)) {
    igraph::V(object)$name  <- names
  } else {
    igraph::V(object)$name  <- sample(baby_names,
                                      network_nodes(object))
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
                          network_nodes(object))[as.numeric(object[,1])]
    object[,2]  <- sample(baby_names, 
                          network_nodes(object))[as.numeric(object[,2])]
  }
  object
}

#' @export
to_named.matrix <- function(object, names = NULL) {
  if(is.null(names)) names <- sample(baby_names, 
                                     network_nodes(object))
  if(is_twomode(object)){
    rownames(object)  <- names[seq_len(nrow(object))]
    colnames(object)  <- names[(nrow(object)+1):length(names)]
  } else {
    rownames(object)  <- names
    colnames(object)  <- names
  }
  object
}

#' @export
to_named.network <- function(object, names = NULL) {
  as_network(to_named(as_igraph(object), names))
}

#' @describeIn reformat Returns an object that has all loops or self-ties removed
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

#' @describeIn reformat Returns an object that has any type/mode attributes removed,
#'   but otherwise includes all the same nodes and ties.
#'   Note that this is not the same as `to_mode1()` or `to_mode2()`,
#'   which return only some of the nodes and new ties established by coincidence.
#' @importFrom igraph delete_vertex_attr
#' @export
to_onemode <- function(object) UseMethod("to_onemode")

#' @export
to_onemode.matrix <- function(object) {
  if (is_twomode(object)){
    object <- rbind(cbind(matrix(0, nrow(object), nrow(object)), object),
                    cbind(t(object), matrix(0, ncol(object), ncol(object))))
    colnames(object) <- rownames(object)
  }
  object
}

#' @export
to_onemode.tbl_graph <- function(object) {
  as_tidygraph(to_onemode(as_igraph(object)))
}

#' @export
to_onemode.igraph <- function(object) {
  if ("type" %in% igraph::vertex_attr_names(object)) 
    object <- igraph::delete_vertex_attr(object, "type")
  object
}

#' @describeIn reformat Returns a network that is not divided into two mode types
#'   but embeds two or more modes into a multimodal network structure.
#' @export
to_multilevel <- function(object) UseMethod("to_multilevel")

#' @export
to_multilevel.tbl_graph <- function(object) {
  as_tidygraph(to_multilevel(as_igraph(object)))
}

#' @export
to_multilevel.igraph <- function(object) {
  if(is_twomode(object)){
    igraph::V(object)$lvl <- ifelse(igraph::V(object)$type, 2, 1)
    object <- igraph::delete_vertex_attr(object, "type")
  }
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

#' @describeIn reformat Returns a network that divides the nodes into two mode types.
#' @param mark A logical vector marking two types or modes.
#'   By default "type".
#' @export
to_twomode <- function(object, mark) UseMethod("to_twomode")

#' @export
to_twomode.igraph <- function(object, mark){
  igraph::V(object)$type <- mark
  to_undirected(object)
}

#' @export
to_twomode.tbl_graph <- function(object, mark){
  as_tidygraph(to_twomode.igraph(object, mark))
}

#' @export
to_twomode.network <- function(object, mark){
  as_network(to_twomode(as_igraph(object, mark)))
}


# Transforming ####

#' Tools for transforming networks, graphs, and matrices
#' 
#' @description
#' These functions offer tools for transforming migraph-consistent objects
#' (matrices, igraph, tidygraph, or network objects).
#' Transforming means that the returned object may have different dimensions
#' than the original object.
#' @details
#' Since some modifications are easier to implement for some objects than others,
#' here are the currently implemented modifications:
#' 
#' |  to_      | edgelists | matrices  |igraph  |tidygraph  |network  |
#' | ------------- |:-----:|:-----:|:-----:|:-----:|:-----:|
#' | mode1 | X | X | X | X | X |
#' | mode2 | X | X | X | X | X |
#' | giant  | X | X | X | X | X |
#' | subgraph  | X | X | X | X | X |
#' | ties  | X | X | X | X | X |
#' | blocks  | X | X | X | X | X |
#' | matching | X | X | X | X | X |
#' @name transform
#' @family manipulations
#' @inheritParams reformat
NULL

#' @describeIn transform Results in a weighted one-mode object
#'   that retains the row nodes from a two-mode object,
#'   and weights the ties between them on the basis of
#'   their joint ties to nodes in the second mode (columns)
#' @param similarity Method for establishing ties,
#'   currently "count" (default), "jaccard", or "rand".
#'   "count" calculates the number of coinciding ties,
#'   and can be interpreted as indicating the degree of opportunities
#'   between nodes.
#'   "jaccard" uses this count as the numerator in a proportion,
#'   where the denominator consists of any cell where either node has a tie.
#'   It can be interpreted as opportunity weighted by participation.
#'   "rand", or the Simple Matching Coefficient,
#'   is a proportion where the numerator consists of the count of cells where
#'   both nodes are present or both are absent,
#'   over all possible cells.
#'   It can be interpreted as the (weighted) degree of behavioral mirroring
#'   between two nodes.
#'   "pearson" (Pearson's coefficient) and "yule" (Yule's Q)
#'   produce correlations for valued and binary data, respectively.
#'   Note that Yule's Q has a straightforward interpretation related to the odds ratio.
#' @importFrom igraph bipartite.projection
#' @examples
#' autographr(ison_southern_women) /
#' (autographr(to_mode1(ison_southern_women)) |
#' autographr(to_mode2(ison_southern_women)))
#' @export
to_mode1 <- function(object, similarity = c("count","jaccard","rand","pearson","yule")) UseMethod("to_mode1")

#' @export
to_mode1.matrix <- function(object, 
                            similarity = c("count","jaccard","rand","pearson","yule")) {
  similarity <- match.arg(similarity)
  a <- object %*% t(object)
  b <- object %*% (1 - t(object))
  c <- (1 - object) %*% t(object)
  d <- ncol(object) - a - b - c
  out <- switch(similarity,
         "count" = a,
         "jaccard" = a/(a + b + c),
         "rand" = (a + d)/(a + b + c + d),
         "sokalsneath1" = a/(a + 2 * (b + c)),
         "sokalsneath2" = a * d/sqrt((a + b) * (a + c) * (d + b) * (d + c)),
         "gowerlegendre" = (a - (b + c) + d)/(a + b + c + d),
         "rogerstanimoto" = (a + d)/(a + 2 * (b + c) + d),
         "czekanowski" = 2*a/(2 * a + b + c),
         "ochiai" = a/sqrt((a+b)*(a+c)),
         "pearson" = cor(t(object)),
         "yule" = (a*d - b*c)/(a*d + b*c))
  diag(out) <- 0
  out
}

#' @export
to_mode1.igraph <- function(object, similarity = c("count","jaccard","rand","pearson","yule")) {
  similarity <- match.arg(similarity)
  if(similarity == "count") igraph::bipartite.projection(object)$proj1
  else as_igraph(to_mode1(as_matrix(object), similarity))
}

#' @export
to_mode1.tbl_graph <- function(object, similarity = c("count","jaccard","rand","pearson","yule")) {
  as_tidygraph(to_mode1(as_igraph(object), similarity = similarity))
}

#' @export
to_mode1.network <- function(object, similarity = c("count","jaccard","rand","pearson","yule")) {
  as_network(to_mode1(as_matrix(object), similarity = similarity))
}

#' @export
to_mode1.data.frame <- function(object, similarity = c("count","jaccard","rand","pearson","yule")) {
  as_edgelist(to_mode1(as_matrix(object), similarity = similarity))
}

#' @describeIn transform Results in a weighted one-mode object
#' that retains the column nodes from a two-mode object,
#' and weights the ties between them on the basis of
#' their joint ties to nodes in the first mode (rows).
#' @export
to_mode2 <- function(object, similarity = c("count","jaccard","rand","pearson","yule")) UseMethod("to_mode2")

#' @export
to_mode2.matrix <- function(object, similarity = c("count","jaccard","rand","pearson","yule")) {
  similarity <- match.arg(similarity)
  a <- t(object) %*% object
  b <- t(object) %*% (1 - object)
  c <- (1 - t(object)) %*% object
  d <- nrow(object) - a - b - c
  out <- switch(similarity,
                "count" = a,
                "jaccard" = a/(a + b + c),
                "rand" = (a + d)/(a + b + c + d),
                "sokalsneath1" = a/(a + 2 * (b + c)),
                "sokalsneath2" = a * d/sqrt((a + b) * (a + c) * (d + b) * (d + c)),
                "gowerlegendre" = (a - (b + c) + d)/(a + b + c + d),
                "rogerstanimoto" = (a + d)/(a + 2 * (b + c) + d),
                "czekanowski" = 2*a/(2 * a + b + c),
                "ochiai" = a/sqrt((a+b)*(a+c)),
                "pearson" = cor(object),
                "yule" = (a*d - b*c)/(a*d + b*c))
  diag(out) <- 0
  out
}

#' @export
to_mode2.igraph <- function(object, similarity = c("count","jaccard","rand","pearson","yule")) {
  similarity <- match.arg(similarity)
  if(similarity == "count") igraph::bipartite.projection(object)$proj2
  else as_igraph(to_mode2(as_matrix(object), similarity))
}

#' @export
to_mode2.tbl_graph <- function(object, similarity = c("count","jaccard","rand","pearson","yule")) {
  as_tidygraph(to_mode2(as_igraph(object), similarity))
}

#' @export
to_mode2.network <- function(object, similarity = c("count","jaccard","rand","pearson","yule")) {
  as_network(to_mode2(as_matrix(object), similarity))
}

#' @export
to_mode2.data.frame <- function(object, similarity = c("count","jaccard","rand","pearson","yule")) {
  as_edgelist(to_mode2(as_matrix(object), similarity))
}

#' @describeIn transform Returns an object that includes only the main component
#' without any smaller components or isolates
#' @export
to_giant <- function(object) UseMethod("to_giant")

#' @export
to_giant.igraph <- function(object) {
  comps <- igraph::components(object)
  max.comp <- which.max(comps$csize)
  igraph::delete.vertices(object, comps$membership != max.comp)
}

#' @export
to_giant.network <- function(object) {
  network::delete.vertices(object, 
                           which(!sna::component.largest(object,
                                                         result = "membership")))
}

#' @export
to_giant.tbl_graph <- function(object) {
  as_tidygraph(to_giant(as_igraph(object)))
}

#' @export
to_giant.data.frame <- function(object) {
  as_edgelist(to_giant(as_igraph(object)))
}

#' @export
to_giant.matrix <- function(object) {
  as_matrix(to_giant(as_igraph(object)))
}

#' @describeIn transform Returns a network subgraph filtered
#'   on the basis of some node-related logical statement.
#' @param ... Arguments passed on to dplyr::filter
#' @importFrom dplyr filter
#' @export
to_subgraph <- function(object, ...) UseMethod("to_subgraph")

#' @export
to_subgraph.tbl_graph <- function(object, ...){
  dplyr::filter(.data = object, ..., 
                .preserve = FALSE)
}

#' @export
to_subgraph.igraph <- function(object, ...){
  as_igraph(to_subgraph(as_tidygraph(object), ...))
}

#' @export
to_subgraph.network <- function(object, ...){
  as_network(to_subgraph(as_tidygraph(object), ...))
}

#' @export
to_subgraph.data.frame <- function(object, ...){
  as_edgelist(to_subgraph(as_tidygraph(object), ...))
}

#' @export
to_subgraph.matrix <- function(object, ...){
  as_matrix(to_subgraph(as_tidygraph(object), ...))
}

#' @describeIn transform Returns a matrix (named if possible) 
#'   where the edges are the nodes
#' @importFrom igraph make_line_graph E
#' @examples
#' autographr(ison_adolescents) +  
#' autographr(to_ties(ison_adolescents))
#' @export
to_ties <- function(object) UseMethod("to_ties")

#' @export
to_ties.igraph <- function(object){
  out <- igraph::make_line_graph(object)
  out <- add_node_attribute(out, "name", attr(igraph::E(object), "vnames"))
  igraph::V(out)$name <- gsub("\\|", "-", igraph::V(out)$name)
  out
}

#' @export
to_ties.tbl_graph <- function(object){
  as_tidygraph(to_ties(as_igraph(object)))
}

#' @export
to_ties.network <- function(object){
  as_network(to_ties(as_igraph(object)))
}

#' @export
to_ties.data.frame <- function(object){
  as_edgelist(to_ties(as_igraph(object)))
}

#' @export
to_ties.matrix <- function(object){
  as_matrix(to_ties(as_igraph(object)))
}

#' @describeIn transform Returns a reduced graph from a given
#'   partition membership vector.
#'   Reduced graphs provide summary representations of network structures 
#'   by collapsing groups of connected nodes into single nodes 
#'   while preserving the topology of the original structures.
#' @param membership A vector of partition memberships.
#' @param FUN A function for summarising block content.
#'   By default `mean`.
#'   Other recommended options include `median`, `sum`,
#'   `min` or `max`.
#' @examples 
#' (adolblock <- to_blocks(ison_adolescents, 
#'   node_regular_equivalence(ison_adolescents, k = 3)))
#' autographr(adolblock)
#' @export
to_blocks <- function(object, membership, FUN = mean) UseMethod("to_blocks")

#' @export
to_blocks.matrix <- function(object, membership, FUN = mean){
  if(is_twomode(object)){
    mat <- to_onemode(object)
    m1_membs <- membership[!node_mode(object)]
    m2_membs <- membership[node_mode(object)]
    x <- length(unique(m1_membs))
    y <- length(unique(m2_membs))
    out <- matrix(nrow = unique(m1_membs)[x],
                  ncol = unique(m2_membs)[y])
    for(i in unique(m1_membs)) for (j in unique(m2_membs))
      out[i, j] <- FUN(mat[membership == i, 
                           membership == j, drop = FALSE], 
                       na.rm = TRUE)
    rownames(out) <- paste("Block", seq_len(unique(m1_membs)[x]))
    colnames(out) <- paste("Block", seq_len(unique(m2_membs)[y]))
  } else {
    mat <- object
    parts <- max(membership)
    out <- matrix(nrow = parts, 
                  ncol = parts)
    for(i in seq_len(parts)) for (j in seq_len(parts))
      out[i, j] <- FUN(mat[membership == i, 
                           membership == j, drop = FALSE], 
                       na.rm = TRUE)
    rownames(out) <- paste("Block", seq_len(parts))
    colnames(out) <- paste("Block", seq_len(parts))
  }
  out[is.na(out)] <- 0
  out
}

#' @export
to_blocks.igraph <- function(object, membership, FUN = mean){
  as_igraph(to_blocks(as_matrix(object), membership, FUN))
}

#' @export
to_blocks.network <- function(object, membership, FUN = mean){
  as_network(to_blocks(as_matrix(object), membership, FUN))
}

#' @export
to_blocks.data.frame <- function(object, membership, FUN = mean){
  as_edgelist(to_blocks(as_matrix(object), membership, FUN))
}

#' @export
to_blocks.tbl_graph <- function(object, membership, FUN = mean){
  as_tidygraph(to_blocks(as_matrix(object), membership, FUN))
}

#' @describeIn transform Returns a network with only
#'   matching ties
#' @section to_matching:
#'   `to_matching()` uses `{igraph}`'s `max_bipartite_match()`
#'   to return a network in which each node is only tied to
#'   one of its previous ties.
#'   The number of these ties left is its _cardinality_,
#'   and the algorithm seeks to maximise this such that,
#'   where possible, each node will be associated with just one
#'   node in the other mode or some other mark.
#'   The algorithm used is the push-relabel algorithm
#'   with greedy initialization and a global relabelling
#'   after every \eqn{\frac{n}{2}} steps,
#'   where \eqn{n} is the number of nodes in the network.
#' @references 
#'   Goldberg, A V; Tarjan, R E (1986). 
#'   "A new approach to the maximum flow problem". 
#'   _Proceedings of the eighteenth annual ACM symposium on Theory of computing – STOC '86_. p. 136. 
#'   \doi{10.1145/12130.12144}
#' @param mark A logical vector marking two types or modes.
#'   By default "type".
#' @importFrom igraph max_bipartite_match
#' @examples 
#' autographr(to_matching(ison_southern_women), "hierarchy")
#' @export
to_matching <- function(object, mark = "type") UseMethod("to_matching")

#' @export
to_matching.igraph <- function(object, mark = "type"){
  if(length(unique(node_attribute(object, mark)))>2)
    stop("This function currently only works with binary attributes.")
  el <- igraph::max_bipartite_match(object, 
                 types = node_attribute(object, mark))$matching
  el <- data.frame(from = names(el), to = el)
  out <- suppressWarnings(as_igraph(el, twomode = TRUE))
  out <- igraph::delete_vertices(out, "NA")
  out <- to_twomode(out, node_attribute(object, mark))
  out
}

#' @export
to_matching.tbl_graph <- function(object, mark = "type"){
  as_tidygraph(to_matching(as_igraph(object), mark))
}

#' @export
to_matching.network <- function(object, mark = "type"){
  as_network(to_matching(as_igraph(object), mark))
}

#' @export
to_matching.data.frame <- function(object, mark = "type"){
  as_edgelist(to_matching(as_igraph(object), mark))
}

#' @export
to_matching.matrix <- function(object, mark = "type"){
  as_matrix(to_matching(as_igraph(object), mark))
}

#' @describeIn transform Returns the complement of a network
#'   where only ties _not_ present in the original network
#'   are included in the new network.
#' @importFrom igraph complementer
#' @examples 
#' autographr(to_anti(ison_southern_women), "hierarchy")
#' @export
to_anti <- function(object) UseMethod("to_anti")

#' @export
to_anti.matrix <- function(object){
  matrix(1, nrow(object), ncol(object)) - object
}

#' @export
to_anti.data.frame <- function(object){
  as_edgelist.matrix(to_anti.matrix(as_matrix(object)))
}

#' @export
to_anti.igraph <- function(object){
  if(is_twomode(object)){
    as_igraph(to_anti.matrix(as_matrix(object)))
  } else {
    igraph::complementer(as_igraph(object), 
                         loops = is_complex(object))
  }
}

#' @export
to_anti.tbl_graph <- function(object){
  if(is_twomode(object)){
    as_tidygraph(to_anti.matrix(as_matrix(object)))
  } else {
    as_tidygraph(igraph::complementer(as_igraph(object), 
                         loops = is_complex(object)))
  }
}

#' @export
to_anti.network <- function(object){
  as_network(to_anti(as_igraph(object)))
}

# Splitting and joining ####
#' Tools for splitting and joining networks, graphs, and matrices
#' 
#' @description
#'   These functions offer tools for splitting migraph-consistent objects
#'   (matrices, igraph, tidygraph, or network objects).
#'   Splitting means that the returned object will be a list of objects.
#'   Joining expects a list of objects and returns a network object.
#' @name split
#' @family manipulations
#' @inheritParams reformat
NULL

#' @describeIn split Returns a list of ego (or focal)
#'   networks.
#' @param max_dist The maximum breadth of the neighbourhood.
#'   By default 1.
#' @param min_dist The minimum breadth of the neighbourhood.
#'   By default 0. 
#'   Increasing this to 1 excludes the ego,
#'   and 2 excludes ego's direct alters.
#' @importFrom igraph make_ego_graph
#' @examples 
#' autographs(to_egos(ison_adolescents))
#' autographs(to_egos(ison_adolescents,2))
#' @export
to_egos <- function(object, 
                    max_dist = 1, 
                    min_dist = 0) UseMethod("to_egos")

#' @export
to_egos.igraph <- function(object, 
                           max_dist = 1, 
                           min_dist = 0){
  if(is_twomode(object)) max_dist <- max_dist*2
  out <- igraph::make_ego_graph(object,
                                order = max_dist,
                                mindist = min_dist)
  if(is_labelled(object)) 
    names(out) <- node_names(object)
  out
}

#' @export
to_egos.tbl_graph <- function(object, 
                           max_dist = 1, 
                           min_dist = 0){
  out <- to_egos(as_igraph(object), 
                       max_dist, 
                       min_dist)
  lapply(out, function(x) as_tidygraph(x))
}

#' @export
to_egos.network <- function(object, 
                              max_dist = 1, 
                              min_dist = 0){
  out <- to_egos(as_igraph(object), 
                       max_dist, 
                       min_dist)
  lapply(out, function(x) as_network(x))
}

#' @export
to_egos.matrix <- function(object, 
                              max_dist = 1, 
                              min_dist = 0){
  out <- to_egos(as_igraph(object), 
                       max_dist, 
                       min_dist)
  lapply(out, function(x) as_matrix(x))
}

#' @export
to_egos.data.frame <- function(object, 
                              max_dist = 1, 
                              min_dist = 0){
  out <- to_egos(as_igraph(object), 
                       max_dist, 
                       min_dist)
  lapply(out, function(x) as_edgelist(x))
}

#' @describeIn split Returns a list of subgraphs
#'   on some given node attribute.
#' @param attribute A character string indicating the categorical
#'   attribute in a network used to split into subgraphs.
#' @importFrom igraph induced_subgraph
#' @examples
#' ison_adolescents %>%
#'   activate(nodes) %>%
#'   mutate(unicorn = sample(c("yes", "no"), 8,
#'   replace = TRUE)) %>%
#'   to_subgraphs(attribute = "unicorn")
#' @export
to_subgraphs <- function(object, attribute) UseMethod("to_subgraphs")

#' @export
to_subgraphs.igraph <- function(object, attribute){
  types <- unique(node_attribute(object, attribute))
  lapply(types, function(x) igraph::induced_subgraph(object, 
                              node_attribute(object, attribute) == x))
}

#' @export
to_subgraphs.tbl_graph <- function(object, attribute){
  lapply(to_subgraphs(as_igraph(object), attribute), as_tidygraph)
}

#' @export
to_subgraphs.network <- function(object, attribute){
  lapply(to_subgraphs(as_igraph(object), attribute), as_network)
}

#' @describeIn split Returns a list of the components
#'   in a network.
#' @examples 
#' to_components(ison_marvel_relationships)
#' @export
to_components <- function(object) UseMethod("to_components")

#' @importFrom igraph decompose
#' @export
to_components.igraph <- function(object){
  igraph::decompose(object)
}

#' @export
to_components.tbl_graph <- function(object){
  out <- to_components.igraph(as_igraph(object))
  lapply(out, function(x) as_tidygraph(x))
}

#' @export
to_components.network <- function(object){
  out <- to_components.igraph(as_igraph(object))
  lapply(out, function(x) as_network(x))
}

#' @export
to_components.matrix <- function(object){
  out <- to_components.igraph(as_igraph(object))
  lapply(out, function(x) as_matrix(x))
}

#' @export
to_components.data.frame <- function(object){
  out <- to_components.igraph(as_igraph(object))
  lapply(out, function(x) as_edgelist(x))
}

#' @describeIn split Returns a network
#'   with some discrete observations over time
#'   into a list of those observations.
#' @param attribute Character string indicating the date
#'   attribute in a network used to split into subgraphs.
#' @param panels Would you like to select certain waves?
#'   NULL by default.
#'   That is, a list of networks for every available wave is returned.
#'   Users can also list specific waves they want to select.
#' @examples
#' ison_adolescents %>%
#'   activate(edges) %>%
#'   mutate(wave = sample(1995:1998, 10, replace = TRUE)) %>%
#'   to_waves(attribute = "wave")
#' ison_adolescents %>%
#'   activate(edges) %>%
#'   mutate(wave = sample(1995:1998, 10, replace = TRUE)) %>%
#'   to_waves(attribute = "wave", panels = c(1995, 1996))
#' @export
to_waves <- function(.data, attribute = "wave", panels = NULL) UseMethod("to_waves")

#' @importFrom tidygraph to_subgraph as_tbl_graph
#' @export
to_waves.tbl_graph <- function(.data, attribute = "wave", panels = NULL) {
  # Todo: what about node attributes, does it make sense here?
  # igraph::get.vertex.attribute(.data, attribute)

  # Check if tie attribute exists in data
  if (is.null(tie_attribute(.data, attribute))) {
    stop("Declared tie 'attribute' not found in data.")
  }
  # Get all unique names
  l <- as.character(unique(tie_attribute(.data, attribute)))
  # Handle NA, if present
  l <- ifelse(is.na(l), "NA", l)
  # Crete a named list
  out <- vector("list", length(l))
  names(out) <- l
  # Return list of lists of tbl_graphs based on attribute
  for (i in names(out)) {
    out[[i]] <- suppressMessages(tidygraph::to_subgraph(.data,
                                                        get(attribute) == i))
    # Fix issue with to_subgraph returning objects of class list
    out[[i]] <- tidygraph::as_tbl_graph(out[[i]]$subgraph)
  }
  if (!is.null(panels)) {
    out <- out[as.character(panels)]
  }
  out
}

#' @importFrom tidygraph as_tbl_graph
#' @export
to_waves.igraph <- function(.data, attribute = "wave", panels = NULL) {
  .data <- tidygraph::as_tbl_graph(.data) %>% activate(edges)
  to_waves.tbl_graph(.data, attribute, panels)
}

#' @importFrom tidygraph as_tbl_graph
#' @export
to_waves.data.frame <- function(.data, attribute = "wave", panels = NULL) {
  .data <- tidygraph::as_tbl_graph(.data) %>% activate(edges)
  to_waves.tbl_graph(.data, attribute, panels)
}

#' @importFrom tidygraph as_tbl_graph
#' @export
to_waves.network <- function(.data, attribute = "wave", panels = NULL) {
  .data <- tidygraph::as_tbl_graph(.data) %>% activate(edges)
  to_waves.tbl_graph(.data, attribute, panels)
}

#' @importFrom tidygraph as_tbl_graph
#' @export
to_waves.matrix <- function(.data, attribute = "wave", panels = NULL) {
  .data <- tidygraph::as_tbl_graph(.data) %>% activate(edges)
  to_waves.tbl_graph(.data, attribute, panels)
}

#' @describeIn split Returns a list of a network
#'   with some continuous time variable at some time slice(s).
#' @param attributes List indicating two attributes used to slice data.
#' @param slice Character string or character list indicating the date(s)
#'   or integer(s) range used to slice data (e.g slice = c(1:2, 3:4)).
#' @examples
#' ison_adolescents %>%
#'   activate(edges) %>%
#'   mutate(beg = sample(1:3, 10, replace = TRUE),
#'   end = sample(4:6, 10, replace = TRUE)) %>%
#'   to_slices(attributes = c("beg", "end"), slice = c("1:6", "2:5", "3:4"))
#' @export
to_slices <- function(.data, attributes, slice) UseMethod("to_slices")

#' @export
to_slices.tbl_graph <- function(.data, attributes = c("beg", "end"), slice) {
  # Todo: what about node attributes, does it make sense here?
  # igraph::get.vertex.attribute(.data, attribute)

  # Check attribute and slices
  if (length(attributes) != 2) {
    stop("Please declare 2 attributes.")
  }
  if (missing(slice)) {
    stop("Please declare the slices used to split the data.")
  }
  # Check if tie attribute is date or numeric
  if (!inherits(tie_attribute(.data, attributes[1]), "Date") &
      !is.numeric(tie_attribute(.data, attributes[1]))) {
    stop("Please declare either a date or an interger as a tie 'attribute'.")
  }
  # Check slices are correctly declared
  if (any(!grepl("\\:", slice))) {
    stop("Please declare how to slice as a list chracter
         using a range of values (e.g. slice = c('1:2', '3:4')).")
  }
  # Check if date or numeric
  if (!inherits(tie_attribute(.data, attributes[1]), "Date") &
      !is.numeric(tie_attribute(.data, attributes[1]))) {
    stop("Please declare either a date or an interger as a tie 'attribute'.")
  }
  # Create an empty list
  out <- vector("list", length(slice))
  # Check if dates or numeric
  if (inherits(tie_attribute(.data, attributes[1]), "Date")) {
    # Slice into lists of lists
    for (i in seq_len(length(slice))) {
      slice1 <- as.Date(strsplit(slice[i], "\\:")[[1]][1])
      slice2 <- as.Date(strsplit(slice[i], "\\:")[[1]][2])
      out[[i]] <- suppressMessages(
        tidygraph::to_subgraph(.data, get(attributes[1]) >= slice1 &
                                 get(attributes[2]) <= slice2)
      )
      # Fix issue with to_subgraph returning objects of class list
      out[[i]] <- tidygraph::as_tbl_graph(out[[i]]$subgraph)
    }
  } else {
    # Slice into lists of lists
    for (i in seq_len(length(slice))) {
      slice1 <- as.numeric(strsplit(slice[i], "\\:")[[1]][1])
      slice2 <- as.numeric(strsplit(slice[i], "\\:")[[1]][2])
      out[[i]] <- suppressMessages(
        tidygraph::to_subgraph(.data, get(attributes[1]) >= slice1 &
                                 get(attributes[2]) <= slice2)
      )
      # Fix issue with to_subgraph returning objects of class list
      out[[i]] <- tidygraph::as_tbl_graph(out[[i]]$subgraph)
    }
  }
  names(out) <- slice
  out
}

#' @importFrom tidygraph as_tbl_graph
#' @export
to_slices.igraph <- function(.data, attributes, slice) {
  .data <- tidygraph::as_tbl_graph(.data) %>% activate(edges)
  to_slices.tbl_graph(.data, attributes, slice)
}

#' @importFrom tidygraph as_tbl_graph
#' @export
to_slices.data.frame <- function(.data, attributes, slice) {
  .data <- tidygraph::as_tbl_graph(.data) %>% activate(edges)
  to_slices.tbl_graph(.data, attributes, slice)
}

#' @importFrom tidygraph as_tbl_graph
#' @export
to_slices.network <- function(.data, attributes, slice) {
  .data <- tidygraph::as_tbl_graph(.data) %>% activate(edges)
  to_slices.tbl_graph(.data, attributes, slice)
}

#' @importFrom tidygraph as_tbl_graph
#' @export
to_slices.matrix <- function(.data, attributes, slice) {
  .data <- tidygraph::as_tbl_graph(.data) %>% activate(edges)
  to_slices.tbl_graph(.data, attributes, slice)
}

#' @describeIn split Removes network vertices that have no edges
#'   in lists of lists.
#' @param tlist A migraph-compatible network listed according to
#'   a time attribute, waves, or slices.
#' @importFrom tidygraph node_is_isolated
#' @importFrom dplyr filter
#' @examples
#' ison_adolescents %>%
#'   activate(edges) %>%
#'   mutate(wave = sample(1995:1998, 10, replace = TRUE)) %>%
#'   to_waves(attribute = "wave") %>%
#'   to_no_isolates()
#' ison_adolescents %>%
#'   activate(edges) %>%
#'   mutate(beg = sample(1:3, 10, replace = TRUE),
#'   end = sample(4:6, 10, replace = TRUE)) %>%
#'   to_slices(attributes = c("beg", "end"), slice = c("1:6", "2:5", "3:4")) %>%
#'   to_no_isolates()
#' @export
to_no_isolates <- function(.data) {
  # Check if object is a list of lists
  if (!is.list(.data)) {
    stop("Please declare a migraph-compatible network listed according
         to an attribute, waves, or slices.")
  }
  # Remove isolates at each step
  # Delete edges not present vertices
  lapply(.data, function(x) {
    x %>% activate(nodes) %>% dplyr::filter(!tidygraph::node_is_isolated())
  })
} 

#' @describeIn split Returns a single network object
#'  from a list of subgraphs.
#' @importFrom igraph graph_from_data_frame as_data_frame set.vertex.attribute
#' @examples
#' ison_adolescents %>%
#'   activate(nodes) %>%
#'   mutate(unicorn = sample(c("yes", "no"), 8,
#'   replace = TRUE)) %>%
#'   to_subgraphs(attribute = "unicorn") %>%
#'   from_subgraphs()
#' @export
from_subgraphs <- function(.data) {
  if (!is.list(.data)) {
    stop("Please declare a list of subgraphs. ")
  }
  ann <- lapply(.data, as_igraph)
  edges <- igraph::as_data_frame(ann[[1]], what = "edges")
  for (i in seq_along(ann)[-1]) {
    edges <- rbind(edges, igraph::as_data_frame(ann[[i]], what = "edges"))
  }
  vertex <- igraph::as_data_frame(ann[[1]], what = "vertices")
  for (i in seq_along(ann)[-1]) {
    vertex <- rbind(vertex, igraph::as_data_frame(ann[[i]], what = "vertices"))
  }
  out <- igraph::graph_from_data_frame(edges)
  for (i in names(vertex)) {
    out <- suppressWarnings(igraph::set.vertex.attribute(out, name = i,
                                                         value = unlist(vertex[i])))
  }
  out
}

#' @describeIn split Returns a single network object
#'  from a list of egos.
#' @importFrom igraph graph_from_data_frame as_data_frame
#' @importFrom dplyr distinct
#' @examples
#' ison_adolescents %>%
#'   activate(edges) %>%
#'   to_egos() %>%
#'   from_egos()
#' @export
from_egos <- function(.data) {
  if (!is.list(.data)) {
    stop("Please declare a list of egos.")
  }
  ann <- lapply(.data, as_igraph)
  out <- igraph::as_data_frame(ann[[1]])
  for (i in seq_along(ann)[-1]){
    out <- rbind(out, igraph::as_data_frame(ann[[i]]))
  }
  igraph::graph_from_data_frame(dplyr::distinct(out))
}

#' @describeIn split Returns a single network object
#'  from a list of waves.
#' @importFrom igraph graph_from_data_frame as_data_frame
#' @examples
#' ison_adolescents %>%
#'   activate(edges) %>%
#'   mutate(wave = sample(1:4, 10, replace = TRUE)) %>%
#'   to_waves(attribute = "wave") %>%
#'   from_waves()
#' @export
from_waves <- function(.data, directed = FALSE) {
  if (!is.list(.data)) {
    stop("Please declare a list of waves.")
  }
  ann <- lapply(.data, as_igraph)
  out <- igraph::as_data_frame(ann[[1]])
  for (i in seq_along(ann)[-1]){
    out <- rbind(out, igraph::as_data_frame(ann[[i]]))
  }
  igraph::graph_from_data_frame(out)
}

#' @describeIn split Returns a single network object
#'  from a list of slices.
#' @param remove.duplicates Should duplicates be removed?
#' By default FALSE.
#' If TRUE, duplicated edges are removed.
#' @importFrom igraph graph_from_data_frame as_data_frame
#' @importFrom dplyr distinct
#' @examples
#' ison_adolescents %>%
#'   activate(edges) %>%
#'   mutate(beg = sample(1:3, 10, replace = TRUE),
#'   end = sample(4:6, 10, replace = TRUE)) %>%
#'   to_slices(attributes = c("beg", "end"), slice = c("1:6", "2:5", "3:4")) %>%
#'   from_slices()
#' @export
from_slices <- function(.data, remove.duplicates = FALSE) {
  if (!is.list(.data)) {
    stop("Please declare a list of slices.")
  }
  ann <- lapply(.data, as_igraph)
  out <- igraph::as_data_frame(ann[[1]])
  for (i in seq_along(ann)[-1]){
    out <- rbind(out, igraph::as_data_frame(ann[[i]]))
  }
  if (isTRUE(remove.duplicates)) {
    out <- dplyr::distinct(out)
  }
  igraph::graph_from_data_frame(out)
}

# Missing ####

#' Tools for imputing missing tie data
#' 
#' These functions offer tools for imputing missing tie data.
#' @name na
#' @family manipulations
NULL

#' @describeIn na Impute missing tie data as null,
#'   the modal value in sparse social networks.
#' @examples 
#' missTest <- ison_adolescents %>% 
#'    add_tie_attribute("weight", c(1,NA,NA,1,1,1,NA,NA,1,1)) %>% 
#'    as_matrix
#' missTest
#' na_to_null(missTest)
#' na_to_mean(missTest)
#' @export
na_to_null <- function(object) UseMethod("na_to_null")

#' @export
na_to_null.tbl_graph <- function(object){
  object %>% activate(edges) %>% 
    dplyr::filter(!is.na(weight)) %>% 
    activate(nodes)
}

#' @export
na_to_null.igraph <- function(object){
  as_igraph(na_to_null(as_tidygraph(object)))
}

#' @export
na_to_null.network <- function(object){
  as_network(na_to_null(as_tidygraph(object)))
}

#' @export
na_to_null.matrix <- function(object){
  object[is.na(object)] <- 0
  object
}

#' @export
na_to_null.data.frame <- function(object){
  object[is.na(object[,3]),3] <- 0
  object
}

#' @describeIn na Impute missing tie data as
#'   the mean value in the network.
#' @export
na_to_mean <- function(object) UseMethod("na_to_mean")

#' @export
na_to_mean.tbl_graph <- function(object){
  if(is_weighted(object) & any(tie_weights(object)>1)){
    object %>% activate(edges) %>% 
      mutate(weight = ifelse(is.na(weight), 
                             mean(weight, na.rm = TRUE), 
                             weight)) %>% 
      activate(nodes)
  } else {
    prob <- sum(tie_attribute(object, "weight"), na.rm = TRUE)/
      sum(!is.na(tie_attribute(object, "weight")))
    object %>% activate(edges) %>% 
      mutate(weight = vapply(seq_len(weight),
                              function(x) ifelse(is.na(x),
                                             rbinom(1,1,prob),
                                             x),
                            numeric(1))) %>% 
      activate(nodes)
  }
}

#' @export
na_to_mean.igraph <- function(object){
  as_igraph(na_to_mean(as_tidygraph(object)))
}

#' @export
na_to_mean.network <- function(object){
  as_network(na_to_mean(as_tidygraph(object)))
}

#' @export
na_to_mean.matrix <- function(object){
  if(any(object>1, na.rm = TRUE)){
    object[is.na(object)] <- mean(object, na.rm = TRUE)
    object
  } else {
    object[is.na(object)] <- rbinom(sum(is.na(object)), 
                                    1, mean(object, na.rm = TRUE))
    object
  }
}

