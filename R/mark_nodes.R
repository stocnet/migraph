#' Marking nodes based on their properties
#' 
#' @description 
#'   These functions return logical vectors the length of the 
#'   nodes in a network identifying which hold certain properties or positions in the network.
#'   
#'   - `node_is_cutpoint()` marks nodes that cut or act as articulation points in a network,
#'   increasing the number of connected components when removed.
#'   - `node_is_isolate()` marks nodes that are isolates,
#'   with neither incoming nor outgoing ties.
#'   - `node_is_core()` marks nodes that are members of the network's core.
#'   - `node_is_fold()` marks nodes that are in a structural fold between two or more
#'   triangles that are only connected by that node.
#'   - `node_is_mentor()` marks a proportion of high indegree nodes as 'mentors' (see details)
#'   - `node_is_infected()` and `node_is_exposed()` marks nodes that are infected
#'   by a particular time point or exposed to a given (other) mark
#'   - `node_is_random()` marks one or more nodes at random.
#'   - `node_is_max()` and `node_is_min()` are more generally useful
#'   for converting the results from some node measure into a mark-class object.
#'   They can be particularly useful for highlighting which node or nodes
#'   are key because they minimise or, more often, maximise some measure.
#' @inheritParams cohesion
#' @inheritParams node_diffusion
#' @family marks
#' @name mark_nodes
NULL

#' @rdname mark_nodes
#' @examples 
#' node_is_isolate(ison_brandes)
#' @export
node_is_isolate <- function(.data){
  mat <- manynet::as_matrix(.data)
  if(manynet::is_twomode(.data)){
    out <- c(rowSums(mat)==0, colSums(mat)==0)
  } else {
    out <- rowSums(mat)==0 & colSums(mat)==0
  }
  names(out) <- manynet::node_names(.data)
  make_node_mark(out, .data)
}

#' @rdname mark_nodes
#' @importFrom igraph articulation_points
#' @examples 
#' node_is_cutpoint(ison_brandes)
#' @export
node_is_cutpoint <- function(.data){
  if(manynet::is_labelled(.data)){
    out <- manynet::node_names(.data) %in% 
      attr(igraph::articulation_points(manynet::as_igraph(.data)), 
           "names")
    names(out) <- manynet::node_names(.data)
  } else {
    out <- 1:manynet::network_nodes(.data) %in% 
      igraph::articulation_points(manynet::as_igraph(.data))
  }
  make_node_mark(out, .data)
}

#' @rdname mark_nodes
#' @examples 
#' node_is_core(ison_brandes)
#' @export
node_is_core <- function(.data){
  out <- node_core(.data)==1
  make_node_mark(out, .data)
}

#' @rdname mark_nodes
#' @examples
#' node_is_fold(create_explicit(A-B, B-C, A-C, C-D, C-E, D-E))
#' @export
node_is_fold <- function(.data){
  mult_tri <- igraph::count_triangles(.data)>1
  tris <- igraph::triangles(.data)
  tris <- matrix(tris, length(tris)/3, 3, byrow = TRUE)
  out <- vapply(seq_along(mult_tri), function(x){
    if(!mult_tri[x]) FALSE else {
     tri_neigh <- unique(c(tris[apply(tris, 1, function(r) any(x %in% r)),] ))
     tri_neigh <- tri_neigh[tri_neigh != x]
     all(rowSums(igraph::distances(.data, tri_neigh, tri_neigh)==2)>=2)
    }
  }, FUN.VALUE = logical(1) )
  make_node_mark(out, .data)
}

#' @rdname mark_nodes
#' @param elites The proportion of nodes to be selected as mentors.
#'   By default this is set at 0.1.
#'   This means that the top 10% of nodes in terms of degree,
#'   or those equal to the highest rank degree in the network,
#'   whichever is the higher, will be used to select the mentors.
#'   
#'   Note that if nodes are equidistant from two mentors,
#'   they will choose one at random.
#'   If a node is without a path to a mentor,
#'   for example because they are an isolate,
#'   a tie to themselves (a loop) will be created instead.
#'   Note that this is a different default behaviour than that
#'   described in Valente and Davis (1999).
#' @references
#' Valente, Thomas, and Rebecca Davis. 1999.
#' "Accelerating the Diffusion of Innovations Using Opinion Leaders",
#' _Annals of the American Academy of Political and Social Science_ 566: 56-67.
#' @export
node_is_mentor <- function(.data, elites = 0.1){
  indegs <- colSums(manynet::as_matrix(.data)) # get rank order of indegrees
  out <- indegs == max(indegs)
  if(sum(out) < length(indegs)*elites){
    out <- indegs %in% unique(sort(indegs, decreasing=TRUE)[seq_len(length(indegs)*elites)])
  }
  make_node_mark(out, .data)
}

#' @rdname mark_nodes 
#' @examples
#'   # To mark nodes that are latent by a particular time point
#'   node_is_latent(play_diffusion(create_tree(6), latency = 1), time = 1)
#' @export
node_is_latent <- function(diff_model, time = 0){
  event <- nodes <- NULL
  latent <- summary(diff_model) |> 
    dplyr::filter(t <= time & event %in% c("E","I")) |> 
    dplyr::filter(!duplicated(nodes, fromLast = TRUE)) |> 
    dplyr::filter(event == "E") |> 
    dplyr::select(nodes)
  net <- attr(diff_model, "network")
  if(!manynet::is_labelled(net))
    latent <- dplyr::arrange(latent, nodes) else if (is.numeric(latent$nodes))
      latent$nodes <- manynet::node_names(net)[latent$nodes]
  if(manynet::is_labelled(net)){
    nnames <- manynet::node_names(net)
    out <- stats::setNames(nnames %in% latent$nodes, nnames)
  } else {
    seq_len(manynet::network_nodes(net))
    out <- seq_len(manynet::network_nodes(net)) %in% latent$nodes
  }
  make_node_mark(out, net)
}

#' @rdname mark_nodes 
#' @examples
#'   # To mark nodes that are infected by a particular time point
#'   node_is_infected(play_diffusion(create_tree(6)), time = 1)
#' @export
node_is_infected <- function(diff_model, time = 0){
  event <- nodes <- NULL
  infected <- summary(diff_model) |> 
    dplyr::filter(t <= time & event %in% c("I","R")) |> 
    dplyr::filter(!duplicated(nodes, fromLast = TRUE)) |> 
    dplyr::filter(event == "I") |> 
    dplyr::select(nodes)
  net <- attr(diff_model, "network")
  if(manynet::is_labelled(net)){
    nnames <- manynet::node_names(net)
    out <- stats::setNames(nnames %in% infected$nodes, nnames)
  } else {
    seq_len(manynet::network_nodes(net))
    out <- seq_len(manynet::network_nodes(net)) %in% infected$nodes
  }
  make_node_mark(out, net)
}

#' @rdname mark_nodes 
#' @examples
#'   # To mark nodes that are recovered by a particular time point
#'   node_is_recovered(play_diffusion(create_tree(6), recovery = 0.5), time = 3)
#' @export
node_is_recovered <- function(diff_model, time = 0){
  event <- nodes <- NULL
  recovered <- summary(diff_model) |> 
    dplyr::filter(t <= time & event %in% c("R","S")) |> 
    dplyr::filter(!duplicated(nodes, fromLast = TRUE)) |> 
    dplyr::filter(event == "R") |> 
    dplyr::select(nodes)
  net <- attr(diff_model, "network")
  if(!manynet::is_labelled(net))
    recovered <- dplyr::arrange(recovered, nodes) else if (is.numeric(recovered$nodes))
      recovered$nodes <- manynet::node_names(net)[recovered$nodes]
  if(manynet::is_labelled(net)){
    nnames <- manynet::node_names(net)
    out <- stats::setNames(nnames %in% recovered$nodes, nnames)
  } else {
    seq_len(manynet::network_nodes(net))
    out <- seq_len(manynet::network_nodes(net)) %in% recovered$nodes
  }
  make_node_mark(out, net)
}

#' @rdname mark_nodes 
#' @param mark A valid 'node_mark' object or
#'   logical vector (TRUE/FALSE) of length equal to 
#'   the number of nodes in the network.
#' @section Exposed:
#'   `node_is_exposed()` is similar to `node_exposure()`,
#'   but returns a mark (TRUE/FALSE) vector indicating which nodes
#'   are currently exposed to the diffusion content.
#'   This diffusion content can be expressed in the 'mark' argument.
#'   If no 'mark' argument is provided,
#'   and '.data' is a diff_model object,
#'   then the function will return nodes exposure to the seed nodes
#'   in that diffusion.
#' @param mark vector denoting which nodes are infected
#' @examples
#'   # To mark which nodes are currently exposed
#'   (expos <- node_is_exposed(manynet::create_tree(14), mark = c(1,3)))
#'   which(expos)
#' @export
node_is_exposed <- function(.data, mark){
  event <- nodes <- NULL
  if(missing(mark) && inherits(.data, "diff_model")){
    mark <- summary(.data) |> 
      dplyr::filter(t == 0 & event == "I") |> 
      dplyr::select(nodes) |> unlist()
    .data <- attr(.data, "network")
  }
  if(is.logical(mark)) mark <- which(mark)
  out <- rep(F, manynet::network_nodes(.data))
  out[unique(setdiff(unlist(igraph::neighborhood(.data, nodes = mark)),
                     mark))] <- TRUE
  make_node_mark(out, .data)
}

#' @rdname mark_nodes
#' @param size The number of nodes to select (as TRUE).
#' @examples 
#' node_is_random(ison_brandes, 2)
#' @export
node_is_random <- function(.data, size = 1){
  n <- manynet::network_nodes(.data)
  out <- rep(FALSE, n)
  out[sample.int(n, size)] <- TRUE
  make_node_mark(out, .data)
}

#' @rdname mark_nodes
#' @param node_measure An object created by a `node_` measure.
#' @param ranks The number of ranks of max or min to return.
#'   For example, `ranks = 3` will return TRUE for nodes with
#'   scores equal to any of the top (or, for `node_is_min()`, bottom)
#'   three scores.
#'   By default, `ranks = 1`.
#' @examples 
#' node_is_max(node_degree(ison_brandes))
#' @export
node_is_max <- function(node_measure, ranks = 1){
  if(!inherits(node_measure, "node_measure"))
    stop("This function expects an object of class `node_measure`")
  if(any(attr(node_measure, "mode"))){
    mode1 <- as.numeric(node_measure)[!as.logical(attr(node_measure, "mode"))]
    max1 <- mode1[order(mode1, decreasing = TRUE)[1:ranks]]
    mode2 <- as.numeric(node_measure)[as.logical(attr(node_measure, "mode"))]
    max2 <- mode2[order(mode2, decreasing = TRUE)[1:ranks]]
    out <- ((as.numeric(node_measure) %in% max1 & 
      !as.logical(attr(node_measure, "mode"))) | 
      (as.numeric(node_measure) %in% max2 & 
      as.logical(attr(node_measure, "mode"))))
    attr(out, "mode") <- attr(node_measure, "mode")
  } else {
    out <- node_measure %in% node_measure[order(node_measure,
                                   decreasing = TRUE)[1:ranks]]
  }
  names(out) <- attr(node_measure, "names")
  class(out) <- c("node_mark", class(out))
  out
}

#' @rdname mark_nodes
#' @examples 
#' node_is_min(node_degree(ison_brandes))
#' @export
node_is_min <- function(node_measure, ranks = 1){
  if(!inherits(node_measure, "node_measure"))
    stop("This function expects an object of class `node_measure`")
  if(any(attr(node_measure, "mode"))){
    mode1 <- as.numeric(node_measure)[!as.logical(attr(node_measure, "mode"))]
    max1 <- mode1[order(mode1, decreasing = FALSE)[1:ranks]]
    mode2 <- as.numeric(node_measure)[as.logical(attr(node_measure, "mode"))]
    max2 <- mode2[order(mode2, decreasing = FALSE)[1:ranks]]
    out <- ((as.numeric(node_measure) %in% max1 & 
               !as.logical(attr(node_measure, "mode"))) | 
              (as.numeric(node_measure) %in% max2 & 
                 as.logical(attr(node_measure, "mode"))))
    attr(out, "mode") <- attr(node_measure, "mode")
  } else {
    out <- node_measure %in% node_measure[order(node_measure,
                                                decreasing = FALSE)[1:ranks]]
  }
  names(out) <- attr(node_measure, "names")
  class(out) <- c("node_mark", class(out))
  out
}