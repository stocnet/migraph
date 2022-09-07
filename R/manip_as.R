#' Coercion between migraph-compatible object classes 
#' 
#' @description
#' The `as_` functions in `{migraph}` coerce objects
#' between several common classes of social network objects.
#' These include:
#' - edgelists, as data frames or tibbles
#' - adjacency (one-mode/unipartite) and incidence (two-mode/bipartite) matrices
#' - `{igraph}` `graph` objects
#' - `{tidygraph}` `tbl_graph` objects
#' - `{network}` `network` objects
#' 
#' An effort is made for all of these coercion routines to be as lossless
#' as possible, though some object classes are better at retaining certain
#' kinds of information than others.
#' Note also that there are some reserved column names in one or more
#' object classes, which could otherwise lead to some unexpected results.
#' @name as
#' @family manipulations
#' @inheritParams is
#' @param twomode Logical option used to override heuristics for 
#'   distinguishing incidence (two-mode/bipartite) from 
#'   adjacency (one-mode/unipartite) networks. 
#'   By default FALSE.
#' @details 
#' Edgelists are expected to be held in data.frame or tibble class objects.
#' The first two columns of such an object are expected to be the
#' senders and receivers of a tie, respectively, and are typically
#' named "from" and "to" (even in the case of an undirected network).
#' These columns can contain integers to identify nodes or character
#' strings/factors if the network is labelled.
#' If the sets of senders and receivers overlap, a one-mode network
#' is inferred. If the sets contain no overlap, a two-mode network
#' is inferred.
#' If a third, numeric column is present, a weighted network
#' will be created.
#' 
#' Matrices can be either adjacency (one-mode) or incidence (two-mode) matrices.
#' Incidence matrices are typically inferred from unequal dimensions,
#' but since in rare cases a matrix with equal dimensions may still
#' be an incidence matrix, an additional argument `twomode` can be
#' specified to override this heuristic.
#' 
#' This information is usually already embedded in `{igraph}`, 
#' `{tidygraph}`, and `{network}` objects.
#' @importFrom tidygraph as_tbl_graph is.tbl_graph
#' @importFrom network is.network as.network
#' @importFrom network as.matrix.network.incidence as.matrix.network.adjacency
#' @examples
#' test <- data.frame(from = c("A","B","B","C","C"),
#'                    to = c("I","G","I","G","H"))
#' as_edgelist(test)
#' as_matrix(test)
#' as_igraph(test)
#' as_tidygraph(test)
#' as_network(test)
#' as_graphAM(test)
#' @return
#' The currently implemented coercions or translations are:
#' 
#' |  to/from  | edgelists  | matrices  |igraph  |tidygraph  |network  | siena | goldfish
#' | ------------- |:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|
#' | edgelists (data frames)  | X | X | X | X | X | X | X |
#' | matrices                 | X | X | X | X | X | X | X |
#' | igraph                   | X | X | X | X | X | X | X |
#' | tidygraph                | X | X | X | X | X | X | X |
#' | network                  | X | X | X | X | X | X | X |
#' | graphAM                  | X | X | X | X | X | X | X |
NULL

# Edgelists ####

#' @rdname as
#' @importFrom igraph as_edgelist
#' @importFrom dplyr arrange
#' @export
as_edgelist <- function(object,
                        twomode = FALSE) UseMethod("as_edgelist")

#' @export
as_edgelist.igraph <- function(object,
                               twomode = FALSE){
  out <- igraph::get.data.frame(object)
  dplyr::as_tibble(out)
}

#' @export
as_edgelist.tbl_graph <- function(object,
                                  twomode = FALSE){
  out <- igraph::get.data.frame(object)
  dplyr::as_tibble(out)
}

#' @export
as_edgelist.network <- function(object,
                                twomode = FALSE){
  out <- sna::as.edgelist.sna(object)
  edges <- as.data.frame(out)
  if (is_twomode(object)) {
    edges <- edges[((nrow(edges)/2) + 1):nrow(edges),]
  }
  from <- to <- NULL
  names(edges) <- c("from", "to", "weight")
  # Handle node names
  if (is_labelled(object)) {
    names <- attr(out, "vnames")
    edges[,1] <- names[edges[,1]]
    edges[,2] <- names[edges[,2]]
  }
  # Handle edge weights
  if (is_weighted(object)) {
    edges[,3] <- network::get.edge.attribute(object, "weight")
  }
  # Remove weight column if only unity weights.
  if (all(edges$weight == 1)) edges <- edges[, -3]
  dplyr::arrange(dplyr::as_tibble(edges), from, to)
}

#' @export
as_edgelist.matrix <- function(object,
                               twomode = FALSE){
  as_edgelist(as_igraph(object,
                               twomode = FALSE))
}

#' @export
as_edgelist.data.frame <- function(object,
                                   twomode = FALSE){
  if(ncol(object) == 2 && any(names(object) != c("from", "to"))){
    names(object) <- c("from", "to")
    object
  } else if(ncol(object) == 3 && 
            (any(names(object) != c("from", "to", "weight")) | 
            any(names(object) != c("from", "to", "sign")))){
    names(object) <- c("from", "to", "weight")
    object
  } else object
}

#' @export
as_edgelist.network.goldfish <- function(object,
                                         twomode = FALSE) {
  as_matrix(as_igraph(object, twomode = twomode))
}

#' @export
as_edgelist.siena <- function(object,
                              twomode = NULL) {
  as_edgelist(as_igraph(object, twomode = twomode))
}

# Matrices ####

#' @rdname as
#' @export
as_matrix <- function(object,
                      twomode = NULL) UseMethod("as_matrix")

#' @export
as_matrix.data.frame <- function(object,
                                 twomode = NULL){
  if ("tbl_df" %in% class(object)) object <- as.data.frame(object)
  
  if (ncol(object) == 2 | !is_weighted(object)) {
    object <- data.frame(object) # in case it's a tibble
    object <- as.data.frame(table(c(object[,1]),
                                  c(object[,2])))
    names(object) <- c("from","to","weight")
  }
  if (ncol(object) == 3) {
    # Adds a third (weight) column to a two-column edgelist
    # object <- object[order(object[,1], object[,2]),]
    nodes1 <- as.character(unique(object[,1]))
    nodes1 <- sort(nodes1)
    nodes2 <- as.character(unique(object[,2]))
    nodes2 <- sort(nodes2)
    if(length(intersect(nodes1, nodes2)) > 0 &
       !setequal(nodes1, nodes2))
      nodes1 <- nodes2 <- sort(unique(c(nodes1,nodes2)))
    if (nrow(object) != length(nodes1)*length(nodes2)) {
      allcombs <- expand.grid(nodes1, nodes2, stringsAsFactors = FALSE)
      allcombs <- subset(allcombs, !duplicated(allcombs))
      names(allcombs) <- c("from","to")
      object <- merge(allcombs, object, all.x = TRUE)
      object <- object[order(object[,2], object[,1]),]
      object[is.na(object)] <- 0
    }
    object <- dplyr::arrange(object, 
                             as.character(.data$to), 
                             as.character(.data$from))
    out <- structure(as.numeric(object[,3]),
                     .Dim = c(as.integer(length(nodes1)),
                              as.integer(length(nodes2))),
                     .Dimnames = list(nodes1, nodes2))
  }
  out
}

#' @export
as_matrix.matrix <- function(object,
                             twomode = NULL) {
  object
}

#' @export
as_matrix.igraph <- function(object,
                             twomode = NULL) {
  if ((!is.null(twomode) && twomode) | (is.null(twomode) & is_twomode(object))) {
    if (is_weighted(object) | is_signed(object)) {
      mat <- igraph::as_incidence_matrix(object, sparse = FALSE,
                                         attr = igraph::edge_attr_names(object)[[1]])
    } else {
      mat <- igraph::as_incidence_matrix(object, sparse = FALSE,
                                         attr = NULL)
    }
  } else {
    if (is_weighted(object) | is_signed(object)) {
      mat <- igraph::as_adjacency_matrix(object, sparse = FALSE,
                                         attr = igraph::edge_attr_names(object)[[1]])
    } else {
      mat <- igraph::as_adjacency_matrix(object, sparse = FALSE,
                                         attr = NULL)
    }
  }
  mat
}

#' @export
as_matrix.tbl_graph <- function(object,
                                twomode = NULL) {
  as_matrix(as_igraph(object), twomode = twomode)
}

#' @export
as_matrix.network <- function(object,
                              twomode = NULL) {
  if (network::is.bipartite(object)) {
    if ("weight" %in% network::list.edge.attributes(object)) {
      network::as.matrix.network(object,
                                 attrname = "weight",
                                 expand.bipartite = FALSE)
      # Note: if expand.bipartite is true it returns the adjacency matrix. If
      # false it returns the incidence matrix that we want. Use
      # to_multilevel(mat) on the resulting matrix to do the conversion if needed.
    } else {
      network::as.matrix.network(object,
                                 expand.bipartite = FALSE)
    }
  } else {
    if ("weight" %in% network::list.edge.attributes(object)) {
      network::as.matrix.network(object, attrname = "weight")
    } else {
      network::as.matrix.network(object)
    }
  }
}

#' @export
as_matrix.network.goldfish <- function(object,
                                       twomode = FALSE) {
  as_matrix(as_igraph(object, twomode = twomode))
}

#' @export
as_matrix.siena <- function(object,
                            twomode = NULL) {
  # Get the dependent network(s) first
  # Identify all dyadic depvars
  dvs <- lapply(object$depvars, function(x) is.matrix(x[,,1]) )
  ddvs <- names(which(dvs == TRUE))
  # Add in first wave of first DV network
  out <- object$depvars[[ddvs[1]]][,,1]
  # Add remaining waves
  for(d in 2:dim(object$depvars[[ddvs[1]]])[3]) {
    out <- object$depvars[[ddvs[1]]][,,d] + out
  }
  # Add other dyadic depvars
  if (length(ddvs) > 1) {
    for (l in 2:length(ddvs)) {
      for (d in seq_len(dim(object$depvars[[ddvs[l]]])[3])) {
        out <- object$depvars[[ddvs[l]]][,,d] + out
      }
    }
  }
  # Add dycCovars
  for (k in seq_len(length(object$dycCovars))) {
    out <- object$dycCovars[[ddvs[k]]] + out
  }
  # Add dyvCovars
  for (k in seq_len(length(object$dyvCovars))) {
    for (d in seq_len(dim(object$dyvCovars[[k]])[3])) {
      out <- object$dyvCovars[[k]][,,d] + out
    }
  }
  out
}

# igraph ####

#' @rdname as
#' @importFrom igraph graph_from_data_frame graph_from_incidence_matrix
#' @importFrom igraph graph_from_adjacency_matrix
#' @export
as_igraph <- function(object,
                      twomode = FALSE) UseMethod("as_igraph")

#' @export
as_igraph.data.frame <- function(object,
                                 twomode = FALSE) {
  if ("tbl_df" %in% class(object)) object <- as.data.frame(object)
  
  # Warn if no column named weight and weight set to true
  if (is_weighted(object) & !("weight" %in% names(object))) {
    names(object)[3] <- "weight"
    # stop("Please rename the weight column of your dataframe to 'weight'")
  }
  graph <- igraph::graph_from_data_frame(object)
  if (length(intersect(c(object[,1]), c(object[,2]))) == 0) {
    igraph::V(graph)$type <- igraph::V(graph)$name %in% object[,2]
  }
  graph
}

#' @export
as_igraph.matrix <- function(object,
                             twomode = FALSE) {
  if (nrow(object) != ncol(object) | twomode) {
    if (!(all(object %in% c(0, 1)))) {
      graph <- igraph::graph_from_incidence_matrix(object, 
                                                   weighted = TRUE, 
                                                   directed = FALSE)
    } else {
      graph <- igraph::graph_from_incidence_matrix(object, 
                                                   directed = FALSE)
    }
  } else {
    if (!(all(object %in% c(0, 1)))) {
      graph <- igraph::graph_from_adjacency_matrix(object, 
                                                   mode = ifelse(all(object == t(object)),
                                                                 "undirected", "directed"),
                                                   weighted = TRUE)
    } else {
      graph <- igraph::graph_from_adjacency_matrix(object, 
                                                   mode = ifelse(all(object == t(object)),
                                                                 "undirected", "directed"))
    }
  }
  graph
}

#' @export
as_igraph.igraph <- function(object,
                             twomode = FALSE) {
  class(object) <- "igraph"
  object
}

#' @export
as_igraph.tbl_graph <- function(object,
                                twomode = FALSE) {
  class(object) <- "igraph"
  object
}

#' @export
as_igraph.network <- function(object, 
                              twomode = FALSE) {
  # Extract node attributes
  attr <- names(object[[3]][[1]])
  # Convert to igraph
  if (network::is.bipartite(object)) {
    if ("weight" %in% network::list.edge.attributes(object)) {
      graph <- sna::as.sociomatrix.sna(object, attrname = "weight")
      graph <- igraph::graph_from_incidence_matrix(graph, weighted = TRUE)
    } else {
      graph <- sna::as.sociomatrix.sna(object)
      graph <- igraph::graph_from_incidence_matrix(graph) 
    }
  } else {
    if ("weight" %in% network::list.edge.attributes(object)) {
      graph <- sna::as.sociomatrix.sna(object, attrname = "weight")
      graph <- igraph::graph_from_adjacency_matrix(graph,
                                                   weighted = TRUE,
                                                   mode = ifelse(object$gal$directed,
                                                                 "directed",
                                                                 "undirected"))
    } else if (length(network::list.edge.attributes(object)) > 1) {
      object$gal$multiple <- FALSE
      graph <- sna::as.sociomatrix.sna(object, attrname = network::list.edge.attributes(object)[1])
      graph <- igraph::graph_from_adjacency_matrix(graph,
                                                   weighted = TRUE,
                                                   mode = ifelse(object$gal$directed,
                                                                 "directed",
                                                                 "undirected"))
    } else {
      graph <- sna::as.sociomatrix.sna(object)
      graph <- igraph::graph_from_adjacency_matrix(graph,
                                                 mode = ifelse(object$gal$directed,
                                                               "directed",
                                                               "undirected"))
    }
  }
  # Add remaining node level attributes
  if (length(attr) > 2) {
    for (a in attr[2:length(attr)]) {
      graph <- add_node_attribute(graph, 
                                   attr_name = a, 
                                   vector = sapply(object[[3]], "[[", a))
    }
  }
  graph
}

#' @export
as_igraph.network.goldfish <- function(object,
                                       twomode = FALSE) {
  
  # orig <- deparse(substitute(object))
  # y <- ls(envir = .GlobalEnv)
  # envir  <- .GlobalEnv
  # 
  # classesToKeep <- c("nodes.goldfish", "network.goldfish")
  # checkClasses <- function(object, classes) vapply(classes, 
  #                                                  function(x) methods::is(object, x), logical(1))
  # ClassFilter <- function(x) any(checkClasses(get(x), classes = classesToKeep))
  # gfobjs <- Filter(ClassFilter, y)
  # classes <- vapply(gfobjs, FUN = function(x) checkClasses(get(x), 
  #                                                          classes = classesToKeep), 
  #                   FUN.VALUE = logical(length(classesToKeep)))
  
  if(sum(object)==0){
    out <- igraph::graph_from_data_frame(d = get(attr(object, "events"))[,2:4],
                                         directed = attr(object, "directed"),
                                         vertices = get(attr(object, "nodes")))
  } else stop("Non-empty starts are not yet supported by this function.")
  out
}

#' @export
as_igraph.siena <- function(object, twomode = NULL) {
  edges <- NULL
  orig <- NULL
  
  ## Helper functions for as_igraph.siena
  .get_rem_time_periods <- function(g, x, name = NULL) {
    for(d in 2:dim(g)[3]){
      x <- join_ties(x, as_igraph(g[,,d]), 
                     attr_name = paste0(name, "_", "t", d))
    }
    x
  }
  
  .get_all_time_periods <- function(g, x, name = NULL) {
    # g is a matrix but x is igraph obj
    for(d in seq_len(dim(g)[3])){
      y <- g[,,d]
      if (isTRUE(is_twomode(y))) {
        # add names for new network
        rownames(y) <- as.character(seq_len(nrow(y)))
        colnames(y) <- as.character(paste0("N", seq_len(ncol(y))))
        # join ties
        if (isTRUE(is_twomode(x))) { # x and y are twomode
          x <- join_ties(x, as_igraph(y), 
                         attr_name = paste0(name, "_", "t", d))
        } else { # x is onemode but y is twomode
          y <- as_edgelist(y)
          y <- y %>%
            dplyr::mutate(weight = 1)
          x <- dplyr::bind_rows(y, as_edgelist(x)) %>%
            as_igraph()
          x <- add_tie_attribute(x, paste0(name, "_", "t", d),
                                 tie_attribute(x, "weight")) %>%
            igraph::delete_edge_attr("weight")
        }
      } else {
        # add names for one-mode y
        y <- add_node_attribute(y, "name", as.character(seq_len(graph_nodes(y))))
        # join ties
        if (isTRUE(is_twomode(x))) { # x is twomode but y is onemode
          y <- as_edgelist(y)
          y <- y %>%
            dplyr::mutate(weight = 1)
          x <- dplyr::bind_rows(y, as_edgelist(x)) %>%
            as_igraph()
          x <- add_tie_attribute(x, paste0(name, "_", "t", d),
                                 tie_attribute(x, "weight")) %>%
            igraph::delete_edge_attr("weight")
        } else { # x and y are onemode
          x <- join_ties(x, as_igraph(y), 
                         attr_name = paste0(name, "_", "t", d))
        }
      }
    }
    x
  }
  
  .get_attributes <- function(ndy, x, name = NULL) {
    for(d in seq_len(dim(ndy)[3])) {
      x <- add_node_attribute(x,
                              attr_name = paste0(name, "_", "t", d),
                              as.vector(ndy[,,d]))
    }
    x
  }
  
  # We always get the dependent network(s) first
  # Identify all dyadic and non-dyadic depvars
  dvs <- lapply(object$depvars, function(x) is.matrix(x[,,1]) )
  ddvs <- names(which(dvs == TRUE))
  # Add in first network as base and add names
  out <- object$depvars[[ddvs[1]]][,,1] # first wave
  if (is_twomode(out) == FALSE) {
    out <- add_node_attribute(out, "name", as.character(seq_len(graph_nodes(out))))
  } else {
    rownames(out) <- as.character(seq_len(nrow(out)))
    colnames(out) <- as.character(paste0("N", seq_len(ncol(out))))
  }
  # add ties from rest of time periods
  out <- .get_rem_time_periods(object$depvars[[ddvs[1]]], out,
                               name = ddvs[1])
  out <- add_tie_attribute(out, paste0(ddvs[1], "_", "t1"),
                           tie_attribute(out, "orig")) %>%
    igraph::delete_edge_attr("orig")

  # Add rest of the dyadic depvars
  if (length(ddvs) > 1) {
    for (l in 2:length(ddvs)) {
      out <- .get_all_time_periods(object$depvars[[ddvs[l]]], out,
                                   name = ddvs[l])
    }
  }
  
  # add dycCovar
  for (k in seq_len(length(object$dycCovars))) {
    out <- join_ties(out, as_igraph(object$dycCovars[k]), 
                     attr_name = paste0(names(object$dycCovars)[k]))
  }
  # add dyvCovars
  for (k in seq_len(length(object$dyvCovars))) {
    out <- .get_all_time_periods(object$dyvCovars[[k]], out,
                                 name = paste0(names(object$dyvCovars)[k]))
  }
  # Add any behavioral depvars
  if(length(which(dvs == FALSE)) > 0){
    bdvs <- names(which(dvs == FALSE))
    for (b in seq_len(length(bdvs))) {
      out <- .get_attributes(object$depvars[[bdvs[b]]], out,
                             name = bdvs[b])
    }
  }
  # add composition change
  for (k in seq_len(length(object$compositionChange))) {
    out <- add_node_attribute(out, paste0(names(object$compositionChange)[k]),
                              as.vector(object$compositionChange[[k]]))
  }
  # add cCovar
  for (k in seq_len(length(object$cCovars))) {
    out <- add_node_attribute(out, paste0(names(object$cCovars)[k]),
                              as.vector(object$cCovars[[k]]))
  }
  # add vCovar
  for (k in seq_len(length(object$vCovars))) {
    out <- .get_attributes(object$vCovars[[k]], out,
                          name = paste0(names(object$vCovars)[k]))
  }
  out
}

# tidygraph ####

#' @rdname as
#' @export
as_tidygraph <- function(object, twomode = FALSE) UseMethod("as_tidygraph")

#' @export
as_tidygraph.data.frame <- function(object, twomode = FALSE) {
  tidygraph::as_tbl_graph(as_igraph(object))
}

#' @export
as_tidygraph.matrix <- function(object, twomode = FALSE) {
  tidygraph::as_tbl_graph(as_igraph(object))
}

#' @export
as_tidygraph.igraph <- function(object, twomode = FALSE) {
  tidygraph::as_tbl_graph(object)
}

#' @export
as_tidygraph.tbl_graph <- function(object, twomode = FALSE) {
  object
}

#' @export
as_tidygraph.network <- function(object, twomode = FALSE) {
  tidygraph::as_tbl_graph(as_igraph(object))
}

#' @export
as_tidygraph.network.goldfish <- function(object,
                                          twomode = FALSE) {
  
  # orig <- deparse(substitute(object))
  # y <- ls(envir = .GlobalEnv)
  # envir  <- .GlobalEnv
  # 
  # classesToKeep <- c("nodes.goldfish", "network.goldfish")
  # checkClasses <- function(object, classes) vapply(classes, 
  #                               function(x) methods::is(object, x), logical(1))
  # ClassFilter <- function(x) any(checkClasses(get(x), classes = classesToKeep))
  # gfobjs <- Filter(ClassFilter, y)
  # classes <- vapply(gfobjs, FUN = function(x) checkClasses(get(x), 
  #                                classes = classesToKeep), 
  #                   FUN.VALUE = logical(length(classesToKeep)))
  
  if(sum(object)==0){
    out <- igraph::graph_from_data_frame(d = get(attr(object, "events"))[,2:4],
                                         directed = attr(object, "directed"),
                                         vertices = get(attr(object, "nodes")))
    out <- as_tidygraph(out)
  } else stop("Non-empty starts are not yet supported by this function.")
  
  # if(rowSums(classes)['network.goldfish']>1){
  #   nets <- colnames(classes)[classes['network.goldfish', ]==TRUE]
  #   nets <- nets[nets != orig]
  #   for(edges in nets){
  #     eventlist <- get(attr(get(edges), "events"))
  #     eventlist <- eventlist[,2:4]
  #     eventlist <- eventlist[!duplicated(eventlist),] # currently not carrying multiple ties across
  #     other <- as_tidygraph(eventlist)
  #     out <- join_edges(out, other, edges)
  #   }
  # }
  
  out
}

#' @export
as_tidygraph.siena <- function(object, twomode = FALSE) {
  as_tidygraph(as_igraph.siena(object, twomode = FALSE))
}

# Network ####

#' @rdname as
#' @export
as_network <- function(object,
                       twomode = FALSE) UseMethod("as_network")

#' @export
as_network.network <- function(object,
                               twomode = FALSE) {
  object
}

#' @export
as_network.matrix <- function(object,
                              twomode = FALSE) {
  # Convert to adjacency matrix if not square already
  if (is_twomode(object)) {
    out <- to_multilevel(object)
  } else out <- object
  network::as.network(out, 
                      directed = is_directed(object),
                      bipartite   = ifelse(is_twomode(object),
                                           nrow(object),
                                           FALSE),
                      loops = ifelse(sum(diag(out)) > 0, TRUE, FALSE),
                      ignore.eval = ifelse(is_weighted(object),
                                           FALSE, TRUE),
                      names.eval  = ifelse(is_weighted(object),
                                           "weight", NULL))
}

#' @export
as_network.igraph <- function(object,
                              twomode = FALSE) {
  name <- type <- NULL
  attr <- as.data.frame(igraph::get.vertex.attribute(object))
  if ("name" %in% colnames(attr)) attr <- subset(attr, select = c(-name))
  if ("type" %in% colnames(attr)) attr <- subset(attr, select = c(-type))
  out <- as_network(as_matrix(object))
  if (length(attr) > 0) {
    out <- network::set.vertex.attribute(out, names(attr), attr)
  }
  out
}

#' @export
as_network.tbl_graph <- function(object,
                                 twomode = FALSE) {
  nodes <- NULL
  attr <- as.data.frame(activate(object, nodes))[-1]
  out <- as_network(as_matrix(object))
  if (length(attr) > 0) {
    out <- network::set.vertex.attribute(out, names(attr), attr)
  }
  out
}

#' @export
as_network.data.frame <- function(object,
                                  twomode = NULL) {
  if ("tbl_df" %in% class(object)) object <- as.data.frame(object)
  as_network(as_matrix(object, twomode))
}

#' @export
as_network.network.goldfish <- function(object,
                                        twomode = FALSE) {
  as_network(as_igraph(object, twomode = twomode))
}

#' @export
as_network.siena <- function(object, twomode = FALSE) {
  as_network(as_igraph.siena(object, twomode = FALSE))
}

# RSiena ####

# #' @rdname as
# #' @export
# as_siena <- function(object,
#                       twomode = FALSE) UseMethod("as_siena")

# #' @export
# as_siena.tbl_graph <- function(object, twomode = FALSE){
#   RSiena::sienaDependent()
#   RSiena::coCovar()
#   RSiena::varCovar()
#   RSiena::sienaDependent()
#   RSiena::sienaDataCreate()
# }

# graphAM ####

#' @rdname as
#' @importFrom methods new
#' @export
as_graphAM <- function(object, twomode = NULL) UseMethod("as_graphAM")

setClass("attrData", representation(data="list",
                                    defaults="list"))

setClass("renderInfo", 
         representation(nodes="list", # information on nodes
                        edges="list", # information on edges
                        graph="list",
                        pars="list")) # list passed on to graph.par before rendering

setClass("graphBase")

setClass("graph", representation(## edgemode="character",
  edgeData="attrData",
  nodeData="attrData",
  
  renderInfo="renderInfo",
  ## nodeInfo="list",
  ## edgeInfo="list",
  
  graphData="list",
  "VIRTUAL"),
  contains = "graphBase")

setClass("graphAM", contains="graph",
         slots = c(adjMat="matrix", edgemode="character"))

#' @export
as_graphAM.matrix <- function(object, twomode = NULL){
  methods::new("graphAM", adjMat = to_onemode(object), 
               edgemode = ifelse(is_directed(object), 
                                 "directed", "undirected"))
}

#' @export
as_graphAM.igraph <- function(object, twomode = NULL){
  as_graphAM(as_matrix(object), twomode)
}

#' @export
as_graphAM.tbl_graph <- function(object, twomode = NULL){
  as_graphAM(as_matrix(object), twomode)
}

#' @export
as_graphAM.network <- function(object, twomode = NULL){
  as_graphAM(as_matrix(object), twomode)
}

#' @export
as_graphAM.data.frame <- function(object, twomode = NULL){
  as_graphAM(as_matrix(object), twomode)
}

#' @export
as_graphAM.siena <- function(object, twomode = NULL){
  as_graphAM(as_matrix(object), twomode)
}

#' @export
as_graphAM.network.goldfish <- function(object, twomode = NULL){
  as_graphAM(as_matrix(object), twomode)
}
