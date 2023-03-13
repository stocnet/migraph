#' Adding and copying attributes from one network to another
#' 
#' @description 
#'   These functions allow users to add nodes, ties, or attributes to the nodes or ties
#'   of a network.
#'   The `join_*()`, `mutate_*()`, `select_*()`, `filter_*()`, `rename_*()`, and `summarise_*()`
#'   functions adapt the `{dplyr}`-type syntax to work with networks of any type.
#'   The `add_*()` functions operate similarly to in `{igraph}`.
#' @family manipulations
#' @inheritParams is
#' @param object2 A second object to copy nodes or edges from.
#' @param attr_name Name of the new attribute in the resulting object.
#' @param vector A vector of values for the new attribute.
#' @name add
NULL

#' @describeIn add Copies node attributes from a given graph into specified graph
#' @examples 
#'   other <- create_empty(4) %>% mutate(name = c("Sue", "Tim", "Pam", "Mark"))
#'   join_nodes(ison_adolescents, other)
#' @export
join_nodes <- function(.data, source, by = NULL,
                       join_type = c("full","left", "right", "inner")){
  join_type <- match.arg(join_type)
  out <- as_tidygraph(.data)
  source <- as_tidygraph(source)
  switch(join_type,
         "full" = dplyr::full_join(out, source, by = by, copy = TRUE),
         "left" = dplyr::left_join(out, source, by = by, copy = TRUE),
         "right" = dplyr::right_join(out, source, by = by, copy = TRUE),
         "inner" = dplyr::inner_join(out, source, by = by, copy = TRUE))
}

#' @describeIn add Copies ties from another graph to specified graph and 
#' adds a tie attribute identifying the ties that were newly added
#' @importFrom igraph add_edges
#' @importFrom rlang :=
#' @importFrom dplyr mutate summarise across group_by everything
#' @examples 
#'   autographr(mpn_elite_mex)
#'   both <- join_ties(mpn_elite_mex, generate_random(mpn_elite_mex), "random")
#'   autographr(both)
#'   random <- to_uniplex(both, "random")
#'   autographr(random)
#'   autographr(to_uniplex(both, "orig"))
#' @export
join_ties <- function(object, object2, attr_name){
  edges <- NULL
  from <- NULL
  to <- NULL
  el <- c(t(as.matrix(as_edgelist(object2))))
  obj <- as_tidygraph(object) %>% 
    activate(edges)
  if(ncol(as.data.frame(obj)) < 3){
    obj <- obj %>% igraph::set_edge_attr("orig", value = 1)
  } 
  out <- igraph::add_edges(as_igraph(obj),
                           el, object2 = 1) %>% 
    as_tidygraph()
  
  if(!missing(attr_name)){
    out <- out %>% activate(edges) %>% 
      rename(!!attr_name := "object2")
  }
  
  edges <- out %>%
    activate(edges) %>%
    as.data.frame() %>% 
    dplyr::group_by(from, to) %>%
    dplyr::summarise(across(everything(), 
                            function(x){
                              out <- suppressWarnings(max(x, na.rm = TRUE))
                              if(is.infinite(out)) out <- 0
                              out
                            }), 
                     .groups = "keep") %>% ungroup()
  nodes <- out %>% activate(nodes) %>% as.data.frame()
  tidygraph::tbl_graph(nodes, edges, 
                       directed = is_directed(object))
  
}

#' @describeIn add Add additional ties to a network
#' @importFrom igraph add_edges
#' @examples
#'   add_nodes(ison_adolescents, 4, list(name = c("Matthew", "Mark", "Luke", "Tim")))
#' @export
add_nodes <- function(.data, nodes, attribute = NULL) UseMethod("add_nodes")

#' @export
add_nodes.igraph <- function(.data, nodes, attribute = NULL){
  igraph::add_vertices(.data, nv = nodes, attr = attribute)
}

#' @export
add_nodes.tbl_graph <- function(.data, nodes, attribute = NULL){
  as_tidygraph(add_nodes(as_igraph(.data), nodes, attribute))
}

#' @export
add_nodes.network <- function(.data, nodes, attribute = NULL){
  as_network(add_nodes(as_igraph(.data), nodes, attribute))
}

#' @describeIn add Add additional ties to a network
#' @importFrom igraph add_edges
#' @examples
#'   add_ties(ison_adolescents, c(1,2), list(time = 2, increment = -1))
#' @export
add_ties <- function(.data, ties, attribute = NULL) UseMethod("add_ties")

#' @export
add_ties.igraph <- function(.data, ties, attribute = NULL){
  igraph::add_edges(.data, edges = ties, attr = attribute)
}

#' @export
add_ties.tbl_graph <- function(.data, ties, attribute = NULL){
  as_tidygraph(add_ties(as_igraph(.data), ties, attribute))
}

#' @export
add_ties.network <- function(.data, ties, attribute = NULL){
  as_network(add_ties(as_igraph(.data), ties, attribute))
}

#' @describeIn add Insert specified values from a vector into the graph 
#' as node attributes
#' @importFrom igraph vertex_attr<-
#' @examples 
#' add_node_attribute(mpn_elite_mex, "wealth", 1:35)
#' add_node_attribute(mpn_elite_usa_advice, "wealth", 1:14)
#' @export
add_node_attribute <- function(.data, attr_name, vector){
  if(length(vector)!=network_nodes(.data)){
    if(is_twomode(.data) && any(length(vector) == network_dims(.data))){
      if(length(vector) == network_dims(.data)[1]){
        vector <- c(vector, rep(NA, network_dims(.data)[2]))
      } else if (length(vector) == network_dims(.data)[2]){
        vector <- c(rep(NA, network_dims(.data)[1]), vector)
      }
    } else 
      stop("Attribute vector must be same length as nodes in object.")
  }
  out <- as_igraph(.data)
  igraph::vertex_attr(out, name = attr_name) <- vector
  out
}

#' @describeIn add Insert specified values from a vector into the network.
#'   as tie attributes
#' @importFrom igraph edge_attr
#' @examples
#' add_tie_attribute(ison_adolescents, "weight", c(1,2,1,1,1,3,2,2,3,1))
#' @export
add_tie_attribute <- function(.data, attr_name, vector){
  out <- as_igraph(.data)
  igraph::edge_attr(out, name = attr_name) <- vector
  out
}

#' @export
mutate.igraph <- function(.data, ...){
  .data %>% as_tidygraph() %>% 
    mutate(...) %>% as_igraph()
}

#' @describeIn add Tidy way to add vector as tie attributes.
#' @export
mutate_ties <- function(.data, ...){
  nodes <- edges <- NULL
  out <- as_tidygraph(.data)
  out %>% activate(edges) %>% mutate(...) %>% activate(nodes)
}

#' @describeIn add Tidy way to select tie attributes.
#' @export
select_ties <- function(.data, ...){
  nodes <- edges <- NULL
  out <- as_tidygraph(.data)
  out %>% activate(edges) %>% dplyr::select(...) %>% activate(nodes)
}

#' @describeIn add Tidy way to filter ties based on a logical statement with
#'   relation to some tie attribute.
#' @export
filter_ties <- function(.data, ...){
  nodes <- edges <- NULL
  out <- as_tidygraph(.data)
  out %>% activate(edges) %>% dplyr::filter(...) %>% activate(nodes)
}

#' @describeIn add Tidy way to rename tie attributes.
#' @export
rename_ties <- function(.data, ...){
  nodes <- edges <- NULL
  out <- as_tidygraph(.data)
  out %>% activate(edges) %>% dplyr::rename(...) %>% activate(nodes)
}

#' @describeIn add Tidy way to summarise tie attributes.
#' @importFrom dplyr summarise
#' @export
summarise_ties <- function(.data, ...){
  out <- migraph::as_edgelist(.data) %>% 
    dplyr::summarise(..., .by = c("from","to")) %>% 
    as_tidygraph(twomode = is_twomode(.data))
  out <- as_tidygraph(bind_node_attributes(out, .data))
  if(!is_directed(.data)) out <- to_undirected(out)
  out
}

#' @describeIn add Copying all nodal attributes from one network to another
#' @export
bind_node_attributes <- function(.data, source){
  out <- as_igraph(.data)
  source <- as_igraph(source)
  if(network_nodes(.data) != network_nodes(source)){
    # warning("Not the same dimensions. Coercing to same.")
    out <- add_nodes(out, network_nodes(source) - network_nodes(out))
  }
  for(a in igraph::vertex_attr_names(source)){
    out <- igraph::set.vertex.attribute(out, a, 
                                        value = igraph::get.vertex.attribute(source, a))
  }
  out
}
