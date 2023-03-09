#' Adding and copying attributes from one graph to another
#' 
#' @description 
#' These functions allow users to add attributes to a graph from another graph
#' or from a specified vector supplied by the user.
#' @family manipulations
#' @inheritParams is
#' @param object2 A second object to copy nodes or edges from.
#' @param attr_name Name of the new attribute in the resulting object.
#' @param vector A vector of values for the new attribute.
#' @name add
NULL

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

#' @describeIn add Add additional ties to a network
#' @importFrom igraph add_edges
#' @examples
#' add_ties(ison_adolescents, "weight", c(1,2,1,1,1,3,2,2,3,1))
#' @export
add_ties <- function(.data, ties, attribute = NULL){
  out <- as_igraph(.data)
  out <- igraph::add_edges(out, edges = ties, attr = attribute)
  out
}

#' @describeIn add Tidy way to add vector as tie attributes.
#' @export
mutate_ties <- function(.data, ...){
  nodes <- edges <- NULL
  out <- as_tidygraph(.data)
  out %>% activate(edges) %>% mutate(...) %>% activate(nodes)
}

#' @describeIn add Copies node attributes from a given graph into specified graph
#' @export
copy_node_attributes <- function(object, object2){
  if(network_nodes(object) != network_nodes(object2)) 
    stop("Objects need to be of compatible dimensions.")
  object <- as_igraph(object)
  object2 <- as_igraph(object2)
  for(a in igraph::vertex_attr_names(object2)){
    object <- igraph::set.vertex.attribute(object, a, 
                                           value = igraph::get.vertex.attribute(object2, a))
  }
  object
}

#' @describeIn add Copies ties from another graph to specified graph and 
#' adds a tie attribute identifying the ties that were newly added
#' @importFrom igraph add_edges
#' @importFrom rlang :=
#' @importFrom dplyr mutate summarise across group_by everything
#' @examples 
#' autographr(mpn_elite_mex)
#' both <- join_ties(mpn_elite_mex, generate_random(mpn_elite_mex), "random")
#' autographr(both)
#' random <- to_uniplex(both, "random")
#' autographr(random)
#' autographr(to_uniplex(both, "orig"))
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

#' @export
mutate.igraph <- function(.data, ...){
  .data %>% as_tidygraph() %>% 
    mutate(...) %>% as_igraph()
}
