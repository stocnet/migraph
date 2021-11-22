#' Adding and copying attributes from one graph to another
#' 
#' @param object A migraph-consistent object.
#' @param object2 A second object to copy nodes or edges from.
#' @param attr_name Name of the new attribute in the resulting object.
#' @param vector A vector of values for the new attribute.
#' @name add
NULL

#' @rdname add
#' @importFrom igraph vertex_attr<-
#' @export
add_node_attributes <- function(object, attr_name, vector){
  if(length(vector)!=graph_nodes(object)) 
    stop("Attribute vector must be same length as nodes in object.")
  object <- as_igraph(object)
  vertex_attr(object, name = attr_name) <- vector
  object
}

#' @rdname add
#' @export
add_edge_attributes <- function(object, object2){
  object <- tidygraph::graph_join(as_tidygraph(object), as_tidygraph(object2))
  object
}

#' @rdname add
#' @export
copy_node_attributes <- function(object, object2){
  if(graph_nodes(object) != graph_nodes(object2)) stop("Objects need to be of compatible dimensions.")
  object <- as_igraph(object)
  object2 <- as_igraph(object2)
  for(a in igraph::vertex_attr_names(object2)){
    object <- igraph::set.vertex.attribute(object, a, value = igraph::get.vertex.attribute(object2, a))
  }
  object
}

#' @rdname add
#' @importFrom igraph add_edges
#' @importFrom rlang :=
#' @examples 
#' autographr(mpn_elite_mex)
#' both <- mutate_edges(mpn_elite_mex, generate_random(mpn_elite_mex), "random")
#' autographr(both)
#' random <- to_uniplex(both, "random")
#' autographr(random)
#' @export
mutate_edges <- function(object, object2, attr_name){
  edges <- NULL
  el <- c(t(as.matrix(as_edgelist(object2))))
  out <- igraph::add_edges(as_igraph(object),
                           el, object2 = 1) %>% 
    as_tidygraph()
  
  if(!missing(attr_name)){
    out <- out %>% activate(edges) %>% 
      rename(!!attr_name := "object2")
  }
  
  edges <- out %>% activate(edges) %>% as.data.frame() %>% group_by(from, to) %>% 
    summarise(across(everything(), 
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
