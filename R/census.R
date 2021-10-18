#' Triad census by nodes
#' @param object a migraph-consistent object
#' @importFrom igraph vcount graph.neighborhood delete_vertices triad_census
#' @examples 
#' node_triad_census(adolescent_society)
#' @export
node_triad_census <- function(object){
  x <- as_igraph(object)
  out <- vector() # This line intialises an empty vector
  for (i in seq_len(igraph::vcount(x))){ # For each (i) in 
    nb.wi <- igraph::graph.neighborhood(x, order = 1, V(x)[i], mode = 'all')[[1]] 
    # Get i's local neighbourhood. See also make_ego_graph()
    nb.wi <- nb.wi + (igraph::vcount(x) - igraph::vcount(nb.wi)) 
    # Add the removed vertices back in (empty) for symmetry
    nb.wo <- igraph::delete_vertices(nb.wi, i)
    # Make a graph without i in it
    out <- rbind(out,
                 igraph::triad_census(nb.wi) - igraph::triad_census(nb.wo) )
    # Get the difference in triad census between the local graph
    # with and without i to get i's triad census
  } # Close the for loop
  colnames(out) <- c("003","012","102","021D",
                     "021U","021C","111D","111U",
                     "030T","030C","201","120D",
                     "120U","120C","210","300")
  out # This line says the function returns the output
}

#' Triad census for the whole graph
#' @param object a migraph-consistent object
#' @export
graph_triad_census <- function(object){
  if(is_twomode(object)){
    stop("A twomode or multilevel option for a triad census is not yet implemented.")
  } else {
    igraph::triad_census(as_igraph(object))  
  }
}