#' Core-periphery clustering algorithms
#' @references 
#' Lip, Sean Z. W. 2011. 
#' “A Fast Algorithm for the Discrete Core/Periphery Bipartitioning Problem.”
#' @examples 
#' mpn_elite_usa_advice %>% as_tidygraph %>% 
#'  mutate(corep = node_core(mpn_elite_usa_advice)) %>% 
#'  autographr(node_color = "corep")
#' @export
node_core <- function(object){
  degi <- node_degree(object, normalized = FALSE)
  nord <- order(degi, decreasing = TRUE)
  zbest <- graph_nodes(object)*3
  kbest <- 0
  z <- 1/2*sum(degi)
  for(k in 1:(graph_nodes(object)-1)){
    z <- z + k - 1 - degi[nord][k]
    if(z < zbest){
      zbest <- z
      kbest <- k
    }
  }
  out <- ifelse(seq_len(graph_nodes(object)) %in% nord[seq_len(kbest)],
         2,1)
  if(is_labelled(object)) names(out) <- node_names(object)
  make_node_member(out, object)
}