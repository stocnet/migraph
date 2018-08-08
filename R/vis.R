plot.twomode <- function(mat, ...){
  require(igraph)
  plot(graph_from_incidence_matrix(t(mat)), ...,
                              layout = layout_as_bipartite,
                              vertex.frame.color=NA, vertex.label=NA, vertex.size=20,
                              vertex.shape=c(rep("square",ncol(mat)),rep("circle",nrow(mat))),
                              vertex.color=c(rep("lightblue",ncol(mat)),rep("green",nrow(mat))))
}
