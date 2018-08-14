plot.twomode <- function(mat, greyscale=T, ...){
  
  require(igraph)

  if(greyscale) {
    colors <- c(rep("black",ncol(mat)),rep("white",nrow(mat)))
    frames <- "black"
  } else {
    colors <- c(rep("lightblue",ncol(mat)),rep("green",nrow(mat)))
    frames <- NA
  }
  
  plot(graph_from_incidence_matrix(t(mat)), ...,
                              layout = layout_as_bipartite,
                              vertex.frame.color=frames, vertex.label=NA, vertex.size=20,
                              vertex.shape=c(rep("square",ncol(mat)),rep("circle",nrow(mat))),
                              vertex.color=colors
       )
}
