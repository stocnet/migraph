mat.even <- matrix(0,5,3)
mat.even[1:2,1] <- 1
mat.even[2:3,2] <- 1
mat.even[4:5,3] <- 1

mat.dist <- matrix(0,5,3)
mat.dist[1:2,1] <- 1
mat.dist[,2] <- 1
mat.dist[4:5,3] <- 1

mat.none <- matrix(0,5,2)
mat.none[,1] <- 1

mat.some <- matrix(0,5,1)
mat.some[1:3,1] <- 1

mat.only <- matrix(1,5,1)

library(igraph)
par(mfrow = c(2,3))
plot(graph_from_incidence_matrix(mat.even), main="mat.even",
     sub = twomode_centralization_degree(mat.even, "cols"),
     layout = layout_as_bipartite,
     vertex.frame.color=NA, vertex.label=NA, vertex.size=20,
     vertex.shape=c(rep("circle",nrow(mat.even)),rep("square",ncol(mat.even))),
     vertex.color=c(rep("green",nrow(mat.even)),rep("lightblue",ncol(mat.even))))
plot(graph_from_incidence_matrix(mat.dist), main="mat.dist",
     sub = twomode_centralization_degree(mat.dist, "cols"),
     layout = layout_as_bipartite,
     vertex.frame.color=NA, vertex.label=NA, vertex.size=20,
     vertex.shape=c(rep("circle",nrow(mat.dist)),rep("square",ncol(mat.dist))),
     vertex.color=c(rep("green",nrow(mat.dist)),rep("lightblue",ncol(mat.dist))))
plot(graph_from_incidence_matrix(mat.none), main="mat.none",
     sub = twomode_centralization_degree(mat.none, "cols"),
     layout = layout_as_bipartite,
     vertex.frame.color=NA, vertex.label=NA, vertex.size=20,
     vertex.shape=c(rep("circle",nrow(mat.none)),rep("square",ncol(mat.none))),
     vertex.color=c(rep("green",nrow(mat.none)),rep("lightblue",ncol(mat.none))))
plot(graph_from_incidence_matrix(mat.some), main="mat.some",
     sub = twomode_centralization_degree(mat.some, "cols"),
     layout = layout_as_bipartite,
     vertex.frame.color=NA, vertex.label=NA, vertex.size=20,
     vertex.shape=c(rep("circle",nrow(mat.some)),rep("square",ncol(mat.some))),
     vertex.color=c(rep("green",nrow(mat.some)),rep("lightblue",ncol(mat.some))))
plot(graph_from_incidence_matrix(mat.only), main="mat.only",
     sub = twomode_centralization_degree(mat.only, "cols"),
     layout = layout_as_bipartite,
     vertex.frame.color=NA, vertex.label=NA, vertex.size=20,
     vertex.shape=c(rep("circle",nrow(mat.only)),rep("square",ncol(mat.only))),
     vertex.color=c(rep("green",nrow(mat.only)),rep("lightblue",ncol(mat.only))))

     