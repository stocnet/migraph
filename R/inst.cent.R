mat.even <- matrix(0,5,3)
mat.even[1:2,1] <- 1
mat.even[2:3,2] <- 1
mat.even[4:5,3] <- 1

mat.dist <- matrix(0,5,3)
mat.dist[1:2,1] <- 1
mat.dist[,2] <- 1
mat.dist[4:5,3] <- 1

mat.none <- matrix(0,5,2)
mat.none[1:4,1] <- 1

mat.some <- matrix(0,5,1)
mat.some[1:3,1] <- 1

mat.only <- matrix(1,5,1)

library(igraph)
pdf("~/Desktop/test.pdf", width=11, height=8)
par(mfrow = c(2,3))
plot(graph_from_incidence_matrix(t(mat.even)), main="mat.even",
     sub = twomode_centralization_degree(mat.even, "cols"),
     layout = layout_as_bipartite,
     vertex.frame.color=NA, vertex.label=NA, vertex.size=20,
     vertex.shape=c(rep("square",ncol(mat.even)),rep("circle",nrow(mat.even))),
     vertex.color=c(rep("lightblue",ncol(mat.even)),rep("green",nrow(mat.even))))
plot(graph_from_incidence_matrix(t(mat.dist)), main="mat.dist",
     sub = twomode_centralization_degree(mat.dist, "cols"),
     layout = layout_as_bipartite,
     vertex.frame.color=NA, vertex.label=NA, vertex.size=20,
     vertex.shape=c(rep("square",ncol(mat.dist)),rep("circle",nrow(mat.dist))),
     vertex.color=c(rep("lightblue",ncol(mat.dist)),rep("green",nrow(mat.dist))))
plot(graph_from_incidence_matrix(t(mat.none)), main="mat.none",
     sub = twomode_centralization_degree(mat.none, "cols"),
     layout = layout_as_bipartite,
     vertex.frame.color=NA, vertex.label=NA, vertex.size=20,
     vertex.shape=c(rep("square",ncol(mat.none)),rep("circle",nrow(mat.none))),
     vertex.color=c(rep("lightblue",ncol(mat.none)),rep("green",nrow(mat.none))))
plot(graph_from_incidence_matrix(t(mat.some)), main="mat.some",
     sub = twomode_centralization_degree(mat.some, "cols"),
     layout = layout_as_bipartite,
     vertex.frame.color=NA, vertex.label=NA, vertex.size=20,
     vertex.shape=c(rep("square",ncol(mat.some)),rep("circle",nrow(mat.some))),
     vertex.color=c(rep("lightblue",ncol(mat.some)),rep("green",nrow(mat.some))))
plot(graph_from_incidence_matrix(t(mat.only)), main="mat.only",
     sub = twomode_centralization_degree(mat.only, "cols"),
     layout = layout_as_bipartite,
     vertex.frame.color=NA, vertex.label=NA, vertex.size=20,
     vertex.shape=c(rep("square",ncol(mat.only)),rep("circle",nrow(mat.only))),
     vertex.color=c(rep("lightblue",ncol(mat.only)),rep("green",nrow(mat.only))))
dev.off()
     