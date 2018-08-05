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

mat.part <- matrix(0,5,5)
mat.part[1:3,1] <- 1
mat.part[1:2,2] <- 1
mat.part[4:5,3] <- 1
mat.part[4:5,4] <- 1
mat.part[3,5] <- 1

mat.some <- matrix(0,5,1)
mat.some[1:3,1] <- 1

mat.only <- matrix(1,5,1)

library(igraph)
# pdf("~/Desktop/test.pdf", width=11, height=8)
par(mfrow = c(2,3))
plot(graph_from_incidence_matrix(t(mat.even)), main="mat.even",
     sub = paste(twomode_centralization_degree(mat.part), twomode_fragmentation(mat.even), sep = ", "),
     layout = layout_as_bipartite,
     vertex.frame.color=NA, vertex.label=NA, vertex.size=20,
     vertex.shape=c(rep("square",ncol(mat.even)),rep("circle",nrow(mat.even))),
     vertex.color=c(rep("lightblue",ncol(mat.even)),rep("green",nrow(mat.even))))
plot(graph_from_incidence_matrix(t(mat.dist)), main="mat.dist",
     sub = paste(twomode_centralization_degree(mat.part), twomode_fragmentation(mat.dist), sep = ", "),
     layout = layout_as_bipartite,
     vertex.frame.color=NA, vertex.label=NA, vertex.size=20,
     vertex.shape=c(rep("square",ncol(mat.dist)),rep("circle",nrow(mat.dist))),
     vertex.color=c(rep("lightblue",ncol(mat.dist)),rep("green",nrow(mat.dist))))
plot(graph_from_incidence_matrix(t(mat.none)), main="mat.none",
     sub = paste(twomode_centralization_degree(mat.part), twomode_fragmentation(mat.none), sep = ", "),
     layout = layout_as_bipartite,
     vertex.frame.color=NA, vertex.label=NA, vertex.size=20,
     vertex.shape=c(rep("square",ncol(mat.none)),rep("circle",nrow(mat.none))),
     vertex.color=c(rep("lightblue",ncol(mat.none)),rep("green",nrow(mat.none))))
plot(graph_from_incidence_matrix(t(mat.part)), main="mat.part",
     sub = paste(twomode_centralization_degree(mat.part), twomode_fragmentation(mat.part), sep = ", "),
     layout = layout_as_bipartite,
     vertex.frame.color=NA, vertex.label=NA, vertex.size=20,
     vertex.shape=c(rep("square",ncol(mat.part)),rep("circle",nrow(mat.part))),
     vertex.color=c(rep("lightblue",ncol(mat.part)),rep("green",nrow(mat.part))))
plot(graph_from_incidence_matrix(t(mat.some)), main="mat.some",
     sub = paste(twomode_centralization_degree(mat.part), twomode_fragmentation(mat.some), sep = ", "),
     layout = layout_as_bipartite,
     vertex.frame.color=NA, vertex.label=NA, vertex.size=20,
     vertex.shape=c(rep("square",ncol(mat.some)),rep("circle",nrow(mat.some))),
     vertex.color=c(rep("lightblue",ncol(mat.some)),rep("green",nrow(mat.some))))
plot(graph_from_incidence_matrix(t(mat.only)), main="mat.only",
     sub = paste(twomode_centralization_degree(mat.part), twomode_fragmentation(mat.only), sep = ", "),
     layout = layout_as_bipartite,
     vertex.frame.color=NA, vertex.label=NA, vertex.size=20,
     vertex.shape=c(rep("square",ncol(mat.only)),rep("circle",nrow(mat.only))),
     vertex.color=c(rep("lightblue",ncol(mat.only)),rep("green",nrow(mat.only))))
# dev.off()
