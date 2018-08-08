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

# mat.part <- matrix(0,5,5)
# mat.part[1:3,1] <- 1
# mat.part[1:2,2] <- 1
# mat.part[4:5,3] <- 1
# mat.part[4:5,4] <- 1
# mat.part[3,5] <- 1
# 
mat.part <- mat.dist
mat.part[2,1] <- 0
mat.part[1,2] <- 0
mat.part[4,3] <- 0

mat.some <- matrix(0,5,5)
mat.some[1:3,1] <- 1
mat.some[1:3,2] <- 1
mat.some[1:3,3] <- 1
mat.some[4:5,4] <- 1
mat.some[4:5,5] <- 1

mat.only <- matrix(1,5,1)

library(igraph)
# pdf("~/Desktop/dombycoh.pdf", width=11, height=8)
par(mfrow = c(2,3))
plot.twomode(mat.even, main="mat.even",
     sub = paste(round(twomode_dominance(mat.even),2), round(twomode_coherence(mat.even),2),
                 # round(twomode_clustering(mat.even) / round(twomode_fragmentation(mat.even),2), 
                 sep = "\n"))
plot.twomode(mat.dist, main="mat.dist",
     sub = paste(round(twomode_dominance(mat.dist),2), round(twomode_coherence(mat.dist),2),
                 # twomode_clustering(mat.dist) / round(twomode_fragmentation(mat.dist),2), 
                 sep = "\n"))
plot.twomode(mat.none, main="mat.none",
     sub = paste(round(twomode_dominance(mat.none),2), round(twomode_coherence(mat.none),2),
                 # twomode_fragmentation(mat.none), twomode_clustering(mat.none), 
                 sep = "\n"))
plot.twomode(mat.part, main="mat.part",
     sub = paste(round(twomode_dominance(mat.part),2), round(twomode_coherence(mat.part),2),
                 # round(twomode_fragmentation(mat.part),2), round(twomode_clustering(mat.part),2), 
                 sep = "\n"))
plot.twomode(mat.some, main="mat.some",
     sub = paste(round(twomode_dominance(mat.some),2), round(twomode_coherence(mat.some),2),
                 # twomode_fragmentation(mat.some), twomode_clustering(mat.some), 
                 sep = "\n"))
plot.twomode(mat.only, main="mat.only",
     sub = paste(round(twomode_dominance(mat.only),2), round(twomode_coherence(mat.only),2),
                 # twomode_fragmentation(mat.only), twomode_clustering(mat.only), 
                 sep = "\n"))
# dev.off()
