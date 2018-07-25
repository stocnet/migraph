# library(igraph)
# allgraph <- graph_from_incidence_matrix(twomode)
# plot(allgraph)
# 
# mean(path.length.hist(allgraph)$res)
# 
# smallworldmemb <- matrix(NA,ncol(twomode),10)
# for(c in 2:ncol(twomode)){
#   m <- twomode[,1:c]
#   # r <- twomode.lattice(m)
#   r <- matrix(rbinom(nrow(m)*ncol(m),1, prob = sum(m)/length(m)),nrow(m),ncol(m))
#   smallworldmemb[c,1] <- twomode.clustering(m)
#   smallworldmemb[c,2] <- twomode.clustering(r)
#   smallworldmemb[c,3] <- twomode.clustering(m)/twomode.clustering(r)
#   
#   g <- graph_from_incidence_matrix(m)
#   l <- graph_from_incidence_matrix(r)
#   smallworldmemb[c,4] <- mean_distance(g)
#   smallworldmemb[c,5] <- mean_distance(l)
#   smallworldmemb[c,6] <- mean_distance(g)/mean_distance(l)
#   
#   smallworldmemb[c,7] <- smallworldmemb[c,3]/smallworldmemb[c,6]
#   
#   smallworldmemb[c,8] <- fit_power_law(degree(g, v=V(g)$type))$alpha
#   smallworldmemb[c,9] <- fit_power_law(degree(g, v=V(g)$type))$KS.stat
#   smallworldmemb[c,10] <- fit_power_law(degree(g, v=V(g)$type))$KS.p
# }
# smallworldmemb <- cbind(1:ncol(twomode),smallworldmemb)
# smallworldmemb <- as.data.frame(smallworldmemb)
# names(smallworldmemb) <- c("Num","ObsClust","ExpClust","ClustRat",
#                            "ObsPath","ExpPath","PathRat","SmallWorld",
#                            "KS.alpha","KS.stat","KS.p")
# library(ggplot2)
# ggplot(smallworldmemb, aes(x=Num,y=SmallWorld)) + geom_line() + geom_smooth(span=1.2) + theme_bw()
# ggsave("memb.smallworld.PDF", width = 11, height = 8)
# ggplot(smallworldmemb, aes(x=Num,y=KS.alpha)) + geom_line() + geom_smooth(span=1.2) + theme_bw()
# ggsave("memb.scalefree.PDF", width = 11, height = 8)
# 
# m=twomode
# 
# # Functions ####
# 
# library(igraph)
# 
# c=1
# twomode.smallworld <- function(mat){
#   out <- matrix(NA,ncol(mat),7)
#   for(c in 2:ncol(mat)){
#     m <- mat[,1:c]
#     g <- graph_from_incidence_matrix(m)
#     out[c,1] <- twomode.clustering(m)
#     out[c,4] <- mean_distance(g)
#     
#     r <- r2dtable(100, rowSums(m), colSums(m))
#     out[c,2] <- mean(unlist(lapply(r, twomode.clustering)))
#     out[c,5] <- mean(unlist(lapply(lapply(r, graph_from_incidence_matrix),
#                                   mean_distance)))
# 
#     out[c,3] <- out[c,1]/out[c,2]
#     out[c,6] <- out[c,4]/out[c,5]
#     out[c,7] <- out[c,3]/out[c,6]
#   }
#   out <- cbind(1:ncol(mat),out)
#   out <- as.data.frame(out)
#   names(out) <- c("Num","ObsClust","ExpClust","ClustRat",
#                              "ObsPath","ExpPath","PathRat","SmallWorld")
#   out
# }
# twomode.smallworld(twomode)
# plot(out[,7])
# loess(out[,7])
# 
# library(ggplot2)
# ggplot(out, aes(x=Num,y=SmallWorld)) + geom_line() + geom_smooth(span=0.5) + theme_bw()
# ggsave("memb.smallworld2.PDF", width = 11, height = 8)
# 

#' Two-mode lattice
#'
#' This function allows you to express your love of lattices.
#' @param m A matrix
#' @keywords two-mode
#' @export
#' @examples
#' twomode.lattice(matrix)
twomode.lattice <- function(m){
  out <- matrix(c(rep(1, sum(m)), 
                  rep(0, length(m)-sum(m))),
                nrow(m), ncol(m), byrow = T)
  out <- rbind(out,rep(0,ncol(out)))
  out <- matrix(out, nrow(m), ncol(m), byrow = F)
  out
}

#' Two-mode clustering
#'
#' This function allows you to calculate how much two-mode clustering there is.
#' @param m A matrix
#' @keywords two-mode
#' @export
#' @examples
#' twomode.clusterings(matrix)
twomode.clustering <- function(m){
  twopaths <- crossprod(m)
  diag(twopaths) <- 0
  indegrees <- colSums(m)
  cycle4 <- sum(twopaths * (twopaths-1)) / 
    (sum(twopaths * (twopaths-1)) + sum(twopaths * 
                                          (matrix(indegrees,c,c) - twopaths)))
  cycle4
}


