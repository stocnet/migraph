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

mat.side <- matrix(0,4,4)
mat.side[1:4,1] <- 1
mat.side[3,2] <- 1
mat.side[4,3] <- 1
mat.side[1,4] <- 1

mat.link <- matrix(0,4,4)
mat.link[1:2,1] <- 1
mat.link[2:3,2] <- 1
mat.link[3:4,3] <- 1
mat.link[c(1,4),4] <- 1

mat.pole <- matrix(1,4,4)
mat.pole[1:2,3:4] <- 0
mat.pole[3:4,1:2] <- 0

mat.only <- matrix(1,5,1)

mat.sole <- matrix(0,4,4)
mat.sole[1:4,1] <- 1

library(igraph)
# pdf("~/Desktop/dombycoh.pdf", width=11, height=8)
par(mfrow = c(2,3))
plot_twomode(mat.even, main="mat.even",
     sub = paste(round(twomode_dominance(mat.even),2), round(twomode_coherence(mat.even),2),
                 # round(twomode_clustering(mat.even) / round(twomode_fragmentation(mat.even),2), 
                 sep = "\n"))
plot_twomode(mat.dist, main="mat.dist",
     sub = paste(round(twomode_dominance(mat.dist),2), round(twomode_coherence(mat.dist),2),
                 # twomode_clustering(mat.dist) / round(twomode_fragmentation(mat.dist),2), 
                 sep = "\n"))
plot_twomode(mat.none, main="mat.none",
     sub = paste(round(twomode_dominance(mat.none),2), round(twomode_coherence(mat.none),2),
                 # twomode_fragmentation(mat.none), twomode_clustering(mat.none), 
                 sep = "\n"))
plot_twomode(mat.part, main="mat.part",
     sub = paste(round(twomode_dominance(mat.part),2), round(twomode_coherence(mat.part),2),
                 # round(twomode_fragmentation(mat.part),2), round(twomode_clustering(mat.part),2), 
                 sep = "\n"))
plot_twomode(mat.some, main="mat.some",
     sub = paste(round(twomode_dominance(mat.some),2), round(twomode_coherence(mat.some),2),
                 # twomode_fragmentation(mat.some), twomode_clustering(mat.some), 
                 sep = "\n"))
plot_twomode(mat.only, main="mat.only",
     sub = paste(round(twomode_dominance(mat.only),2), round(twomode_coherence(mat.only),2),
                 # twomode_fragmentation(mat.only), twomode_clustering(mat.only), 
                 sep = "\n"))
# dev.off()

pdf("~/Desktop/2x2typo.pdf", width=11, height=8)
par(mfrow = c(2,2))
plot_twomode(mat.side, main="High Dom, Low Coh",
             sub = paste(round(twomode_dominance(mat.side),2), round(twomode_clustering(mat.side),2),
                         sep = ", "))
plot_twomode(mat.sole, main="High Dom, High Coh",
             sub = paste(round(twomode_dominance(mat.sole),2), round(twomode_clustering(mat.sole),2),
                         sep = ", "))
plot_twomode(mat.link, main="Low Dom, Low Coh",
             sub = paste(round(twomode_dominance(mat.link),2), round(twomode_clustering(mat.link),2),
                         sep = ", "))
plot_twomode(mat.pole, main="Low Dom, High Coh",
             sub = paste(round(twomode_dominance(mat.pole),2), round(twomode_clustering(mat.pole),2),
                         sep = ", "))
dev.off()

# Analyse real data
# rm(list=ls())
library(gnevar)
library(ggplot2)
# plot_twomode(slice(node1=stat_actor, node2=envr_agree, ties=envr_membs, time="2008-01-01"))

envr_topo <- vector()
for (t in 1950:2010){
  temp <- slice(node1=stat_actor, node2=envr_agree, ties=envr_membs, time=paste(t,"-01-01",sep=""))
  envr_topo <- rbind(envr_topo,
                     c(Coherence=twomode_clustering(temp), Dominance=twomode_dominance(temp), Year=t))
}
envr_topo <- as.data.frame(envr_topo)
plot_2x2(envr_topo)
ggsave("~/Dropbox/Research/Project EPSION/TopoTypo/envr_topo.pdf",width=8,height=8)
  
fish_agree <- envr_agree[grepl("Fish",envr_agree$Subject),]
fish_membs <- envr_membs[envr_membs$MitchID %in% fish_agree$MitchID,]
fish_topo <- vector()
for (t in 1950:2010){
  temp <- slice(node1=stat_actor, node2=fish_agree, ties=fish_membs, time=paste(t,"-01-01",sep=""))
  fish_topo <- rbind(fish_topo,
                     c(Coherence=twomode_clustering(temp), Dominance=twomode_dominance(temp), Year=t))
}
fish_topo <- as.data.frame(fish_topo)
plot_2x2(fish_topo)
ggsave("~/Dropbox/Research/Project EPSION/TopoTypo/fish_topo.pdf",width=8,height=8)

watr_agree <- envr_agree[grepl("Freshwater",envr_agree$Subject),]
watr_membs <- envr_membs[envr_membs$MitchID %in% watr_agree$MitchID,]
watr_topo <- vector()
for (t in 1950:2010){
  temp <- slice(node1=stat_actor, node2=watr_agree, ties=watr_membs, time=paste(t,"-01-01",sep=""))
  watr_topo <- rbind(watr_topo,
                     c(Coherence=twomode_clustering(temp), Dominance=twomode_dominance(temp), Year=t))
}
watr_topo <- as.data.frame(watr_topo)
plot_2x2(watr_topo)
ggsave("~/Dropbox/Research/Project EPSION/TopoTypo/watr_topo.pdf",width=8,height=8)

trad_topo <- vector()
for (t in 1960:2010){
  temp <- slice(node1=stat_actor, node2=trad_agree, ties=trad_membs, time=paste(t,"-01-01",sep=""))
  trad_topo <- rbind(trad_topo,
                     c(Coherence=twomode_clustering(temp), Dominance=twomode_dominance(temp), Year=t))
}
trad_topo <- as.data.frame(trad_topo)
plot_2x2(trad_topo)
ggsave("~/Dropbox/Research/Project EPSION/TopoTypo/trad_topo.pdf",width=8,height=8)

ally_topo <- vector()
for (t in 1950:2010){
  temp <- slice(node1=stat_actor, node2=ally_agree, ties=ally_membs, time=paste(t,"-01-01",sep=""))
  ally_topo <- rbind(ally_topo,
                     c(Coherence=twomode_clustering(temp), Dominance=twomode_dominance(temp), Year=t))
}
ally_topo <- as.data.frame(ally_topo)
plot_2x2(ally_topo)
ggsave("~/Dropbox/Research/Project EPSION/TopoTypo/ally_topo.pdf",width=8,height=8)


str(ally_agree)


