devtools::install_bitbucket("jhollway/roctopus", auth_user = "jhollway", password = "VQaKN0TmGD8w")

library(igraph)
# pdf("~/Desktop/dombycoh.pdf", width=11, height=8)
par(mfrow = c(2,3))
plot_twomode(mat.even, main="mat.even",
     sub = paste(round(twomode_dominance(mat.even),2), round(twomode_modularity(mat.even),2),
                 # round(twomode_clustering(mat.even) / round(twomode_fragmentation(mat.even),2), 
                 sep = "\n"))
plot_twomode(mat.dist, main="mat.dist",
     sub = paste(round(twomode_dominance(mat.dist),2), round(twomode_modularity(mat.dist),2),
                 # twomode_clustering(mat.dist) / round(twomode_fragmentation(mat.dist),2), 
                 sep = "\n"))
plot_twomode(mat.none, main="mat.none",
     sub = paste(round(twomode_dominance(mat.none),2), round(twomode_modularity(mat.none),2),
                 # twomode_fragmentation(mat.none), twomode_clustering(mat.none), 
                 sep = "\n"))
plot_twomode(mat.part, main="mat.part",
     sub = paste(round(twomode_dominance(mat.part),2), round(twomode_modularity(mat.part),2),
                 # round(twomode_fragmentation(mat.part),2), round(twomode_clustering(mat.part),2), 
                 sep = "\n"))
plot_twomode(mat.some, main="mat.some",
     sub = paste(round(twomode_dominance(mat.some),2), round(twomode_modularity(mat.some),2),
                 # twomode_fragmentation(mat.some), twomode_clustering(mat.some), 
                 sep = "\n"))
plot_twomode(mat.only, main="mat.only",
     sub = paste(round(twomode_dominance(mat.only),2), round(twomode_modularity(mat.only),2),
                 # twomode_fragmentation(mat.only), twomode_clustering(mat.only), 
                 sep = "\n"))
# dev.off()

# attr.diff <- c(1,1,1,4)
# attr.ramp <- c(1,2,3,4)
# attr.pole <- c(1,1,2,2)
# attr.even <- c(1,1,1,1)
# attr.altr <- c(1,2,1,2)
# attr.only <- c(2,1,1,1)
# 
pdf("~/Dropbox/Research/Project EPSION/TopoTypo/2x2typo.pdf", width=8, height=8)
par(mfrow = c(2,2))
plot_twomode(mat.hier, c(attr.altr,attr.pole)*10, main="High Dominance, Low Coherence",
             # ,sub = paste(round(twomode_dominance(mat.hier, attr.diff),2), 
             #              round(twomode_modularity(mat.hier, attr.altr),2),
             # sep = ", ")
             vertex.size=c(rep(15,4),attr.pole*10),
             vertex.color=c(rep(c("blue","red"),2),rep("black",4))
)
plot_twomode(mat.core, c(attr.only,attr.diff)*10, main="High Dominance, High Coherence",
             # ,sub = paste(round(twomode_dominance(mat.core, attr.diff),2), 
             #              round(twomode_modularity(mat.core, attr.only),2),
             # sep = ", ")
             vertex.size=c(rep(15,4),attr.pole*10),
             vertex.color=c(rep(c("blue","red"),2),rep("black",4))
)
plot_twomode(mat.link, c(attr.altr,attr.even)*10, main="Low Dominance, Low Coherence",
             # ,sub = paste(round(twomode_dominance(mat.link, attr.even),2), 
             #              round(twomode_modularity(mat.link, attr.altr),2),
             # sep = ", ")
             vertex.size=c(rep(15,4),c(1,2,1,2)*10),
             vertex.color=c(rep(c("blue","red"),2),rep("black",4))
)
plot_twomode(mat.pole, c(attr.pole,attr.even)*10, main="Low Dominance, High Coherence",
             # ,sub = paste(round(twomode_dominance(mat.pole, attr.even),2), 
             #              round(twomode_modularity(mat.pole, attr.pole),2),
             # sep = ", ")
             vertex.size=c(rep(15,4),c(1,2,1,2)*10),
             vertex.color=c(rep("blue",2),rep("red",2),rep("black",4))
)
dev.off()

# is it that the relationship of institutions between actors should be consistent
# (i.e. two actors share similar or dissimilar institutions?)
# or that the relationship of institutions within actors should be consistent (current)
# (i.e. are actors members of similar or dissimilar institutions?)

# Analyse real data
# rm(list=ls())
library(gnevar)
data(list = data(package = "gnevar")$results[,3])

library(ggplot2)
library(bipartite)
# plot_twomode(slice(node1=stat_actor, node2=envr_agree, ties=envr_membs, time="2008-01-01"))

library(wbstats)
gdp_data <- wb(country = unique(stat_actor$StatID),
               indicator = "NY.GDP.MKTP.CD", startdate = 1960, enddate = 2018)
mil_data <- wb(country = unique(stat_actor$StatID),
               indicator = "MS.MIL.XPND.CN", startdate = 1960, enddate = 2018)
wtr_data <- wb(country = unique(stat_actor$StatID),
               indicator = "ER.GDP.FWTL.M3.KD", startdate = 1960, enddate = 2018)
fsh_data <- wb(country = unique(stat_actor$StatID),
               indicator = "ER.FSH.CAPT.MT", startdate = 1960, enddate = 2018)
tex_data <- wb(country = unique(stat_actor$StatID),
               indicator = "NE.EXP.GNFS.CD", startdate = 1960, enddate = 2018)
tim_data <- wb(country = unique(stat_actor$StatID),
               indicator = "NE.IMP.GNFS.CD", startdate = 1960, enddate = 2018)

# table(fish_agree$Secretariat)
fsh_secs <- as.numeric(factor(fish_agree$Secretariat))
fsh_secs[is.na(fsh_secs)] <- max(fsh_secs, na.rm = T)+1
fsh_secs <- cbind(fish_agree$MitchID, fish_secs)
# table(trad_agree$Label)
trd_secs <- as.numeric(factor(trad_agree$Label))
trd_secs[is.na(trd_secs)] <- max(trd_secs, na.rm = T)+1
trd_secs <- cbind(as.numeric(trad_agree$DestaID), trd_secs)
# table(ally_agree$AtopID)
all_secs <- as.numeric(factor(ally_agree$AtopID))
all_secs[is.na(all_secs)] <- max(all_secs, na.rm = T)+1
all_secs <- cbind(ally_agree$AtopID, all_secs)

envr_topo <- twomode_2x2(stat_actor, envr_agree, envr_membs, gdp_data, 1960, 2010)
plot_2x2(envr_topo)
ggsave("~/Dropbox/Research/Project EPSION/TopoTypo/envr_topo.pdf",width=8,height=8)
  
# watr_agree <- envr_agree[grepl("Freshwater",envr_agree$Subject),]
# watr_membs <- envr_membs[envr_membs$MitchID %in% watr_agree$MitchID,]
# watr_topo <- twomode_2x2(stat_actor, watr_agree, watr_membs, wtr_data, 1960, 2010)
# plot_2x2(watr_topo)
# ggsave("~/Dropbox/Research/Project EPSION/TopoTypo/watr_topo2.pdf",width=8,height=8)

fish_agree <- envr_agree[grepl("Fish",envr_agree$Subject),]
fish_membs <- envr_membs[envr_membs$MitchID %in% fish_agree$MitchID,]
fish_topo <- twomode_2x2(stat_actor, fish_agree, fish_membs, fsh_data, fsh_secs, 1960, 2010)
plot_2x2(fish_topo, colour = "forestgreen")
ggsave("~/Dropbox/Research/Project EPSION/TopoTypo/fish_topo2.pdf",width=8,height=8)
pdf("~/Dropbox/Research/Project EPSION/TopoTypo/fish_panel.pdf", width=11, height=8)
par(mfrow=c(1,2))
plotweb(slice(stat_actor, fish_agree, fish_membs, 1960)[,colSums(slice(stat_actor, fish_agree, fish_membs, 1960))>1])
plotweb(slice(stat_actor, fish_agree, fish_membs, 2010)[,colSums(slice(stat_actor, fish_agree, fish_membs, 2010))>1])
dev.off()

trad_topo <- twomode_2x2(stat_actor, trad_agree, trad_membs, tim_data, trd_secs, 1960, 2010)
plot_2x2(trad_topo, colour = "blue")
ggsave("~/Dropbox/Research/Project EPSION/TopoTypo/trad_topo2.pdf",width=8,height=8)
pdf("~/Dropbox/Research/Project EPSION/TopoTypo/trad_panel.pdf", width=11, height=8)
par(mfrow=c(1,2))
plotweb(slice(stat_actor, trad_agree, trad_membs, 1960)[,colSums(slice(stat_actor, trad_agree, trad_membs, 1960))>1])
plotweb(slice(stat_actor, trad_agree, trad_membs, 2010)[,colSums(slice(stat_actor, trad_agree, trad_membs, 2010))>1])
dev.off()

ally_topo <- twomode_2x2(stat_actor, ally_agree, ally_membs, mil_data, all_secs, 1960, 2010)
plot_2x2(ally_topo, colour = "red")
ggsave("~/Dropbox/Research/Project EPSION/TopoTypo/ally_topo2.pdf",width=8,height=8)
pdf("~/Dropbox/Research/Project EPSION/TopoTypo/ally_panel.pdf", width=11, height=8)
par(mfrow=c(1,2))
plotweb(slice(stat_actor, ally_agree, ally_membs, 1960)[,colSums(slice(stat_actor, ally_agree, ally_membs, 1960))>1])
plotweb(slice(stat_actor, ally_agree, ally_membs, 2010)[,colSums(slice(stat_actor, ally_agree, ally_membs, 2010))>1])
dev.off()

# library(gnevar)
# fish_agree$Secretariat[fish_agree$Secretariat=="No known secretariat"] <- NA
# fish_agree$Secretariat[fish_agree$Secretariat=="No secretariat established"] <- NA
# fish_agree$Secretariat[fish_agree$Secretariat=="Secretariat not yet identified"] <- NA
# unique(fish_agree$Secretariat)
# 
# fish_secs <- as.numeric(factor(fish_agree$Secretariat))
# fish_secs[is.na(fish_secs)] <- 0

