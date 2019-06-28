#' Plotting two-mode networks
#' 
#' Better default plotting of two-mode networks than igraph
#' @param mat A matrix
#' @param greyscale Whether to use black/white or blue/green, Default: T
#' @param ... Any other arguments passed to igraph.plotting
#' @return A plot
#' @details Makes for clearer communication of structure than igraph defaults
#' for two-mode networks by providing default colours, shapes, size, and frames,
#' and avoiding labels.
#' @examples
#' \dontrun{
#' plot_twomode(create_lattice(6,12))
#' }
#' @import igraph
#' @export 
plot_twomode <- function(mat, attr=NULL, greyscale=T, ...){
  
  require(igraph)
  if(is.null(attr)) attr <- 5

  if(greyscale) {
    colors <- c(rep("black",ncol(mat)),rep("white",nrow(mat)))
    frames <- "black"
  } else {
    colors <- c(rep("lightblue",ncol(mat)),rep("green",nrow(mat)))
    frames <- NA
  }
  
  plot(graph_from_incidence_matrix(t(mat)), ...,
                              layout = layout_as_bipartite,
                              vertex.frame.color=frames, vertex.label=NA,
       vertex.size=attr,
       # if(nrow(mat)<20 & ncol(mat)<20) {vertex.size=20,}
                              vertex.shape=c(rep("square",ncol(mat)),rep("circle",nrow(mat))),
                              vertex.color=colors
       )
}

#' Plotting two-by-two plots
#' 
#' Better default plotting of values traced through a two-by-two than ggplot2
#' @param dat A data.frame
#' @return A plot
#' @details Makes for clearer communication of structure than ggplot2 defaults
#' @examples
#' \dontrun{
#' library(gnevar)
#' library(wbstats)
#' mil_data <- wb(country = unique(stat_actor$StatID), indicator = "MS.MIL.XPND.CN", startdate = 1960, enddate = 2018)
#' ally_secs <- structure((!is.na(ally_agree$Secretariat))*1, names=as.character(ally_agree$AtopID))
#' ally_topo <- twomode_2x2(stat_actor, ally_agree, ally_membs, mil_data, ally_secs, 1960, 2018)
#' plot_2x2(ally_topo)
#' }
#' @import ggplot2
#' @export 
plot_2x2 <- function(dat, colour="red"){
  require(ggplot2)
  ggplot(dat, aes_string(x=names(dat)[1], y=names(dat)[2])) + 
    geom_path(#aes_string(color=names(dat)[3])#, 
              # arrow = arrow(angle=15, type="closed")
      color=colour
              ) + 
    annotate("text", x=dat[1,1], y=dat[1,2], label=dat[1,3]) +
    annotate("text", x=dat[dat[,3]==1990,1], y=dat[dat[,3]==1990,2], label=dat[dat[,3]==1990,3]) +
    annotate("text", x=dat[nrow(dat),1], y=dat[nrow(dat),2], label=dat[nrow(dat),3]) +
    scale_x_continuous(limits = c(0,1)) + scale_y_continuous(limits = c(0,1)) +
    theme_minimal() + geom_vline(xintercept = .5, color="darkgrey") + geom_hline(yintercept = .5, color="darkgrey")
}

#' Plotting multilevel networks
#' 
#' Plotting of a two-mode network with multilevel structure revealed
#' @param mat An incidence matrix
#' @return A plot
#' @details Removes all isolates to clarify structure. 
#' Relies on Fruchterman-Rheingold to give initial coordinates, 
#' which is then rotated around the x-axis.
#' TODO: Allow more than two levels to be plotted at once.
#' @examples
#' \dontrun{
#' plot_multilevel(create_random(10,12,0.2))
#' }
#' @import igraph
#' @export 
plot_multilevel <- function(mat,levels=NULL){
  require(igraph)
  if(!is.null(levels)) stop("Level specification not currently allowed")#c(rep(0,nrow(mat)), rep(1,ncol(mat)))
  
  g <- graph_from_incidence_matrix(t(mat))
  g <- delete_vertices(g, degree(g)==0)
  f <- layout_with_fr(g)
  f <- cbind(f, (!V(g)$type)*12)
  rot <- rbind(c(1, 0, 0),
               c(0, cos(45), -sin(45)),
               c(0, sin(45), cos(45))
               )
  
  f <- f %*% rot
  plot(g, vertex.frame.color=NA, vertex.label=NA, vertex.size=3,
       vertex.color=c(rep("blue",ncol(mat)),rep("green",nrow(mat))),
       vertex.shape=c(rep("square",ncol(mat)),rep("circle",nrow(mat))),
       layout = f)
  
  
  
  
  
}

#' Plotting multilevel networks on a globe
#' 
#' Plotting of a two-mode network with multilevel structure revealed
#' @param edges A data frame that includes (e.g. country) actors in the first column,
#' and institutional ties in the second column.
#' @param actors A data frame that includes 'StatID' variable (for identification),
#' and 'Latitude' and 'Longitude' variables for plotting.
#' @param time A date to render network for.
#' @return An html/js viewer pane
#' @details Will separate and plot bilateral and pluri-/mini-/multilateral agreements
#' with different colours. It will also plot a pin for the centroid marker of each
#' multilateral agreement.
#' @examples
#' \dontrun{
#' plot_globalnet(mat)
#' }
#' @import threejs
#' @import geosphere
#' @export 
plot_globalnet <- function(edges, actors, time){
  library(threejs)
  eg <- edges[edges$Beg <= time & edges$End >= time,]
  
  # Compile bilaterals
  beg  <- eg[eg[,2] %in% names(table(eg[,2])[table(eg[,2])==2]),]
  beg <- cbind(beg[c(TRUE,FALSE),"StatID"], beg[c(FALSE,TRUE),"StatID"])
  beg <- cbind(actors[match(beg[,1], actors$StatID), c("Latitude","Longitude")],
               actors[match(beg[,2], actors$StatID), c("Latitude","Longitude")])
  beg <- sapply(beg, as.numeric)
  
  # Compile multilaterals
  meg <- eg[eg[,2] %in% names(table(eg[,2])[table(eg[,2])>2]),]
  meg <- cbind(actors[match(meg[,1], actors$StatID), c("Latitude","Longitude")],
               meg[,2])
  meg[,1] <- as.numeric(meg[,1])
  meg[,2] <- as.numeric(meg[,2])
  meg$Lat <- NA
  meg$Lon <- NA
  library(geosphere)
  for(multi in meg$`meg[, 2]`){
    temp <- meg[meg$`meg[, 2]` %in% multi,]
    meg[meg$`meg[, 2]` %in% multi, "Lat"] <- centroid(as.matrix(temp[, c(2,1)]))[2]
    meg[meg$`meg[, 2]` %in% multi, "Lon"] <- centroid(as.matrix(temp[, c(2,1)]))[1]
  }
  meg$`meg[, 2]` <- NULL
  meg <- as.matrix(meg)
  colnames(meg) <- c("Latitude", "Longitude", "Latitude", "Longitude")
  
  eg <- rbind(beg, meg)
  
  globejs(arcs=eg, 
          lat=unname(meg[,3]), long=unname(meg[,4]), color = "#ffff00",
          img = "inst/carte-geographique-du-monde.jpg",
          arcsColor=c(rep("#C3073F",nrow(beg)),rep("#3500D3",nrow(meg))),
          arcsHeight=0.5, arcsLwd=2, arcsOpacity=0.3,
          atmosphere=F, bg="#C2B9B0")
}
