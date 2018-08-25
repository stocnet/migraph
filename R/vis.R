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
#' plot_twomode(mat)
#' }
#' @import igraph
#' @export 
plot_twomode <- function(mat, attr, greyscale=T, ...){
  
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
#' ally_topo <- twomode_2x2(stat_actor, ally_agree, ally_membs, mil_data, 1960, 2018)
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

