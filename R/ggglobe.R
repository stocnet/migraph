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
#' ggglobe(mat)
#' }
#' @import threejs
#' @import geosphere
#' @export 
ggglobe <- function(edges, actors, time){
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
