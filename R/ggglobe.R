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
#' @name maps
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

#' @rdname maps
#' @export
ggatlas <- function(){
  library(maps)
  library(geosphere)
  
  #par(mfrow=c(3,3))
  
  year=seq(1920,2010,by=1) 
  
  for (t in year)
  {
    
    s=bilate$Signatures #evaluate which agreement is vaild for that decade
    e=bilate$Withdrawal1
    i=which((s<=t+1 & is.na(e))|(s>=t & e<=t+1)|(s<=t & e>=t+1)|(s<=t & e<=t+1 & e>=t)|(s>=t & s<=t+1 & e>=t+1)) #whether the vaild period of the agreement overlap with that decade
    #valid agreements in that period
    
    ss=statenodes$Start  #similar way to evaluate which countries existed in that decade
    se=statenodes$End
    su=statenodes$UnSov
    sr=statenodes$ReSov
    
    c=which( ((su>=t | sr<=t+1)|(is.na(su))) & ((ss>=t & se<=t+1)|(ss<=t & se>=t+1)|(ss<=t & se<=t+1 & se>=t)|(ss>=t & ss<=t+1 & se>=t+1))) #number of existed countries in that decade
    os=statenodes[c,][order(statenodes[c,]$Id),] #ordered state nodes
    countries=data.frame(id=os$Id) #id of existed countries
    el<-data.frame(from=bilate[i,]$Country.x,to=bilate[i,]$Country.y)  #edge list for vaild agreement
    
    #delete the agreements with unexisted countries
    i=1
    while(i<=nrow(el)){
      from=which(countries==as.vector(unlist(el[i,][1])))
      to=which(countries==as.vector(unlist(el[i,][1])))
      if(length(from)==0|length(to)==0){
        el<-el[-i,]  
      }else{
        i=i+1
      }
      
    }
    
    fish.network<-graph.data.frame(el,directed=F,vertices=countries) #use the edge list to output graph data frame
    par(mar=c(2,2,2,2))
    co=cbind(statenodes[c,]$Longitude,statenodes[c,]$Latitude)
    rownames(co)=statenodes[c,]$Id
    colnames(co)=c("Longitude","Latitude")
    
    png(sprintf("movie/image%04d.png",t),width=5000, height=2500, res=200)
    
    
    map("world", fill=TRUE, col="darkgrey",mar=c(1,1,1,1),lwd=0.005,border=NA,resolution=0,ylim=c(-90,100))
    if(nrow(el)>0){
      for (i in 1:nrow(el)){
        a<-co[as.vector(unlist(el[i,][1])),]
        b<-co[as.vector(unlist(el[i,][2])),]
        inter <- gcIntermediate(a,b,n=100, addStartEnd=TRUE,breakAtDateLine=TRUE)
        if(is.list(inter)){
          inter1 <- inter[[1]] 
          inter2 <- inter[[2]]
          lines(inter1,col=rgb(red=0.2,green=0.2,blue=0.8,alpha=0.2),lwd = 6)
          lines(inter2,col=rgb(red=0.2,green=0.2,blue=0.8,alpha=0.2),lwd = 6)
        }else{
          lines(inter, col=rgb(red=0.2,green=0.2,blue=0.8,alpha=0.2), lwd=6) 
        }
      }
    }
    
    #plot.igraph(fish.network,vertex.size=5,vertex.label=NA,layout=co)
    pp=paste("",t,sep='')
    #pt=paste("",t+1,sep='')
    mtext(pp,cex=5,side=1)
    dev.off()
  }
}
