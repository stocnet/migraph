#' Two-mode clustering
#'
#' This function allows you to calculate how much two-mode clustering there is.
#' @param mat A matrix
#' @family two-mode functions
#' @export
#' @examples
#' twomode_clustering(matrix)
twomode_clustering <- function(mat){
  c <- ncol(mat)
  indegrees <- colSums(mat)
  twopaths <- crossprod(mat)
  diag(twopaths) <- 0
  cycle4 <- sum(twopaths * (twopaths - 1)) /
    (sum(twopaths * (twopaths - 1)) + sum(twopaths *
                                          (matrix(indegrees, c, c) - twopaths)))
  if(is.nan(cycle4)) cycle4 <- 1
  return(cycle4)
}

#' Three-mode clustering
#'
#' This function allows you to calculate how much three-mode clustering there is.
#' @param mat1 The first two-mode matrix
#' @param mat2 The second two-mode matrix
#' @family three-mode functions
#' @export
#' @examples
#' threemode_clustering(matrix)
threemode_clustering <- function(mat1, mat2){
  
  c <- ncol(mat1)

  twopaths1 <- crossprod(mat1)
  indegrees <- diag(twopaths1)
  diag(twopaths1) <- 0
  twopaths2 <- tcrossprod(mat2)
  outdegrees <- diag(twopaths2)
  diag(twopaths2) <- 0
  
  twopaths <- twopaths1 + twopaths2
  degrees <- indegrees + outdegrees
  
  cycle4 <- sum(twopaths * (twopaths - 1)) /
    (sum(twopaths * (twopaths - 1)) + sum(twopaths *
                                            (matrix(degrees, c, c) - twopaths)))
  if(is.nan(cycle4)) cycle4 <- 1
  return(cycle4)
}

#' Two-mode small-world
#' 
#' Calculates small-world metrics for two-mode networks
#' @param mat A matrix
#' @param n Number of simulated 
#' @family two-mode functions
#' @return Returns a table of small-world related metrics for each second-mode node.
#' @details The first column of the returned table is simply the number of the second-mode column.
#' The next three columns report the observed and expected clustering, 
#' and the ratio of the former to the later.
#' The next three columns report the observed and expected path-length,
#' and the ratio of the former to the later.
#' The last column reports the ratio of the observed/expected clustering ratio
#' to the observed/expected path-length ratio, which is known as a small-world metric.
#' Expected clustering and paths is the mean of twomode_clustering and mean_distance
#' over 100 random simulations with the same row and column sums.
#' @examples twomode_smallworld(mat)
#' @seealso \code{\link{twomode_clustering}} for how clustering is calculated
#' @import igraph
#' @export 
twomode_smallworld <- function(mat, n=100){
  require(igraph)
  out <- matrix(NA, ncol(mat), 7)
  for(c in 2:ncol(mat)){
    m <- mat[, 1:c]
    g <- graph_from_incidence_matrix(m)
    out[c, 1] <- twomode_clustering(m)
    out[c, 4] <- mean_distance(g)

    r <- r2dtable(n, rowSums(m), colSums(m))
    out[c, 2] <- mean(unlist(lapply(r, twomode_clustering)))
    out[c, 5] <- mean(unlist(lapply(lapply(r, graph_from_incidence_matrix),
                                  mean_distance)))

    out[c, 3] <- out[c, 1] / out[c, 2]
    out[c, 6] <- out[c, 4] / out[c, 5]
    out[c, 7] <- out[c, 3] / out[c, 6]
  }
  out <- cbind(1:ncol(mat), out)
  out <- as.data.frame(out)
  names(out) <- c("Num", "ObsClust", "ExpClust", "ClustRat",
                             "ObsPath", "ExpPath", "PathRat", "SmallWorld")
  out
}

# The following functions were previously named "BBCentralization" and "BDCentralization"
# from a BBCentralization.R script.

#' Two-mode dominance
#'
#' This function allows you to calculate how (degree) centralized a two-mode graph is.
#' @param mat An affiliation or incidence matrix. For centralization around rows, simply transpose the matrix first (\code{t()})
#' @param attr Optionally, an attribute vector.
#' @family two-mode functions
#' @export
#' @examples
#' twomode_dominance(mat)
#' twomode_dominance(mat, attr = gdp2010)
twomode_dominance <- function(mat, attr = NULL){

  # Get dimensions
  n <- nrow(mat)
  m <- ncol(mat)

  # If attribute absent, use 1s
  if (is.null(attr)) attr <- rep(1, n)
  # If attribute missing, use 0s
  attr[is.na(attr)] <- 0

  # Get distributions
  msum <- colSums(mat * attr, na.rm = T)

  if(m > 1){
    out <- sum(max(msum) - msum) / (sum(attr, na.rm = T) * (m-1))
  } else {
    out <- msum / sum(attr, na.rm = T)
  }
  
  return(out)
}

twomode_dominance_bilatbase <- function(mat, attr = NULL){

  # Get dimensions
  n <- nrow(mat)
  m <- ncol(mat)

  # If attribute absent, use 1s
  if (is.null(attr)) attr <- rep(1, n)
  # If attribute missing, use 0s
  attr[is.na(attr)] <- 0

  # Get distributions
  msum <- colSums(mat*attr, na.rm = T)
  
  if(m > 1){
    out <- sum(max(msum)-msum) / ((sum(attr, na.rm = T) - 2)*(m-1))
  } else {
    out <- msum / sum(attr, na.rm = T)
  }
  
  return(out)
}

#' Two-mode degree centralization
#'
#' This function allows you to calculate how (degree) centralized a two-mode graph is.
#' @param graph An igraph graph
#' @references Borgatti, Stephen P, and Daniel S Halgin. 2011. ``Analyzing Affiliation Networks." In The SAGE Handbook of 
#' Social Network Analysis, edited by John Scott and Peter J Carrington, 417–33. London, UK: Sage.
# #' @family two-mode functions
#' @export
#' @examples
#' twomode_centralization_degree(graph)
twomode_centralization_degree <- function(graph){
  require(igraph)
  nodeset <- names(which(igraph::degree(graph)==max(igraph::degree(graph)))) %in%
    V(graph)$name[V(graph)$type==T]
  m <- length(which(V(graph)$type==T))
  n <- length(which(V(graph)$type!=T))

  sum(max(igraph::degree(graph)[which(V(graph)$type==nodeset)], na.rm=T)-
        igraph::degree(graph)[which(V(graph)$type==nodeset)])/
    ((n-1)*(m-1))
}

#' Two-mode degree centrality
#'
#' @param graph 
#' @param v 
#' @param mode 
#' @param twomode 
#'
#' @return
#' @export
#'
#' @examples
twomode_centrality_degree <- function (graph, v = V(graph), mode = c("all"), twomode = TRUE) 
{
  require(igraph)
  v <- as.igraph.vs(graph, v)
  mode <- igraph.match.arg(mode)
  on.exit(.Call(C_R_igraph_finalizer))
  res <- .Call(C_R_igraph_degree, graph, v - 1, as.numeric(mode), 
               as.logical(loops))
  if (twomode) {
    bipartite.mapping(graph)
    n <-attr(whichV(graph)$type=T)
    m <- attr(whichV(graph)$type!=T)
    resn <- res/(vcount(whichV(graph)$type!=T))
    resm <- res/(vcount(whichV(graph)$type=T))
    # Add them together (?)
    res <- list(resn, resm)
  }
  if (igraph_opt("add.vertex.names") && is_named(graph)) {
    names(res) <- V(graph)$name[v]
  }
  res
}
# Only the first attempt, bear with me here...

#' Two-mode betweenness centralization
#'
#' This function allows you to calculate how (betweenness) centralized a two-mode graph is.
#' @param graph An igraph graph
# #' @family two-mode functions
#' @export
#' @examples
#' twomode_centralization_between(graph)
twomode_centralization_between <- function(graph){
  require(igraph)
  nodeset <- names(which(betweenness(graph)==max(betweenness(graph)))) %in% 
    V(graph)$name[V(graph)$type==T]
  m <- length(which(V(graph)$type==nodeset))
  n <- length(which(V(graph)$type!=nodeset))
  # p <- (m-1)%/%n
  # r <- (m-1)%%n
  # s <- (n-1)%/%m
  # t <- (n-1)%%m
  # 
  # Event side centralization
  # sum(max(betweenness(graph)[which(V(graph)$type==nodeset)])-
  #       betweenness(graph)[which(V(graph)$type==nodeset)])/
  #   ((m-1)*(n^2*(p+1)^2 + n*(p+1)*(2*r-p-1)-r*(2*p-r+3)))/2
  if (m > n){
    sum(max(betweenness(graph)[which(V(graph)$type==nodeset)], na.rm=T)-
          betweenness(graph)[which(V(graph)$type==nodeset)])/
      (2*(m-1)^2*(n-1))
  }
  if (m <= n){
    sum(max(betweenness(graph)[which(V(graph)$type==nodeset)], na.rm=T)-
          betweenness(graph)[which(V(graph)$type==nodeset)])/
      ((m-1)*((1/2)*n*(n-1)+(1/2)*(m-1)*(m-2)+(m-1)*(n-1)))
  }
  
  # Whole network centralization
  # bnorm <- c(betweenness(graph)[which(V(graph)$type!=nodeset)]/
  #              ((1/2)*(m^2*(s+1)^2+m*(s+1)*(2*t-s-1)-t*(2*s-t+3))),
  #            betweenness(graph)[which(V(graph)$type==nodeset)]/
  #              ((1/2)*(n^2*(p+1)^2+n*(p+1)*(2*r-p-1)-r*(2*p-r+3))))
  # sum(max(bnorm)-bnorm)/
  #   ((m+n-1) - ((p*(n-r)*(2*m+2*n-p-3) + r*(p+1)*(2*m+2*n-p-4))/
  #                (m^2*(s+1)^2+m*(s+1)*(2*t-s-1)-t*(2*s-t+3))))
  
  #   if (m > n){
  #     sum(max(betweenness(graph))-betweenness(graph))/
  #       (2*(m-1)*(n-1)*(m+n-1) - (n-1)*(m+n-2) - (1/2)*(m-n)*(m+3*n-3))
  #   }
  #   if (m <= n){
  #     sum(max(betweenness(graph))-betweenness(graph))/
  #       (((1/2)*n*(n-1)+(1/2)*(m-1)*(m-2)+(m-1)*(n-2)) * ((m+n-1)+(m-1)))
  #   }
}

#' Two-mode betweeness centrality
#'
#' @param graph 
#' @param v 
#' @param weights 
#' @param nobigint 
#' @param twomode 
#'
#' @return
#' @export
#'
#' @examples
twomode_centrality_betweeness <- function (graph, v = V(graph), weights = NULL, 
          nobigint = TRUE, twomode = TRUE) {
  twomode_centrality_betweeness 
  v <- as.igraph.vs(graph, v)
  if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
    weights <- E(graph)$weight
  }
  if (!is.null(weights) && any(!is.na(weights))) {
    weights <- as.numeric(weights)
  }
  else {
    weights <- NULL
  }
  on.exit(.Call(C_R_igraph_finalizer))
  res <- .Call(C_R_igraph_betweenness, graph, v - 1, as.logical(directed), 
               weights, as.logical(nobigint))
  if (twomode) {
    bipartite.mapping(graph)
    vcn <- vcount(whichV(graph)$type=T)
    vcm <- vcount(whichV(graph)$type!=T)
    resn <- (1/2*vcn)*(vcn-1) + 1/2*((vcm - 1)*(vcm - 2)) + (vcm - 1)*(vcn - 1)
    resm <- (1/2*vcm)*(vcm-1) + 1/2*((vcn - 1)*(vcn - 2)) + (vcn - 1)*(vcm - 1)
    #Need to add them together now (?)
    res <- list(resn, resm)
    }
  if (igraph_opt("add.vertex.names") && is_named(graph)) {
    names(res) <- V(graph)$name[v]
  }
  res
}

# Only the first attempt, please bear with me here once again ... 


# twomode_centrality_closeness <- 
# math: Ni(size of other nodes set) + 2*(No(set of nodes own vertices)) - 2 
# Now how to do centralization for closenes?

# twomode_centrality_eigenvector <- 
# math: square root of (1/(2*(No))) 
# Is this it? 

#'
#' Two-mode constraint
#' 
#' This function extends Ronald Burt's constraint measure to two-mode networks.
#' @param mat A matrix
#' @return Constraint scores for each second-mode node
#' @details Note that this function returns constraint scores
#' for each second-mode node by default. To return constraint scores
#' for each first-mode node, please pass the function the transpose of the matrix. 
#' See Ron Burt's work on structural holes for more details.
#' @family two-mode functions
#' @examples twomode_constraint(mat)
#' @export 
twomode_constraint <- function(mat){
    inst <- colnames(mat)
    rowp <- mat * matrix(1/rowSums(mat), nrow(mat), ncol(mat))
    colp <- mat * matrix(1/colSums(mat), nrow(mat), ncol(mat), byrow = T)
    res <- vector()
    for (i in inst){
      ci <- 0
      membs <- names(which(mat[,i]==1))
      for (a in membs){
        pia <- colp[a,i]
        oth <- membs[membs!=a]
        pbj <- 0
        if (length(oth)==1){
          for (j in inst[mat[oth,]>0 & inst!=i]){
            pbj <- sum(pbj, sum(colp[oth,i] * rowp[oth,j] * colp[a,j]))
          }
        } else {
          for (j in inst[colSums(mat[oth,])>0 & inst!=i]){
            pbj <- sum(pbj, sum(colp[oth,i] * rowp[oth,j] * colp[a,j]))
          }
        }
        cia <- (pia + pbj)^2
        ci <- sum(ci, cia)
      }
      res <- rbind(res, c(i,round(ci*100)))
    }
    return(res)
  }

#' Two-mode components
#' 
#' This function identifies components in a two-mode network.
#' @param mat A matrix
#' @return A list including the number of components in the network,
#' the fragmentation of the network (number of components/number of nodes in the column nodeset),
#' and the component membership of each node in that nodeset.
#' @details Note that this function applies only to one dimension/mode (the columns).
#' Use a transposed matrix to return values for the other dimension/mode.
#' @family two-mode functions
#' @examples 
#' twomode_components(mat)
#' @export 
twomode_components <- function(mat){
  # components - how many institutional fragments do we have?
  if(is.matrix(mat)){
    m <- ncol(mat)
    } else {
    m <- 1
  }

  twopaths <- crossprod(mat)
  twopaths[lower.tri(twopaths)] <- 0
  diag(twopaths) <- 1
  connect <- which(twopaths > 0, arr.ind = T)
  for (i in 1:m){
    if (sum(connect==i)*1>2) connect <- connect[-which(connect[,1]==i & connect[,2]==i),]
  }

  memb <- data.frame(node=1:m, comp=NA)
  memb[1,2] <- comp <- 1
  if (nrow(memb)==2 & !is.matrix(connect) & all(connect==c(1,2))) {
    memb[2,2] <- 1
  } else {
    while (anyNA(memb[,2])){
      if(anyNA(memb[connect[connect[, 1] %in% which(memb[, 2] == comp), 2], 2])){
        memb[connect[connect[, 1] %in% which(memb[, 2] == comp), 2], 2] <- comp
      } else {
        memb[which(is.na(memb[,2]))[1],2] <- comp <- max(memb[,2], na.rm = T) + 1
      }
      
    }
  }
  
  return(list(components=max(memb[,2]), 
              fragmentation=max(memb[,2])/m, 
              membership=memb))
}

#' Two-mode coherence
#' 
#' This function calculates coherence for a two-mode network
#' @param mat A matrix
#' @return Average coherence across the components of the network
#' @details Note that this function applies only to one dimension/mode (the columns).
#' Use a transposed matrix to return values for the other dimension/mode.
#' @family two-mode functions
#' @seealso twomode_fragmentation
#' @examples 
#' twomode_coherence(mat, attr)
#' @export 
twomode_coherence <- function(mat, attr=NULL){
  
  if(is.null(attr) & is.matrix(mat)) attr <- rep(1,ncol(mat))
  if(is.null(attr) & !is.matrix(mat)) attr <- 1
  
  # Get components
  frag <- twomode_components(mat)
  comps <- lapply(1:frag$components, function(c) mat[, frag$membership$node[frag$membership$comp==c]])
  size <- sapply(1:frag$components, function(x) (sum(frag$membership$comp==x)/max(frag$membership$node)) )
  atts <- lapply(1:frag$components, function(c) attr[frag$membership$comp==c])
  
  # Jaccard function
  jaccard <- function(M, user1, user2) {
    sums = rowSums(M[,c(user1, user2)])
    
    similarity = length(sums[sums==2])
    total = length(sums[sums==1]) + similarity
    
    similarity/total
  }
  
  # Get mean similarity within each component
  sim <- mapply(function(comps, atts) if(is.matrix(comps)){
    mean( # Average sim across all institution pairs
      sapply(combn(ncol(comps),2, simplify = F), # For each institution pair
             function(x) jaccard(comps, x[1], x[2]) * # Membership similarity [0,1]
               ((((atts[x[1]]==atts[x[2]])*1)-.5)*2) # Mandate similarity [-1,1]
      ) 
    )
  } else {
    1 # No other institutions in component, must be coherent
  }, comps, atts)
  
  sim <- (sum(sim * size)+1)/2
  return(sim)
}

#' Two-mode two-by-two analysis
#' 
#' This function calculates coherence and dominance for two-mode networks (and attributes)
#' extracted for each year between the given dates
#' @param node1 First nodeset object (will become matrix rows)
#' @param node2 Second nodeset object (will become matrix columns)
#' @param ties Edgelist of affiliations/memberships of first nodeset in second nodeset
#' @param attr1 Object containing information on an attribute by year, in long format
#' @param attr2 Object containing information on an attribute by year, in long format
#' @param start Integer, for example a year like 1960
#' @param end Integer, for example a year like 2010
#' @return A data frame with two columns (Coherence and Dominance)
#' @details Note that this function has defaults that make sense for use with 
#' the gnevar datasets. Please contact me if you would like to make this function
#' more general.
#' @family two-mode functions
#' @seealso twomode_clustering
#' @seealso twomode_dominance
#' @examples 
#' \dontrun{
#' library(gnevar)
#' library(wbstats)
#' mil_data <- wb(country = unique(stat_actor$StatID), indicator = "MS.MIL.XPND.CN", startdate = 1960, enddate = 2018)
#' ally_secs <- structure((!is.na(ally_agree$Secretariat))*1, names=as.character(ally_agree$AtopID))
#' ally_topo <- twomode_2x2(stat_actor, ally_agree, ally_membs, mil_data, ally_secs, 1960, 2018)
#' plot_2x2(ally_topo)
#' }
#' @export 
twomode_2x2 <- function(node1, node2, ties, attr1, attr2, start, end){
  require(gnevar)
  
  # dat <- lapply(paste(start:end,"-01-01",sep=""), function(t)  as.matrix(gnevar::snap(node1, node2, ties, time=t)) )
  # Just for while there are data issues:
  dat <- lapply(paste(start:end,"-01-01",sep=""), function(t)  as.matrix(gnevar::snap(node1, node2, ties, time=t)[,colSums(gnevar::snap(node1, node2, ties, time=t))>1] ))
  dat <- lapply(dat, function(dat) (dat>0)*1 )
  
  attr1 <- mapply(function(t, dat)      structure(attr1[attr1$date==t & attr1$iso3c %in% rownames(dat),"value"], 
                                                 names=attr1[attr1$date==t & attr1$iso3c %in% rownames(dat),"iso3c"]),
                 as.character(start:end), dat)
  attr1 <- mapply(function(attr1, dat) attr1[match(rownames(dat),names(attr1))], attr1, dat)

  # attr2 <- mapply(function(t, dat)      structure(attr2[attr2$date==t & attr2$iso3c %in% rownames(dat),"value"], 
  #                                                 names=attr2[attr2$date==t & attr2$iso3c %in% rownames(dat),"iso3c"]),
  #                 as.character(start:end), dat)
  attr2 <- lapply(dat, function(x) attr2[match(colnames(dat),names(attr2))])
  # attr2 <- mapply(function(attr2, dat) attr2[match(colnames(dat),names(attr2))], attr2, dat)
  
  out <- mapply(function(dat, attr1, attr2, t) c(Coherence=twomode_modularity(dat, attr2),
                          Dominance=twomode_dominance(dat, attr1),
                          Year=t),
         dat, attr1, attr2, t=start:end)
  
  return(as.data.frame(t(out)))
}


#' Two-mode modularity
#' 
#' This function calculates modularity of two-mode networks.
#' A vector of group assignment can be given for one of the two
#' node sets which will make assignment more rapid. 
#' @param mat A matrix
#' @param attr A vector of group assignment for the second mode node set (by default)
#' @return A modularity score
#' @details Note that this function has defaults that make sense for use with 
#' the gnevar datasets. Please contact me if you would like to make this function
#' more general.
#' @family two-mode functions
#' @seealso twomode_coherence
#' @seealso twomode_dominance
#' @examples 
#' \dontrun{
#' twomode_modularity(mat, attr)
#' }
#' @export 
twomode_modularity <- function(mat,attr=NULL){#,attr2=NULL
  
  # Start with C
  if(is.null(attr) | all(attr==1)){
    C <- as.matrix(table(1:ncol(mat),1:ncol(mat)))
  } else {
    C <- as.matrix(table(1:ncol(mat),attr))
  }
  
  # Get expected
  M <- sum(mat)
  E <- (matrix(rowSums(mat),nrow(mat),ncol(mat), byrow = F) * 
          matrix(colSums(mat),nrow(mat),ncol(mat), byrow = T)) / M
  B <- (mat-E)
  
  # Assign Rs
  R <- B%*%C
  R <- apply(R, 1, function(x){
    if (length(which(x==max(x))) == 1) {
      return(which(x==max(x)))
    } else {
      return(sample(which(x==max(x)), 1))
    }
  })
  R <- as.matrix(table(R,1:nrow(mat)))
  
  # Calculate Q
  Q <- sum(diag(R%*%B%*%C)) * (1/(M/2)) # Get modularity score from trace
  
  # If no attr, assign Cs
  if(is.null(attr) | all(attr==1)){
    repeat{
    C <- R%*%B
    C <- apply(C, 2, function(x){
      if (length(which(x==max(x))) == 1) {
        return(which(x==max(x)))
      } else {
        return(sample(which(x==max(x)), 1))
      }
    })
    C <- as.matrix(table(1:ncol(mat),C))
    
    R <- B%*%C
    R <- apply(R, 1, function(x){
      if (length(which(x==max(x))) == 1) {
        return(which(x==max(x)))
      } else {
        return(sample(which(x==max(x)), 1))
      }
    })
    R <- as.matrix(table(R,1:nrow(mat)))
    
    if(sum(diag(R%*%B%*%C)) * (1/M)==Q){
      break
    } else {
      Q <- sum(diag(R%*%B%*%C)) * (1/M)
    }
    }
  }
  
  # Q <- Q / (sum(diag(R%*%(M-E)%*%C)) * (1/M))
  Q <- (Q+1)/2
  # Qmax <- (sum(diag(R%*%(M-E)%*%C)) * (1/M)) #Normalise by max poss 
  # for given degree distribution
  # Qmax <- (Qmax+1)/2
  # Q <- Q/Qmax
  Q
}
