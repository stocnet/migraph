# #' @param edgelists A list of edgelists to be turned into a matrix
# mm_matrix <- function(edgelists){
#   
#   edgelists
#   
#   out <- lapply(edgelists, function(x) {
#     out <- table(x[,1], x[,2])
#     out <- matrix(c(out), length(unique(x[,1])), length(unique(x[,2])),
#                   dimnames = list(unique(x[,1]), unique(x[,2])))
#   })
#   out <- abind::abind(out, use.dnns = T, make.names = T)
#   out
#   
#   node1 <- unique(edgelists[[1]][,1])
#   
#   length(edgelists)
#   
#   sapply(edgelists, function(x) trimws(apply(x, 2, unique)))
#   table(edgelists)
#   unlist(edgelists)
#   apply(edgelists[[1]], 2, unique)
  
#'   
#'   if(is.list(edgelists)) edgelists <- dplyr::bind_rows(edgelists)
#'   
#'   do.call(lapply(edgelists, function(x) {
#'     table(x[,1], x[,2])
#'   }), rbind)
#'   
#'   
#'   do.call(plyr::rbind.fill, test)
#'   
#'   data.table::rbindlist(lapply(edgelists, function(x) {
#'     as.data.frame(table(x[,1], x[,2]))
#'   }))
#'   
#'   test <- plyr::rbind.fill(lapply(edgelists, function(x) {
#'     as.data.frame(table(x[,1], x[,2]), stringsAsFactors = F)
#'   }))
#'   
#'   mat <- matrix(0,length(unique(c(test[,1], test[,2]))), length(unique(c(test[,1], test[,2]))))
#'   rownames(mat) <- colnames(mat) <- unique(c(test[,1], test[,2]))
#'   mat[match(test[test[,3]>0,1],rownames(mat)), match(test[test[,3]>0,2],colnames(mat))] <- 1
#'   
#'   mat[match(test[,1],rownames(mat)), match(test[,2],colnames(mat))] <- test[,3]
#'   mat[1,11]
#'   
#'   
#'   
#'   
#'   data <- list(data.frame(numbers=1:10,
#'                           letters=letters[1:10]),
#'                data.frame(letters=letters[1:10],
#'                           colours=rainbow(10)))
#'   
#'   Reduce(rbind, test)
#'   
#'   as.matrix(Matrix::bdiag(test))
#'   
#'   test[[1]] %+% test[[2]]
#'   

#' Transform a two-mode event list into a one-mode event list
#' 
#' This function relies on `tidyverse` functions to quickly and efficiently
#' project a two-mode event list into a one-mode event list of nodes from 
#' the first mode that share connections to nodes in the second mode.
#' @param eventlist a two-mode event list
#' @return a one-mode event list
#' @import tidyverse
#' @export
project_list <- function(eventlist){
  el <- eventlist %>% group_by(receiver, time) %>%
    summarise(nodes = paste(sender, collapse = "_")) %>%
    ungroup() %>% select(-receiver)
  out <- do.call(rbind, 
                 apply(el, 1, 
                       function(x) cbind(t(combn(strsplit(x[2], "_")$nodes,2)), x[1], 1) )) %>%
    as.data.frame(., stringsAsFactors = F)
  names(out) <- c("sender", "receiver", "time", "increment")
  out <- out %>% mutate(time = as.POSIXct(time), increment = as.numeric(increment))
  out
}
