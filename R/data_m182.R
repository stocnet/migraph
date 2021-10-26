#' Multiplex igraph of friends, social, and task ties between 16 anonymous students
#' 
#' M182 was an honors algebra class and friendship, social, and task ties
#' were collected/observed.
#' @docType data
#' @keywords datasets
#' @name m182
#' @usage data(ison_m182)
#' @source See also `data(studentnets.M182, package = "NetData")`
#' Larger comprehensive data set publicly available, contact Daniel A. McFarland for details.
#' @format Multiplex tidygraph of friends, social, and task ties between 16 anonymous students
#' The edge attribute `friend_ties` contains friendship ties,
#' where `2` = best friends, `1` = friend, and `0` is not a friend.
#' `social_ties` consists of social interactions per hour,
#' and `task_ties` consists of task interactions per hour.
"ison_m182"
