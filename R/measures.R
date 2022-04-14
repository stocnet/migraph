make_node_measure <- function(out, object){
  class(out) <- c("node_measure", class(out))
  attr(out, "mode") <- node_mode(object)
  out
}

make_graph_measure <- function(out, object){
  class(out) <- c("graph_measure", class(out))
  attr(out, "mode") <- graph_dims(object)
  out
}

#' @export
print.node_measure <- function(x, ..., 
                          max.length = 6,
                          digits = 3){
  if(any(attr(x, "mode"))){
    y <- x[attr(x, "mode")]
    y <- y[max(1, ((length(y)-max.length)+1)):length(y)]
    z <- x[!attr(x, "mode")]
    z <- z[1:min(length(z), max.length)]
    class(z) <- "numeric"
    z <- format(z, digits = digits)
    class(y) <- "numeric"
    y <- format(y, digits = digits)
    print(noquote(format(c(z, 
                           paste("+", length(x) - (length(z) + length(y)), 
                                 "others"), y))))
  } else {
    z <- x[1:min(length(x), max.length)]
    class(z) <- "numeric"
    z <- format(z, digits = digits)
    print(noquote(format(c(z, 
                           paste("+", length(x) - length(z), 
                                 "others")))))
  }
}

#' @export
print.graph_measure <- function(x, ..., 
                               digits = 3){
    if(length(attr(x, "mode")) == 1){
      print(as.numeric(x), digits = digits)
    } else {
      y <- as.numeric(x)
      names(y) <- paste("Mode", seq_len(length(attr(x, "mode"))))
      print(y, digits = digits)
    }
}

#' @export
plot.node_measure <- function(x, type = c("h", "d"), ...){
  type <- match.arg(type)
  if(is.null(attr(x, "mode"))) 
    attr(x, "mode") <- rep(FALSE, length(x))
  data <- data.frame(Score = x, Mode = attr(x, "mode"))
  if(any(attr(x, "mode"))){
    if(type == "h"){
      p <- ggplot2::ggplot(data = data) +
        ggplot2::geom_histogram(ggplot2::aes(x = .data$Score, 
                                             fill = .data$Mode),
                                binwidth = ifelse(max(data$Score) > 1, 1,
                                                  ifelse(max(data$Score) > .1,
                                                         .1,
                                                         .01))) +
        ggplot2::ylab("Frequency") + 
        ggplot2::scale_fill_grey(labels = c("1", "2"),
                                 start = 0.7, end = 0.4)
    } else {
      p <- ggplot2::ggplot(data = data) +
        ggplot2::geom_density(ggplot2::aes(x = .data$Score, 
                                           color = .data$Mode)) +
        ggplot2::ylab("Density") + 
        ggplot2::scale_color_grey(labels = c("1", "2"),
                                  start = 0.7, end = 0.4)
    }
  } else {
    if(type == "h"){
      p <- ggplot2::ggplot(data = data) +
        ggplot2::geom_histogram(ggplot2::aes(x = .data$Score),
                                binwidth = ifelse(max(data$Score) > 1, 1,
                                                  ifelse(max(data$Score) > .1,
                                                         .1,
                                                         .01))) +
        ggplot2::ylab("Frequency")
    } else {
      p <- ggplot2::ggplot(data = data) +
        ggplot2::geom_density(ggplot2::aes(x = .data$Score)) +
        ggplot2::ylab("Density")
    }
  }
  p + ggplot2::theme_classic() +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "grey90"))
}
