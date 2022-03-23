make_measure <- function(out, object){
  class(out) <- c("measure", class(out))
  attr(out, "mode") <- node_mode(object)
  out
}

#' @export
print.measure <- function(x, ..., 
                          max.length = 6,
                          digits = 3){
  if(any(attr(x, "mode"))){
    y <- x[attr(x, "mode")]
    y <- y[max(1, ((length(y)-max.length)+1)):length(y)]
    x <- x[!attr(x, "mode")]
    x <- x[1:min(length(x), max.length)]
    class(x) <- "numeric"
    x <- format(x, digits = digits)
    class(y) <- "numeric"
    y <- format(y, digits = digits)
    print(noquote(format(c(x, "...", y))))
  } else {
    x <- x[1:min(length(x), max.length)]
    class(x) <- "numeric"
    x <- format(x, digits = digits)
    print(noquote(format(c(x, "..."))))
  }
}

#' @export
plot.measure <- function(x, ..., 
                         method = c("hist", "dens")){
  method <- match.arg(method)
  data <- data.frame(Score = x, Mode = attr(x, "mode"))
  if(any(attr(x, "mode"))){
    if(method == "hist"){
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
    if(method == "hist"){
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
