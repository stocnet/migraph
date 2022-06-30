make_node_measure <- function(out, object) {
  class(out) <- c("node_measure", class(out))
  attr(out, "mode") <- node_mode(object)
  out
}

make_tie_measure <- function(out, object) {
  class(out) <- c("tie_measure", class(out))
  out
}

make_graph_measure <- function(out, object) {
  class(out) <- c("graph_measure", class(out))
  attr(out, "mode") <- graph_dims(object)
  out
}

#' @export
print.node_measure <- function(x, ...,
                          max.length = 6,
                          digits = 3) {
  if (any(attr(x, "mode"))) {
    for(m in c(FALSE, TRUE)){
      names <- list(names(x)[attr(x, "mode")==m])
      mat <- matrix(as.numeric(x)[attr(x, "mode")==m], 
                    dimnames = names)
      mat <- mat[order(mat[,1], decreasing = TRUE),] #rank scores
      mat <- t(mat)
      out <- as.data.frame(mat)
      print(dplyr::tibble(out, .name_repair = "unique"))
      # o <- capture.output(print(dplyr::tibble(out, .name_repair = "unique")))
      # o <- o[!grepl('^ +<...>', o)]
      # o[1] <- "Centrality scores by node"
      # m <- gregexpr('^ *\\d+', o)
      # regmatches(o, m) <- ' '
      # cli::cat_line(o)
    }
    # y <- x[attr(x, "mode")]
    # y <- y[max(1, ((length(y) - max.length) + 1)):length(y)]
    # z <- x[!attr(x, "mode")]
    # z <- z[1:min(length(z), max.length)]
    # class(z) <- "numeric"
    # z <- format(z, digits = digits)
    # class(y) <- "numeric"
    # y <- format(y, digits = digits)
    # print(noquote(format(c(z,
    #                        paste("+", length(x) - (length(z) + length(y)),
    #                              "others"), y))))
  } else {
    names <- list(names(x))
    y <- as.numeric(x)
    mat <- matrix(y, dimnames = names)
    mat <- mat[order(mat[,1], decreasing = TRUE),] #rank scores
    mat <- t(mat)
    out <- as.data.frame(mat)
    tibs <- dplyr::tibble(out, .name_repair = "unique")
    class(tibs) <- c("tblvec", class(tibs))
    tbl_sum.tblvec <- function(x, ...){
      NULL
    }
    ctl_new_pillar.tblvec <- function(controller, x, width, ..., title = NULL){
      out <- NextMethod()
      pillar::new_pillar(list(
        title = out$title,
        data = out$data
      ))
    }
    tbl_format_footer.tblvec <- function(x, setup, ...) {
      if(setup$extra_cols_total > 0){
        pillar::style_subtle(paste("# ... with", setup$extra_cols_total, "more in the vector."))  
      }
    }
    print(tibs)
  }
}

#' @export
print.tie_measure <- function(x, ...,
                               max.length = 6,
                               digits = 3) {
    z <- x[1:min(length(x), max.length)]
    class(z) <- "numeric"
    z <- format(z, digits = digits)
    print(noquote(format(c(z,
                           paste("+", length(x) - length(z), 
                                 "others")))))
}

#' @export
print.graph_measure <- function(x, ...,
                               digits = 3) {
    if (length(attr(x, "mode")) == 1) {
      print(as.numeric(x), digits = digits)
    } else {
      y <- as.numeric(x)
      if (length(y) == 2)
        names(y) <- paste("Mode", seq_len(length(attr(x, "mode"))))
      print(y, digits = digits)
    }
}

#' @export
plot.node_measure <- function(x, type = c("h", "d"), ...) {
  type <- match.arg(type)
  if (is.null(attr(x, "mode")))
    attr(x, "mode") <- rep(FALSE, length(x))
  data <- data.frame(Score = x, Mode = attr(x, "mode"))
  if (any(attr(x, "mode"))) {
    if (type == "h") {
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
    if (type == "h") {
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

#' @export
plot.tie_measure <- function(x, type = c("h", "d"), ...) {
  type <- match.arg(type)
  data <- data.frame(Score = x)
  if (type == "h") {
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
  p + ggplot2::theme_classic() +
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "grey90"))
}

# @param FUN A function by which the values should be aggregated
# or summarised. By default `mean`.
# summary(node_degree(mpn_elite_mex),
#         membership = node_structural_equivalence(mpn_elite_mex, select = "elbow"))
#' @export
summary.node_measure <- function(object, ...,
                                 membership,
                                 FUN = mean) {
  out <- vapply(unique(membership),
                function(x) FUN(object[membership == x]), FUN.VALUE = 1)
  names(out) <- unique(membership)
  out
}
