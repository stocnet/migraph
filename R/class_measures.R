make_node_measure <- function(out, .data) {
  class(out) <- c("node_measure", class(out))
  attr(out, "mode") <- manynet::node_mode(.data)
  out
}

make_tie_measure <- function(out, .data) {
  class(out) <- c("tie_measure", class(out))
  out
}

make_network_measure <- function(out, .data) {
  class(out) <- c("network_measure", class(out))
  attr(out, "mode") <- manynet::network_dims(.data)
  out
}

make_network_measures <- function(out, .data) {
  time <- value <- NULL
  out <- dplyr::as_tibble(out) %>% 
    dplyr::mutate(time = as.numeric(names(out))) %>% 
    dplyr::select(time, value)
  class(out) <- c("network_measures", class(out))
  attr(out, "mode") <- manynet::network_dims(.data)
  out
}

# Printing ####
#' @export
print.node_measure <- function(x, ...,
                          max.length = 6,
                          digits = 3) {
  if (any(attr(x, "mode"))) {
    for(m in c(FALSE, TRUE)){
      print_tblvec(y = as.numeric(x)[attr(x, "mode")==m], 
                   names = list(names(x)[attr(x, "mode")==m]))
      if(!m) cat("\n")
    }
  } else {
    print_tblvec(y = as.numeric(x), 
                 names = list(names(x)))
  }
}

#' @export
print.tie_measure <- function(x, ...,
                               max.length = 6,
                               digits = 3) {
  print_tblvec(y = as.numeric(x), 
               names = list(names(x)))
}

#' @export
print.network_measure <- function(x, ...,
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

# Plotting ####
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
    # if (type == "h") {
      p <- ggplot2::ggplot(data = data) +
        ggplot2::geom_histogram(ggplot2::aes(x = .data$Score),
                                binwidth = ifelse(max(data$Score) > 1, 1,
                                                  ifelse(max(data$Score) > .1,
                                                         .1,
                                                         .01))) +
    #     ggplot2::ylab("Frequency")
    # } else {
    #   p <- ggplot2::ggplot(data = data) +
        ggplot2::geom_density(ggplot2::aes(x = .data$Score), col = 2) +
        ggplot2::scale_y_continuous("Frequency", 
                                    sec.axis = ggplot2::sec_axis(~ ., breaks = c(0,1), name = "Density"))
    # }
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

#' @export
plot.network_measures <- function(x, ...) {
  ggplot2::ggplot(data = x, ggplot2::aes(x = .data$time, y = .data$value)) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::xlab("Time") +
    ggplot2::ylab("Value")
}
  

# make tblvec ####
print_tblvec <- function(y, names){
  mat <- matrix(y, dimnames = names)
  mat <- t(mat)
  out <- as.data.frame(mat)
  tibs <- dplyr::tibble(out, .name_repair = "unique")
  setup <- pillar::tbl_format_setup(tibs)
  body <- pillar::tbl_format_body(tibs, setup)[c(1,3)]
  if(setup$extra_cols_total > 0){
        print(body)
    cat(pillar::style_subtle(paste("# ... with",
                                   setup$extra_cols_total,
                                   "more from this nodeset in the vector.")))
      } else print(body)
}
