#' Plotting of one-mode and two-mode graphs
#' @param x A migraph-compatible object, especially an igraph graph object.
#' @param ... Additional arguments passed on to igraph.
#' @family plotting
#' @examples 
#' mat1 <- create_ring(5,10)
#' plot(mat1)
#' @export
plot.igraph <- function(x, ...){
  object <- as_igraph(x)
  if(is_bipartite(object)){
    igraph::V(object)$color <- ifelse(igraph::V(object)$type, "black", "grey")
    igraph::V(object)$shape <- ifelse(igraph::V(object)$type, "square", "circle")
    igraph::V(object)$label <- NA
    lo <- igraph::layout_as_bipartite(object)
    lo[,2] <- abs(lo[,2]-1)
    igraph::plot.igraph(object, layout = lo, ...)
  } else {
    igraph::V(object)$color <- "white"
      lo <- igraph::layout_nicely(object)
      if(nrow(lo)==2){
        lo[1,] <- c(0,0)
        lo[2,] <- c(1,0)
      } 
      if(nrow(lo)==3){
        lo[1,] <- c(0,0)
        lo[2,] <- c(1,0)
        lo[3,] <- c(.5,.866)
      } 
      if(nrow(lo)==4){
        lo[1,] <- c(0,0)
        lo[2,] <- c(1,0)
        lo[3,] <- c(0,1)
        lo[4,] <- c(1,1)
      } 
    igraph::plot.igraph(object, layout = lo, ...)
  }
}

#' ggplot2-based plotting of blockmodel results
#' @param x A blockmodel-class object.
#' @param ... Additional arguments passed on to ggplot2.
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot geom_tile aes scale_fill_gradient theme_grey labs theme scale_x_discrete scale_y_discrete geom_vline geom_hline element_blank element_text
#' @importFrom rlang .data
#' @examples 
#' usa_concor <- blockmodel_concor(mpn_elite_usa_advice)
#' plot(usa_concor)
#' @export
plot.blockmodel <- function(x, ...){
  
  plot_data <- x$blocked.data
  plot_data <- as.data.frame(plot_data) %>%
    tibble::rownames_to_column("Var1") %>%
    tidyr::pivot_longer(!.data$Var1, names_to = "Var2", values_to = "value")
  
  g <- ggplot2::ggplot(plot_data, ggplot2::aes(.data$Var2, .data$Var1)) + 
    ggplot2::geom_tile(ggplot2::aes(fill = .data$value), colour = "white") + 
    ggplot2::scale_fill_gradient(low = "white", high = "black") +
    ggplot2::theme_grey(base_size = 9) + 
    ggplot2::labs(x = "", y = "") + 
    ggplot2::theme(legend.position = "none",
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 9 * 0.8, colour = "grey50"),
                   axis.text.x = ggplot2::element_text(size = 9 * 0.8, 
                                                       angle = 30, hjust = 0, colour = "grey50"))
  
  if(x$modes==1){
    g <- g + ggplot2::scale_x_discrete(expand = c(0, 0), position = "top", limits = colnames(x$blocked.data)[x$order.vector]) +
      ggplot2::scale_y_discrete(expand = c(0, 0), limits = rev(rownames(x$blocked.data)[x$order.vector])) + 
      ggplot2::geom_vline(xintercept = c(1+which(diff(x$block.membership)!=0))-.5, colour = "red") +
      ggplot2::geom_hline(yintercept = nrow(x$blocked.data) - c(1+which(diff(x$block.membership)!=0))+1.5, colour = "red")
  } else {
    g <- g + ggplot2::scale_y_discrete(expand = c(0, 0), limits = rev(rownames(x$blocked.data)[x$order.vector$nodes1])) +
      ggplot2::scale_x_discrete(expand = c(0, 0), position = "top", limits = colnames(x$blocked.data)[x$order.vector$nodes2]) + 
      ggplot2::geom_vline(xintercept = c(1+which(diff(x$block.membership$nodes2)!=0))-.5, colour = "blue") +
      ggplot2::geom_hline(yintercept = nrow(x$blocked.data) - c(1+which(diff(x$block.membership$nodes1)!=0))+1.5, colour = "red")
  }
  g
  
}

#' A plotting function that visualises historical milestones/releases
#'
#' The function will take a data frame that details this information,
#' or more usefully, a Github repository listing.
#' @param repo the github repository to track, e.g. "snlab-ch/migraph"
#' @importFrom httr GET content warn_for_status stop_for_status http_error
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom stats ave
#' @importFrom stringr str_split str_remove
#' @import ggplot2
#' @import lubridate
#' @details The function creates a project timeline graphic using ggplot2
#' with historical milestones and milestone statuses gathered from a
#' specified GitHub repository.
#' @source https://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/
#' @return A ggplot graph object
#' @examples
#' if(!httr::http_error("https://api.github.com/repos/snlab-ch/migraph/releases")){
#' plot_releases("snlab-ch/migraph")
#' }
#' @export
plot_releases <- function(repo) {
  
  if (!is.data.frame(repo)) {
    
    get_releases <- function(repo) {
      
      repo <- paste0("https://api.github.com/repos/", repo, "/releases")
      # if (httr::http_error(df)) { # network is down = message (not an error anymore)
      #   message("No internet connection or data source broken.")
      #   return(NULL)
      # } else { # network is up = proceed to GET via httr
      
      df <- httr::GET(repo, query = list(state = "all", per_page = 100, page = 1))
      httr::stop_for_status(df)
      httr::warn_for_status(df)
      df <- httr::content(df, type = "text", encoding = "UTF-8")
      df <- jsonlite::fromJSON(df, flatten = TRUE)
      df <- df[, c("tag_name", "url", "published_at")]
      df$date <- stringr::str_remove(df$published_at, "T.*$")
      df$date <- lubridate::ymd(stringr::str_replace(df$date,
                                                     "-[:digit:]*$", "-01"))

      code_milestone <- function(tag_name) {
        tags <- c(tag_name, "v0.0.0")
        test <- lapply(stringr::str_split(stringr::str_remove(tags, "v"), "\\."),
                       function(x) as.numeric(x))
        elemt <- function(lst, n) {
          sapply(lst, `[`, n)
        }
        ifelse(elemt(test, 3) > dplyr::lead(elemt(test, 3)),
               "Patch",
               ifelse(elemt(test, 2) > dplyr::lead(elemt(test, 2)),
                      "Minor", "Major"))[-length(tags)]
      }
      
      df$milestone <- code_milestone(df$tag_name)
      df
      # }
    }
    
    df <- get_releases(repo)
  } else df <- repo
  
  milestone_levels <- c("Patch", "Minor", "Major")
  milestone_colors <- c("darkgreen", "blue", "red")
  
  df$milestone <- factor(df$milestone,
                         levels = milestone_levels, ordered = TRUE)
  
  positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
  directions <- c(1, -1)
  
  line_pos <- data.frame(
    "date" = unique(df$date),
    "position" = rep(positions, length.out = length(unique(df$date))),
    "direction" = rep(directions, length.out = length(unique(df$date)))
  )
  
  df <- merge(df, line_pos, by = "date", all = TRUE)
  df <- df[with(df, order(date, milestone)), ]
  
  text_offset <- 0.05
  df$month_count <- stats::ave(df$date == df$date, df$date, FUN = cumsum)
  df$text_position <- (df$month_count * text_offset * df$direction) + df$position
  
  month_buffer <- 2
  
  month_date_range <- seq(min(df$date) - months(month_buffer),
                          max(df$date) + months(month_buffer), by = "month")
  month_format <- format(month_date_range, "%b")
  month_df <- data.frame(month_date_range, month_format)
  
  year_date_range <- seq(min(df$date) - months(month_buffer),
                         max(df$date) + months(month_buffer), by = "year")
  if (length(year_date_range) == 1) year_date_range <- c(min(df$date) - months(month_buffer),
                                                         max(df$date) + months(month_buffer))
  year_date_range <- as.Date(
    intersect(
      lubridate::ceiling_date(year_date_range, unit = "year"),
      lubridate::floor_date(year_date_range, unit = "year")
    ),  origin = "1970-01-01"
  )
  year_format <- format(year_date_range, "%Y")
  year_df <- data.frame(year_date_range, year_format)
  
  timeline_plot <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = 0,
                                                    col = .data$milestone,
                                                    label = .data$milestone))
  timeline_plot <- timeline_plot + ggplot2::labs(col = "Milestones")
  timeline_plot <- timeline_plot + ggplot2::scale_color_manual(values = milestone_colors,
                                                               labels = milestone_levels,
                                                               drop = FALSE)
  timeline_plot <- timeline_plot + ggplot2::theme_classic()
  
  # Plot horizontal black line for timeline
  timeline_plot <- timeline_plot + ggplot2::geom_hline(yintercept = 0,
                                                       color = "black", size = 0.3)
  
  # Plot vertical segment lines for milestones
  timeline_plot <- timeline_plot + ggplot2::geom_segment(data = df[df$month_count == 1, ],
                                                         ggplot2::aes(y = .data$position,
                                                                      yend = 0, xend = date),
                                                         color = "black", size = 0.2)
  
  # Plot scatter points at zero and date
  timeline_plot <- timeline_plot + ggplot2::geom_point(ggplot2::aes(y = 0), size = 3)
  
  # Don't show axes, appropriately position legend
  timeline_plot <- timeline_plot + ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                                                  axis.text.y = ggplot2::element_blank(),
                                                  axis.title.x = ggplot2::element_blank(),
                                                  axis.title.y = ggplot2::element_blank(),
                                                  axis.ticks.y = ggplot2::element_blank(),
                                                  axis.text.x = ggplot2::element_blank(),
                                                  axis.ticks.x = ggplot2::element_blank(),
                                                  axis.line.x = ggplot2::element_blank(),
                                                  legend.position = "bottom"
  )
  
  # Show text for each month
  timeline_plot <- timeline_plot + ggplot2::geom_text(data = month_df,
                                                      ggplot2::aes(x = month_date_range,
                                                                   y =-0.1, label = month_format),
                                                      size = 2.5, vjust = 0.5, color = "black", angle=90)
  # Show year text if applicable
  if(nrow(year_df)>0) timeline_plot <- timeline_plot + ggplot2::geom_text(data = year_df,
                                                      ggplot2::aes(x = year_date_range,
                                                                   y = -0.2,
                                                                   label = year_format,
                                                                   fontface = "bold"),
                                                      size = 2.5, color = "black")
  # Show text for each milestone
  timeline_plot <- timeline_plot + ggplot2::geom_text(ggplot2::aes(y = .data$text_position, label = .data$tag_name),
                                                      size = 2.5)
  print(timeline_plot)
}

