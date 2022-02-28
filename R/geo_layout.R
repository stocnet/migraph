#' Plot geographical networks
#'
#' Creates a plot of the a unimodal geographical network.
#'
#' @param object Unimodal geographical network. Needs to contain ISO3c country
#' IDs.
#' @param date String date at which the network snapshot was taken e.g.
#' "2010-01-01". Used by \code{{cshapes}} to plot the correct map hence needs to
#' be between 1886 and 2021.
#' @param theme Theme you would like to use to plot the graph. Available themes
#' are "light", "dark", and "earth".
#'
#' @return A map of a country level geographical network.
#'
#' @examples
#' # Load the unimodal network of environmental agreements signed in 2010
#' # Made from {manyenviron} using data from ECOLEX.
#' membership <- migraph:::membership
#'
#' # Plot the network at the specified date
#' # Light theme
#' network_map(membership, date = "2010-01-01", theme = "light") +
#'   labs(title = "International Environmental Treaties 2010",
#'        subtitle = "Ecolex data",
#'        caption = "Created with love by {migraph}")
#' # Dark theme
#' network_map(membership, date = "2010-01-01", theme = "dark") +
#'   labs(title = "International Environmental Treaties 2010",
#'        subtitle = "Ecolex data",
#'        caption = "Created with love by {migraph}")
#' # Earth theme
#' network_map(membership, date = "2010-01-01", theme = "earth") +
#'   labs(title = "International Environmental Treaties 2010",
#'        subtitle = "Ecolex data",
#'        caption = "Created with love by {migraph}")
#' @export

network_map <- function(object,
                        date,
                        theme = "light") {
  ID <- weight <- NULL
  # Checks for correct input
  if (!is_graph(object)) stop("Not a valid graph object.")
  if (is_multiplex(object)) stop("Graph should be unimodal. Use project_cols() to convert it.")
  if (!is.character(date)) as.character(date)
  if (!(theme %in% c("dark", "earth", "light"))) {
    stop("Specify a theme: light, dark, earth")
  }
  # Select ggplot theme
  if (theme == "dark") {
    maptheme <- maptheme(palette = c("#FFFAFA", "#596673"))
    countrycolor <- "#FFFAFA"
  }
  if (theme == "earth") {
    maptheme <- maptheme(palette = c("#79B52F", "#4259FD"))
    countrycolor <- "#79B52F"
  }
  if (theme == "light") {
    maptheme <- maptheme(palette = c("#596673", "#FFFAFA"))
    countrycolor <- "#596673"
  }
  # Step 1: Import the historical shapefile data
  cshapes <- import_cshapes(date)
  # Step 2: create edges with from/to lat/long
  edges <- igraph::as_data_frame(object) %>%
    dplyr::inner_join(cshapes,
      by = c("from" = "ID")
    ) %>%
    dplyr::rename(x = .data$caplong, y = .data$caplat) %>%
    dplyr::inner_join(cshapes,
      by = c("to" = "ID")
    ) %>%
    dplyr::rename(xend = .data$caplong, yend = .data$caplat)
  # Step 3: Create plotted network from computed edges
  g <- migraph::as_tidygraph(edges)
  # Step 4: Get the country shapes from the edges dataframe
  country_shapes <- ggplot2::geom_sf(
    data = cshapes$geometry,
    fill = countrycolor
  )
  # Step 5: Get a non-standard projection of the underlying map(optional)
  # Could include different projections for continents etc
  # Step 6: Generate the point coordinates for capitals
  cshapes_pos <- cshapes %>%
    dplyr::filter(ID %in% migraph::node_names(g)) %>%
    dplyr::rename(x = .data$caplong, y = .data$caplat)
  # Reorder things according to nodes in plotted network g
  cshapes_pos <- cshapes_pos[match(
    migraph::node_names(g),
    cshapes_pos$ID
  ), ]
  # Generate the layout
  lay <- ggraph::create_layout(g, layout = cshapes_pos)
  # Add additional elements to the layout
  edges$circular <- rep(FALSE, nrow(edges))
  edges$edge.id <- rep(1, nrow(edges))
  # Step 7: Plot things
  ggraph::ggraph(lay) + country_shapes +
    ggraph::geom_edge_arc(
      data = edges, aes(edge_width = weight),
      strength = 0.33,
      alpha = 0.25
    ) +
    ggraph::scale_edge_width_continuous(
      range = c(0.5, 2), # scale for edge widths
      guide = "none"
    ) +
    ggraph::geom_node_point(
      shape = 21, # draw nodes
      fill = "white", color = "black",
      stroke = 0.5
    ) +
    ggraph::geom_node_text(aes(label = node_names(g)),
      repel = TRUE, size = 3,
      color = "white", fontface = "bold"
    ) +
    maptheme
}


# Helper function to import historical map data from {cshapes}:

import_cshapes <- function(date, ...) {
  # Step 0: Set string dates to actual dates
  date <- as.Date(date)
  # Step 0.5: Test for correct dates
  if (!(as.numeric(format(date, format = "%Y")) >= 1886 &
    as.numeric(format(date, format = "%Y")) <= 2021)) {
    stop("Please input a date in the following range: 1886-01-01 -
         end of the dataset")
  }
  # Extract data from the {cshapes} package
  cshapes <- cshapes::cshp(date, ..., useGW = FALSE) # Use COW_ID instead of GW
  # Switch to iso3c with custom matching for ambiguous COW codes
  custom_match <- c(
    `730` = "KOR", # Korea
    `345` = "YUG", # Yugoslavia
    `347` = "UNK"
  ) # Kosovo
  cshapes$ID <- countrycode::countrycode(cshapes$cowcode,
    "cown",
    "iso3c",
    custom_match = custom_match
  )
  # Select only relevant columns for map layout
  cshapes <- dplyr::select(cshapes, .data$ID, .data$caplong, .data$caplat)
  cshapes
}

# Helper function providing the network map function with a few map themes.
maptheme <- function(palette = c("#FFFAFA", "#596673")) {
  landcolor <- palette[1]
  oceancolor <- palette[2]
  titlecolor <- ifelse(is_dark(palette[2]), "white", "black")
  # Create map theme
  maptheme <- ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::theme(axis.text = ggplot2::element_blank()) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    ggplot2::theme(axis.title = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::theme(panel.background = ggplot2::element_blank()) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = oceancolor)) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        color = titlecolor,
        hjust = 0.1,
        vjust = 0.1
      ),
      plot.subtitle = ggplot2::element_text(
        color = titlecolor,
        hjust = 0.065,
        vjust = 0.1
      ),
      plot.caption = ggplot2::element_text(
        color = titlecolor,
        hjust = 0.96
      )
    ) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(0, 0, 0.5, 0), "cm"))
  # This function returns a map theme for ggplot
  maptheme
}

# Helper function to check whether a color is light or dark:
is_dark <- function(hex) {
  # Google luma formula for details.
  luma <- 0.33 * grDevices::col2rgb(hex)[[1]] +
    0.5 * grDevices::col2rgb(hex)[[2]] +
    0.16 * grDevices::col2rgb(hex)[[3]]
  isdark <- ifelse(luma < 186, TRUE, FALSE)
  isdark
}
