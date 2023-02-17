#' Make networks from/to external formats
#'
#' @description 
#' Researchers regularly need to work with a variety of external data formats.
#' The following functions offer ways to import from some common external
#' file formats into objects that `{migraph}` and other graph/network packages
#' in R can work with.
#' 
#' Note that these functions are not as actively maintained as others
#' in the package, so please let us know if any are not currently working
#' for you or if there are missing import routines 
#' by [raising an issue on Github](https://github.com/snlab-ch/migraph/issues).
#' @param file A character string with the system path to the file to import.
#' If left unspecified, an OS-specific file picker is opened to help users select it.
#' Note that in `read_ucinet()` the file path should be to the header file (.##h),
#' if it exists and that it is currently not possible to import multiple
#' networks from a single UCINET file. Please convert these one by one.
#' @inheritParams is
#' @param sv Allows users to specify whether their csv file is
#' `"comma"` (English) or `"semi-colon"` (European) separated.
#' @param ... Additional parameters passed to the read/write function.
#' @return `read_edgelist()` and `read_nodelist()` will import
#' into edgelist (tibble) format which can then be coerced or combined into
#' different graph objects from there.
#'
#' `read_pajek()` and `read_ucinet()` will import into
#' a tidygraph format, since they already contain both edge and attribute data.
#' `read_matrix()` will import into tidygraph format too.
#' Note that all graphs can be easily coerced into other formats
#' with `{migraph}`'s `as_` methods.
#'
#' The `write_`functions export to different file formats,
#' depending on the function.
#' @family makes
#' @details There are a number of repositories for network data
#' that hold various datasets in different formats. See for example:
#'
#' - [UCINET data](https://sites.google.com/site/ucinetsoftware/datasets?authuser=0)
#' - [Pajek data](http://vlado.fmf.uni-lj.si/pub/networks/data/)
#'
#' See also:
#'
#' - [networkdata](http://networkdata.schochastics.net/)
#' - [GML datasets](http://www-personal.umich.edu/~mejn/netdata/)
#' - [UCIrvine Network Data Repository](http://networkdata.ics.uci.edu/)
#' - [KONECT project](http://konect.cc/)
#' - [SNAP Stanford Large Network Dataset Collection](http://snap.stanford.edu/data/)
#'
#' Please let us know if you identify any further repositories
#' of social or political networks and we would be happy to add them here.
#'
#' The `_ucinet` functions only work with relatively recent UCINET
#' file formats, e.g. type 6406 files.
#' To import earlier UCINET file types, you will need to update them first.
#' To import multiple matrices packed into a single UCINET file,
#' you will need to unpack them and convert them one by one.
#' @examples
#' \dontrun{
#' # import Roethlisberger & Dickson's horseplay game data set:
#' horseplay <- read_ucinet("WIRING-RDGAM.##h")
#' }
#' @source 
#' `read_ucinet()` and `write_ucinet()` kindly supplied by Christian Steglich, 
#' constructed on 18 June 2015.
#' @importFrom utils read.csv read.csv2 read.table write.csv write.csv2
#' @name read
#' @seealso [as]
NULL

#' @describeIn read Reading adjacency matrices from Excel/csv files
#' @export
read_matrix <- function(file = file.choose(),
                          sv = c("comma", "semi-colon"),
                          ...) {
  sv <- match.arg(sv)
  if (grepl("csv$", file)) {
    if (sv == "comma") {
      out <- read.csv(file, ...) # For US
    } else {
      out <- read.csv2(file, ...) # For EU
    }
  } else if (grepl("xlsx$|xls$", file)) {
    if(requireNamespace("readxl", quietly = TRUE)){
      out <- readxl::read_excel(file, ...)  
    } else stop("Please install `readxl` from CRAN to import Excel files.")
  }
  if((dim(out)[1]+1) == dim(out)[2])
    out <- out[,-1]
  if(!is.null(colnames(out)) & 
     all(colnames(out) == paste0("X",seq_along(colnames(out)))))
    colnames(out) <- NULL
  if(!is.null(colnames(out)) & is.null(rownames(out)) &
     dim(out)[1] == dim(out)[2])
    rownames(out) <- colnames(out)
  as_tidygraph(as.matrix(out))
}

#' @describeIn read Reading edgelists from Excel/csv files
#' @export
read_edgelist <- function(file = file.choose(),
                          sv = c("comma", "semi-colon"),
                          ...) {
  sv <- match.arg(sv)
  if (grepl("csv$", file)) {
    if (sv == "comma") {
      out <- read.csv(file, header = TRUE, ...) # For US
    } else {
      out <- read.csv2(file, header = TRUE, ...) # For EU
    }
  } else if (grepl("xlsx$|xls$", file)) {
    if(requireNamespace("readxl", quietly = TRUE)){
      out <- readxl::read_excel(file, ...)  
    } else stop("Please install `readxl` from CRAN to import Excel files.")
  }
  out
}

#' @describeIn read Writing edgelists to csv files
#' @export
write_edgelist <- function(.data,
                           filename,
                           name,
                           ...) {
  if (missing(.data)) {
    out <- data.frame(
      from = c("A", "B", "C"),
      to = c("B", "C", "A"),
      weight = c(1.1, 11, 110)
    )
    object_name <- "test"
  } else {
    object_name <- deparse(substitute(.data))
    out <- as.data.frame(as_edgelist(.data))
  }
  if (missing(filename)) filename <- paste0(getwd(), "/", object_name, ".csv")
  if (missing(name)) name <- object_name
  write.csv(out, file = filename, row.names = FALSE, ...)
}

#' @describeIn read Reading nodelists from Excel/csv files
#' @export
read_nodelist <- function(file = file.choose(),
                          sv = c("comma", "semi-colon"),
                          ...) {
  sv <- match.arg(sv)
  if (grepl("csv$", file)) {
    if (sv == "comma") {
      out <- read.csv(file, header = TRUE, ...) # For US
    } else {
      out <- read.csv2(file, header = TRUE, ...) # For EU
    }
  } else if (grepl("xlsx$|xls$", file)) {
    if(requireNamespace("readxl", quietly = TRUE)){
      out <- readxl::read_excel(file, ...)  
    } else stop("Please install `readxl` from CRAN to import Excel files.")
  }
  out
}

#' @describeIn read Writing nodelists to csv files
#' @export
write_nodelist <- function(.data,
                           filename,
                           name,
                           ...) {
  if (missing(.data)) {
    out <- data.frame(
      type = c(FALSE, FALSE, TRUE),
      name = c("A", "B", "C")
    )
    object_name <- "test"
  } else {
    object_name <- deparse(substitute(.data))
    out <- as.data.frame(as_tidygraph(.data))
  }
  if (missing(filename)) filename <- paste0(getwd(), "/", object_name, ".csv")
  if (missing(name)) name <- object_name
  write.csv(out, file = filename, row.names = FALSE, ...)
}

#' @describeIn read Reading pajek (.net/.paj) files
#' @param ties Where there are 
#' @importFrom network read.paj
#' @importFrom utils read.delim
#' @export
read_pajek <- function(file = file.choose(), 
                       ties = NULL,
                       ...) {
  paj <- network::read.paj(file, ...)
  if(!is.network(paj)){
    if(is.null(ties)) 
      stop(paste("This file contains multiple networks/ties.",
        "Please choose a set of ties for the imported network among:\n",
                 paste0("- '", names(paj$networks), "'", collapse = "\n "),
        "\n by adding the name as a character string to the `ties = ` argument"))
    out <- paj[[1]][[ties]]
    if("partitions" %in% names(paj)){
      for(x in names(paj$partitions)){
        out <- add_node_attribute(out, 
                                  attr_name = gsub(".clu","",x),
                                  vector = paj$partitions[,x])
      }
    }
    out <- as_tidygraph(out)
  } else {
    out <- as_tidygraph(paj)
  }
  # if(grepl("Partition", utils::read.delim(file))){
  #   clus <- strsplit(paste(utils::read.delim(file)), "\\*")[[1]]
  #   clus <- clus[grepl("^Vertices|^Partition", clus)][-1]
  #   if(length(clus) %% 2 != 0) stop("Unexpected .pajek file structure.")
  #   namo <- clus[c(TRUE, FALSE)]
  #   attr <- clus[c(FALSE, TRUE)]
  #   for (i in seq_len(namo)){
  #     vct <- strsplit(attr[i], ",")[[1]][-1]
  #     vct <- gsub("\"", "", vct)
  #     vct <- gsub(" ", "", vct, fixed = TRUE)
  #     vct <- vct[!grepl("^$", vct)]
  #     if(all(grepl("^-?[0-9.]+$", vct))) vct <- as.numeric(vct)
  #     out <- add_node_attribute(out, 
  #                               attr_name = strsplit(namo[i], " |\\.")[[1]][2],
  #                               vector = vct)
  #   }
  # } 
  out
}

#' @describeIn read Writing pajek .net files
#' @importFrom igraph write_graph
#' @export
write_pajek <- function(.data,
                        filename,
                        ...) {
  if (missing(filename)) {
    object_name <- deparse(substitute(.data))
    filename <- paste0(getwd(), "/", object_name, ".net")
  }
  igraph::write_graph(as_igraph(.data),
    file = filename,
    format = "pajek",
    ...
  )
}

# igraph::write_graph(graph = object, file = path, ...)

#' @describeIn read Reading UCINET files
#' @export
read_ucinet <- function(file = file.choose()) {
  # Some basic checks of the input file
  # Check if the file is a UCINET header file
  if (!grepl(".##h$", file)) {
    stop("Please select the UCINET header file with the
                                  '.##h' extension.")
  } # Continue if header file is selected
  # Check whether there is a data file to be imported in the same folder as the
  # hearder file.
  if (!(file.exists(sub("h$", "d", file)))) stop("UCINET data file not found.
                                                 Please add the '.##d' file in
                                                 the same folder as the header
                                                 file you are trying to
                                                 import. It should also have
                                                 the same name as the header
                                                 file.")
    read_ucinet_header <- function(header_file) {
      UCINET.header <- file(header_file, "rb")
      ignore <- readBin(UCINET.header, what = "int", size = 1)
      headerversion <- paste(
        rawToChar(readBin(UCINET.header, what = "raw", size = 1)),
        rawToChar(readBin(UCINET.header, what = "raw", size = 1)),
        rawToChar(readBin(UCINET.header, what = "raw", size = 1)),
        rawToChar(readBin(UCINET.header, what = "raw", size = 1)),
        rawToChar(readBin(UCINET.header, what = "raw", size = 1)),
        sep = ""
      )
      # Check for correct UCINET version
      if (!(headerversion %in% c("DATE:", "V6404"))) {
        close(UCINET.header)
        stop(paste("Unknown header type; try more recent UCINET file types"))
      }
      # Get ymd and weekday of the UCINET file
      year <- 2000 + readBin(UCINET.header, what = "int", size = 2)
      month <- c(
        "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
        "Sep", "Oct", "Nov", "Dec"
      )[readBin(UCINET.header, what = "int", size = 2)]
      day <- readBin(UCINET.header, what = "int", size = 2)
      dow <- c(
        "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
        "Saturday", "Sunday"
      )[readBin(UCINET.header, what = "int", size = 2)]
      labtype <- readBin(UCINET.header, what = "int", size = 2)
      infile.dt <- c(
        "nodt", "bytedt", "booleandt", "shortintdt", "worddt",
        "smallintdt", "longintdt", "singledt", "realdt", "doubledt",
        "compdt", "extendeddt", "labeldt", "setdt", "stringdt", "pointerdt",
        "chardt", "integerdt", "nodelistdt", "sparsedt", "int64dt"
      )[
        readBin(UCINET.header, what = "int", size = 1)
      ]
      # Get the dimensions of the matrix
      ndim <- readBin(UCINET.header, what = "int", size = 2)
      if (headerversion == "V6404") {
        fct <- 2
      } else {
        fct <- 1
      }
      dims <- c(
        readBin(UCINET.header, what = "int", size = 2 * fct),
        readBin(UCINET.header, what = "int", size = 2 * fct)
      )
      if (ndim == 3) {
        dims[3] <- readBin(UCINET.header, what = "int", size = 2 * fct)
      }
      # Check if user tries to import multiple networks at once.
      # This check fails if it is a time series or multilevel network.
      if (!(ndim == 2 | ndim == 3 & dims[3] == 1)) {
        close(UCINET.header)
        stop(paste("UCINET file with", dims[3], "levels; please convert separately"))
      }
      # Extract the title of the UCINET network
      t.length <- readBin(UCINET.header, what = "int", size = 1)
      if (t.length > 0) {
        titl <- vapply(seq_len(t.length), function(i) {
          rawToChar(readBin(UCINET.header, what = "raw", size = 1))
        }, FUN.VALUE = character(1))
        titl <- paste(titl, collapse = "")
      } else {
        titl <- ""
      }
      haslab <- c(
        readBin(UCINET.header, what = "logical", size = 1),
        readBin(UCINET.header, what = "logical", size = 1)
      )
      if (ndim == 3) {
        haslab[3] <- readBin(UCINET.header, what = "logical", size = 1)
      }
      dim.labels <- list()
      for (arr.dim in seq_len(length(dims))) {
        if (haslab[arr.dim]) {
          dim.labels[[arr.dim]] <- rep(NA, dims[arr.dim])
          for (i in seq_len(dims[arr.dim])) {
            lab <- ""
            lablen <- readBin(UCINET.header, what = "int", size = 2)
            for (let in seq_len(lablen)) {
              lab <- paste(lab,
                rawToChar(readBin(UCINET.header, what = "raw", size = 1)),
                sep = ""
              )
            }
            dim.labels[[arr.dim]][i] <- lab
          }
        }
      }
      # Close file connection
      close(UCINET.header)
      if (ndim == 3 & dims[3] == 1) {
        titl <- dim.labels[[3]][1]
        # warning(paste('UCINET file with one level; level name "',
        # 	titl,'" treated as network name',sep=''))
        ndim <- 2
        dims <- dims[1:2]
        haslab <- haslab[1:2]
        dim.labels <- dim.labels[1:2]
      }
      return(list(
        headerversion = headerversion,
        date = paste(dow, paste(day, month, year, sep = "-")),
        labtype = labtype,
        infile.dt = infile.dt,
        ndim = ndim,
        dims = dims,
        titl = titl,
        haslab = haslab,
        dim.labels = dim.labels
      ))
    }
    # Start of main function code:
    header <- read_ucinet_header(file)
    file <- sub(".##h", "", file)
    # Read in the actual data file ".##d"
    UCINET.data <- file(paste(file, ".##d", sep = ""), "rb")
    thedata <- vector()
    for (i in 1:(header$dims[1] * header$dims[2])) {
      thedata[i] <- readBin(UCINET.data,
        what = "numeric",
        size = 4,
        endian = "little"
      )
    }
    close(UCINET.data)
    # Build the adjacency matrix
    mat <- matrix(thedata,
      nrow = header$dims[2],
      ncol = header$dims[1],
      dimnames = header$dim.labels[c(2, 1)],
      byrow = TRUE
    )
    # put additional info from header file on matrix
    if (!(is.null(header$title))) {
      attr(mat, "title") <- header$title
    }
    attr(mat, "date") <- header$date
    # attr(mat,'labtype') <- header$labtype
    # attr(mat,'infile.dt') <- header$infile.dt
    # Convert the adjacency matrix to a tidygraph object
    as_tidygraph(mat)
}

#' @describeIn read Writing UCINET files
#' @param filename UCINET filename (without ## extension).
#' By default the files will have the same name as the object
#' and be saved to the working directory.
#' @param name name of matrix to be known in UCINET.
#' By default the name will be the same as the object.
#' @importFrom utils askYesNo
#' @return A pair of UCINET files in V6404 file format (.##h, .##d)
#' @examples
#' \dontrun{
#' # export it again to UCINET under a different name:
#' write_ucinet(horseplay, "R&D-horseplay")
#' }
#' @export
write_ucinet <- function(.data,
                         filename,
                         name) {
  object_name <- deparse(substitute(.data))
  if (missing(filename)) filename <- paste0(getwd(), "/", object_name)
  if (missing(name)) name <- object_name
  # Check to avoid overwriting files by mistake
  if (file.exists(paste(filename, ".##h", sep = ""))) {
    overwrite <- utils::askYesNo(paste("There is already a file called ", 
                            object_name, 
                            ".##h here. Do you want to overwrite it?", 
                            sep = ""))
    if (overwrite == FALSE | is.na(overwrite)) {
      stop("Writing aborted by user.")
    }
  }
  mat <- as_matrix(.data)
  # start with UCINET header file:
  UCINET.header <- file(paste(filename, ".##h", sep = ""), "wb")
  writeBin(as.integer(5), UCINET.header, size = 1)
  writeBin(charToRaw("V"), UCINET.header, size = 1)
  writeBin(charToRaw("6"), UCINET.header, size = 1)
  writeBin(charToRaw("4"), UCINET.header, size = 1)
  writeBin(charToRaw("0"), UCINET.header, size = 1)
  writeBin(charToRaw("4"), UCINET.header, size = 1)
  year <- as.integer(substr(Sys.Date(), 3, 4))
  writeBin(year, UCINET.header, size = 2)
  month <- as.integer(substr(Sys.Date(), 6, 7))
  writeBin(month, UCINET.header, size = 2)
  day <- as.integer(substr(Sys.Date(), 9, 10))
  writeBin(day, UCINET.header, size = 2)
  dow <- which(c(
    "Mon",
    "Tue",
    "Wed",
    "Thu",
    "Fri",
    "Sat",
    "Sun"
  ) == substr(date(), 1, 3))
  writeBin(dow, UCINET.header, size = 2)
  writeBin(as.integer(3), UCINET.header, size = 2)
  # labtype, unused in V6404 files
  writeBin(as.integer(7), UCINET.header, size = 1) # infile.dt = 7 'longintdt'
  writeBin(as.integer(2), UCINET.header, size = 2) # ndim = 2 for matrix
  writeBin(ncol(mat), UCINET.header, size = 4) # number of columns of matrix
  writeBin(nrow(mat), UCINET.header, size = 4) # number of rows of matrix
  writeBin(nchar(name), UCINET.header, size = 1) # length of matrix name
  if (nchar(name) > 0) {
    for (i in 1:nchar(name)) {
      writeBin(charToRaw(substr(name, i, i)), UCINET.header, size = 1)
    }
  }
  # Deal with column names of adjacency matrix
  labc <- colnames(mat)
  # Encoding(labc) <- "UTF-8"
  if (!is.null(labc)) {
    if (length(table(labc)) != length(labc)) {
      labc <- NULL
      warning("non-unique column labels, all column labels are dropped")
    }
  }
  writeBin(!is.null(labc), UCINET.header, size = 1)
  # Deal with column names of adjacency matrix
  labr <- rownames(mat)
  # Encoding(labr) <- "UTF-8"
  if (!is.null(labr)) {
    if (length(table(labr)) != length(labr)) {
      labr <- NULL
      warning("non-unique row labels, all row labels are dropped")
    }
  }
  writeBin(!is.null(labr), UCINET.header, size = 1)
  # Write node names of columns
  if (!is.null(labc)) {
    for (i in seq_len(ncol(mat))) {
      writeBin(as.integer(2 * nchar(labc[i])), UCINET.header, size = 2)
      for (let in seq_len(nchar(labc[i]))) {
        writeBin(charToRaw(substr(labc[i], let, let)),
          UCINET.header,
          size = 1
        )
        writeBin(raw(1), UCINET.header, size = 1)
      }
    }
  }
  # Write node names of rows
  if (!is.null(labr)) {
    for (i in seq_len(nrow(mat))) {
      writeBin(as.integer(2 * nchar(labr[i])), UCINET.header, size = 2)
      for (let in seq_len(nchar(labr[i]))) {
        writeBin(charToRaw(substr(labr[i], let, let)),
          UCINET.header,
          size = 1
        )
        writeBin(raw(1), UCINET.header, size = 1)
      }
    }
  }
  close(UCINET.header)
  # continue with UCINET data file: --> Write the actual matrix
  UCINET.data <- file(paste(filename, ".##d", sep = ""), "wb")
  for (i in seq_len(length(mat))) {
    writeBin(t(mat)[i], UCINET.data, size = 4, endian = "little")
  }
  close(UCINET.data)
}

#' @describeIn read Reading DynetML files
#' @export
read_dynetml <- function(file = file.choose()) {
  if(!requireNamespace("xml2", quietly = TRUE)){
    stop("Please install `xml2` from CRAN to import DynetML files.")
  } else {
    
    name <- type <- nodeset <- target <- value <- NULL
    
    xmlfile <- xml2::read_xml(file)
    xmllist <- xml2::as_list(xmlfile)
    
    # Getting nodeset
    # to deal with legacy constructions:
    if("MetaMatrix" %in% names(xmllist$DynamicNetwork))
      nodesets <- xmllist$DynamicNetwork$MetaMatrix$nodes else
        nodesets <- xmllist$DynamicNetwork$MetaNetwork$nodes
    nodesets <- dplyr::coalesce(unlist(lapply(nodesets, 
                                              function(x) ifelse(is.null(attr(x, "id")),
                                                                 NA_character_, attr(x, "id")))),
                                unlist(lapply(nodesets, 
                                              function(x) ifelse(is.null(attr(x, "type")),
                                                                 NA_character_, attr(x, "type")))))
    # to deal with legacy constructions:
    if("MetaMatrix" %in% names(xmllist$DynamicNetwork)){
      nodesets <- unname(rep(nodesets, vapply(xmllist$DynamicNetwork$MetaMatrix$nodes,
                                              function(x) length(x), numeric(1))))
    } else
      nodesets <- unname(rep(nodesets, vapply(xmllist$DynamicNetwork$MetaNetwork$nodes,
                                              function(x) length(x), numeric(1)))) 
    
    # Getting nodes
    nodes <- xml2::as_list(xml2::xml_find_all(xmlfile, ".//node"))
    nodes <- dplyr::bind_rows(lapply(nodes, function(x){
      values <- sapply(x$properties, function(y) attr(y, "value"))
      attrs <- sapply(x$properties, function(y) attr(y, "name"))
      names(values) <- attrs
      c(name = attr(x, "id"), values)
    }))
    # Add nodeset information if necessary
    if(length(unique(nodesets))==2)
      nodes <- nodes %>% dplyr::mutate(type = nodesets == unique(nodesets)[2]) %>% 
      dplyr::select(name, type, dplyr::everything()) else if (length(unique(nodesets))>2)
        nodes <- nodes %>% dplyr::mutate(nodeset = nodesets) %>% 
      dplyr::select(name, nodeset, dplyr::everything())
    
    # Getting edges
    edgelist <- xml2::xml_attrs(xml2::xml_find_all(xmlfile, ".//edge"))
    # to deal with legacy constructions:
    if(length(edgelist)==0) edgelist <- xml2::xml_attrs(xml2::xml_find_all(xmlfile, ".//link"))
    edgelist <- as.data.frame(t(sapply(edgelist, function(x) x, simplify = TRUE)))
    edgelist$type <- NULL
    edgelist$value <- as.numeric(edgelist$value)
    edgelist <- dplyr::filter(edgelist, source %in% nodes$name & target %in% nodes$name)
    edgelist <- dplyr::filter(edgelist, value != 0)
    as_tidygraph(list(nodes = nodes, ties = edgelist))
  }
}
  
