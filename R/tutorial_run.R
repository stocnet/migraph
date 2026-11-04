# Tutorials overview ####

#' Open and extract code from tutorials
#' 
#' @description 
#'   These functions make it easy to use the tutorials
#'   in the `{manynet}` and `{migraph}` packages:
#'   
#'   - `run_tute()` runs a `{learnr}` tutorial from 
#'   either the `{manynet}` or `{migraph}` packages,
#'   wraps `learnr::run_tutorial()` with some convenience.
#'   - `extract_tute()` extracts and opens just the solution code
#'   from a `{manynet}` or `{migraph}` tutorial,
#'   saving the .R script to the current working directory.
#'   - `search_tute()` finds where a keyword or function appears
#'   in the tutorials of all installed stocnet packages.
#'   It prints every match, with the section and the type of chunk it is in,
#'   and returns the name of the tutorial with the most matches,
#'   so that `run_tute(search_tute("brokerage"))` opens that tutorial.
#'
#' @param tute String, name of the tutorial (e.g. "tutorial2"),
#'   or the result of `search_tute()`.
#' @param pattern String, a keyword or a function name to search for.
#'   A keyword matches anywhere in the text or code, ignoring case.
#'   A name that ends in `()`, such as "node_by_deg()",
#'   or that is a function exported by a stocnet package,
#'   matches only where that function is called or named with parentheses.
#' @return `search_tute()` returns a `migraphSearch` object:
#'   the name of the tutorial with the most matches,
#'   with all the matches in its "matches" attribute.
#'   If nothing matches, it is of length zero.
#' @examples
#' search_tute("brokerage")
#' search_tute("node_by_deg()")
#' \dontrun{
#' run_tute(search_tute("brokerage"))
#' }
#' @importFrom dplyr as_tibble select tibble
#' @name tutorials
NULL

stocnet <- c("manynet", "autograph", "netrics", "infernet", "migraph")

#' @rdname tutorials 
#' @export
run_tute <- function(tute) {
  thisRequires("learnr")
  if (!missing(tute) && inherits(tute, "migraphSearch")) {
    if (length(tute) == 0)
      manynet::snet_abort("The search found no tutorial to run.")
    return(learnr::run_tutorial(as.character(tute), attr(tute, "package")))
  }
  avail_pkgs <- stocnet[suppressWarnings(unlist(lapply(stocnet, 
                                                       function(x) nzchar(system.file(package = x)))))]
  if (missing(tute)) {
    tutelist <- lapply(manynet::snet_progress_along(avail_pkgs, 
                                               name = "Checking tutorials in stocnet packages"), 
                       function(p){
                         dplyr::as_tibble(learnr::available_tutorials(package = avail_pkgs[p]),
                                          silent = TRUE) |> dplyr::select(1:3)
                       })
    dplyr::bind_rows(tutelist) |> 
      print()
    manynet::snet_info("You can run a tutorial by typing e.g `run_tute('tutorial1')` or `run_tute('Data')` into the console.")
  } else {
    try(learnr::run_tutorial(tute, "manynet"), silent = TRUE)
    try(learnr::run_tutorial(tute, "autograph"), silent = TRUE)
    try(learnr::run_tutorial(tute, "netrics"), silent = TRUE)
    try(learnr::run_tutorial(tute, "infernet"), silent = TRUE)
    try(learnr::run_tutorial(tute, "migraph"), silent = TRUE)
    manynet::snet_info("Didn't find a direct match, so looking for close matches...")
    tutelist <- lapply(manynet::snet_progress_along(avail_pkgs, 
                                               name = "Checking tutorials in stocnet packages"), function(p){
                                                 dplyr::as_tibble(learnr::available_tutorials(package = avail_pkgs[p]),
                                                                  silent = TRUE) |> dplyr::select(1:3)
                                               })
    avails <- dplyr::bind_rows(tutelist)
    inftit <- grepl(tute, avails$title, ignore.case = TRUE)
    if(!any(inftit) | sum(inftit)>1)
      inftit <- which.min(utils::adist(tute, avails$title, ignore.case = TRUE,
                                       costs = list(ins=0, del=1, sub=1)))
    if(any(inftit) & sum(inftit)==1){
      manynet::snet_success("And found one!")
      try(learnr::run_tutorial(avails$name[inftit], avails$package[inftit]), silent = TRUE)
    } else{
      manynet::snet_warn("...and couldn't find which one you meant. Please specify one of these titles:\n")
      print(avails)
    }
  }
}

#' @rdname tutorials 
#' @export
extract_tute <- function(tute) {
  if (missing(tute)) {
    thisRequires("learnr")
    avail_pkgs <- stocnet[suppressWarnings(unlist(lapply(stocnet, function(x) nzchar(system.file(package = x)))))]
    tutelist <- lapply(manynet::snet_progress_along(avail_pkgs, 
                                               name = "Checking tutorials in stocnet packages"), function(p){
                                                 dplyr::as_tibble(learnr::available_tutorials(package = avail_pkgs[p]),
                                                                  silent = TRUE) |> dplyr::select(1:3)
                                               })
    dplyr::bind_rows(tutelist) |> 
      dplyr::arrange(dplyr::across(dplyr::any_of("name"))) |> 
      print()
    manynet::snet_info("You can extract the code from one of these tutorials by typing e.g `extract_tute('tutorial1')` into the console.")
  } else {
    thisRequires("knitr")
    # Each package can be in a different library, so find each one's own path
    pth <- ""
    for (p in stocnet) {
      thisRequires(p)
      pth <- system.file("tutorials", tute, package = p)
      if (nzchar(pth)) break
    }
    if (!nzchar(pth))
      manynet::snet_abort(paste0("Could not find a tutorial called '", tute, "'."))
    knitr::purl(file.path(pth, list.files(pth, pattern = "*.Rmd")),
                documentation = 1)
    utils::file.edit(gsub(".Rmd", ".R", list.files(pth, pattern = "*.Rmd")))
  }
}


#' @rdname tutorials 
#' @export
search_tute <- function(pattern) {
  if (missing(pattern) || !is.character(pattern) || length(pattern) != 1 ||
      !nzchar(trimws(pattern)))
    manynet::snet_abort("Please give one keyword or function name to search for, e.g. `search_tute('brokerage')`.")
  avail_pkgs <- stocnet[vapply(stocnet, function(x) nzchar(system.file(package = x)),
                               logical(1))]
  term <- sub("^[[:alnum:].]+::", "", sub("\\(\\)$", "", trimws(pattern)))
  is_fn <- grepl("\\(\\)$", trimws(pattern)) ||
    any(vapply(avail_pkgs, function(p) term %in% getNamespaceExports(p), logical(1)))
  matcher <- if (is_fn) {
    rx <- paste0("(^|[^[:alnum:]._])",
                 gsub("([][.|()^{}+$*?\\\\])", "\\\\\\1", term, perl = TRUE),
                 "\\(")
    function(x) grepl(rx, x)
  } else {
    function(x) grepl(tolower(term), tolower(x), fixed = TRUE)
  }
  found <- lapply(avail_pkgs, function(p) {
    tutes <- list.files(system.file("tutorials", package = p), pattern = "\\.Rmd$",
                        recursive = TRUE, full.names = TRUE)
    dplyr::bind_rows(lapply(tutes, .search_tute_file, package = p, matcher = matcher))
  })
  matches <- dplyr::bind_rows(found)
  if (nrow(matches) == 0) {
    matches <- dplyr::tibble(package = character(), name = character(),
                             title = character(), section = character(),
                             type = character(), text = character())
    best <- character(0)
    pkg <- character(0)
  } else {
    counts <- table(factor(matches$name, levels = unique(matches$name)))
    best <- names(counts)[which.max(counts)]
    pkg <- matches$package[match(best, matches$name)]
  }
  structure(best, class = c("migraphSearch", "character"),
            package = pkg, pattern = pattern, matches = matches)
}

#' @export
print.migraphSearch <- function(x, ...) {
  matches <- attr(x, "matches")
  pattern <- attr(x, "pattern")
  if (length(x) == 0) {
    manynet::snet_warn(paste0("Found no tutorial that mentions '", pattern, "'."))
    return(invisible(x))
  }
  manynet::snet_info(paste0("Found '", pattern, "' ", nrow(matches), " times in ",
                            length(unique(matches$name)), " tutorials:"))
  width <- max(30, getOption("width", 80) - 14)
  for (tute in unique(matches$name)) {
    these <- matches[matches$name == tute, ]
    cat("\n", tute, ": ", these$title[1], " (", nrow(these), ")\n", sep = "")
    for (sect in unique(these$section)) {
      here <- these[these$section == sect, ]
      cat("  ", sect, "\n", sep = "")
      text <- ifelse(nchar(here$text) > width,
                     paste0(substr(here$text, 1, width - 3), "..."), here$text)
      cat(paste0("    ", formatC(here$type, width = -9), text, "\n"), sep = "")
    }
  }
  cat("\n")
  manynet::snet_info(paste0("The tutorial with the most matches is '",
                            as.character(x), "'. Run it with `run_tute('",
                            as.character(x), "')`."))
  invisible(x)
}

# Find the lines of one tutorial that match, and say where each one is
.search_tute_file <- function(file, package, matcher) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  name <- basename(dirname(file))
  yaml <- which(lines == "---")
  title <- name
  if (length(yaml) >= 2) {
    tline <- grep("^title:", lines[yaml[1]:yaml[2]], value = TRUE)
    if (length(tline)) title <- trimws(gsub("^title:|[\"']", "", tline[1]))
  }
  # Ignore the YAML, set-up chunks, and styles that come before the first topic
  start <- grep("^## ", lines)[1]
  if (is.na(start)) return(NULL)
  n <- length(lines)
  type <- section <- character(n)
  topic <- subtopic <- ""
  in_chunk <- FALSE
  chunk <- "text"
  keep <- logical(n)
  for (i in start:n) {
    l <- lines[i]
    if (!in_chunk && grepl("^```\\{r", l)) {
      in_chunk <- TRUE
      chunk <- .tute_chunk_type(l)
      # A chunk that asks a question is a quiz, whatever its label
      end <- which(grepl("^```\\s*$", lines) & seq_len(n) > i)[1]
      if (!is.na(end) && end > i + 1 &&
          any(grepl("^\\s*(learnr::)?(question|quiz)\\(", lines[(i + 1):(end - 1)])))
        chunk <- "quiz"
      next
    }
    if (in_chunk && grepl("^```\\s*$", l)) {
      in_chunk <- FALSE
      next
    }
    is_heading <- !in_chunk && grepl("^###? ", l)
    if (is_heading && grepl("^## ", l)) {
      topic <- .tute_heading(l)
      subtopic <- ""
    } else if (is_heading) {
      subtopic <- .tute_heading(l)
    }
    # HTML in the prose is layout, not something a learner reads
    if (!in_chunk && grepl("^\\s*<", l)) next
    if (matcher(l)) {
      keep[i] <- TRUE
      type[i] <- if (in_chunk) chunk else if (is_heading) "heading" else "text"
      section[i] <- if (nzchar(subtopic)) paste(topic, ">", subtopic) else topic
    }
  }
  if (!any(keep)) return(NULL)
  text <- trimws(lines[keep])
  dplyr::tibble(package = package, name = name, title = title,
                section = section[keep], type = type[keep], text = text)
}

.tute_chunk_type <- function(header) {
  label <- sub("^```\\{r[ ,]*([^ ,}=]*).*$", "\\1", header)
  if (grepl("-solution$", label)) "solution"
  else if (grepl("-hint(-[0-9]+)?$", label)) "hint"
  else if (grepl("exercise\\s*=\\s*(TRUE|T)\\b", header)) "exercise"
  else if (grepl("Q$", label)) "quiz"
  else "example"
}

.tute_heading <- function(line) {
  trimws(sub("\\s*\\{#[^}]*\\}\\s*$", "", sub("^#+\\s*", "", line)))
}
