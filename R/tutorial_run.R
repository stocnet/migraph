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
#'   
#' @param tute String, name of the tutorial (e.g. "tutorial2").
#' @importFrom dplyr %>% as_tibble select tibble
#' @name tutorials
NULL

stocnet <- c("manynet", "migraph", "autograph")

#' @rdname tutorials 
#' @export
run_tute <- function(tute) {
  thisRequires("learnr")
  avail_pkgs <- stocnet[suppressWarnings(unlist(lapply(stocnet, 
                                                       function(x) nzchar(system.file(package = x)))))]
  if (missing(tute)) {
    tutelist <- lapply(manynet::snet_progress_along(avail_pkgs, 
                                               name = "Checking tutorials in stocnet packages"), 
                       function(p){
                         dplyr::as_tibble(learnr::available_tutorials(package = avail_pkgs[p]),
                                          silent = TRUE) %>% dplyr::select(1:3)
                       })
    dplyr::bind_rows(tutelist) %>% dplyr::arrange(name) %>% print()
    manynet::snet_info("You can run a tutorial by typing e.g `run_tute('tutorial1')` or `run_tute('Data')` into the console.")
  } else {
    try(learnr::run_tutorial(tute, "manynet"), silent = TRUE)
    try(learnr::run_tutorial(tute, "migraph"), silent = TRUE)
    try(learnr::run_tutorial(tute, "autograph"), silent = TRUE)
    manynet::snet_info("Didn't find a direct match, so looking for close matches...")
    tutelist <- lapply(manynet::snet_progress_along(avail_pkgs, 
                                               name = "Checking tutorials in stocnet packages"), function(p){
                                                 dplyr::as_tibble(learnr::available_tutorials(package = avail_pkgs[p]),
                                                                  silent = TRUE) %>% dplyr::select(1:3)
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
                                                                  silent = TRUE) %>% dplyr::select(1:3)
                                               })
    dplyr::bind_rows(tutelist) %>% dplyr::arrange(name) %>% print()
    manynet::snet_info("You can extract the code from one of these tutorials by typing e.g `extract_tute('tutorial1')` into the console.")
  } else {
    thisRequires("knitr")
    pth <- file.path(path.package("manynet"), "tutorials", tute)
    if(!dir.exists(pth)) {
      thisRequires("autograph")
      pth <- gsub("manynet", "autograph", pth)
    }
    if(!dir.exists(pth)) {
      thisRequires("migraph")
      pth <- gsub("autograph", "migraph", pth)
    }
    knitr::purl(file.path(pth, list.files(pth, pattern = "*.Rmd")),
                documentation = 1)
    utils::file.edit(gsub(".Rmd", ".R", list.files(pth, pattern = "*.Rmd")))
  }
}

