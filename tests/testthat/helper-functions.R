knitr::opts_chunk$set(message = FALSE)

top3 <- function(res, dec = 4){
  if(is.numeric(res)){
    unname(round(res, dec))[1:3]  
  } else unname(res)[1:3]
}

bot3 <- function(res, dec = 4){
  lr <- length(res)
  if(is.numeric(res)){
    unname(round(res, dec))[(lr-2):lr]
  } else unname(res)[(lr-2):lr]
}

top5 <- function(res, dec = 4){
  if(is.numeric(res)){
    unname(round(res, dec))[1:5]
  } else unname(res)[1:3]
}

bot5 <- function(res, dec = 4){
  lr <- length(res)
  if(is.numeric(res)){
    unname(round(res, dec))[(lr-4):lr]
  } else unname(res)[(lr-2):lr]
}

find_pkg_tutorial_paths <- function(pkg) {
  tute_folders <- list.dirs(system.file("tutorials", package = pkg),
                             recursive = F)
  tute_files <- unlist(lapply(tute_folders, function(folder) {
    list.files(folder, pattern = "*.Rmd", full.names = TRUE)
  }))
  tute_files
}

check_tute_rendering <- function(path, quiet = TRUE){
  
  skip_if_not_installed("rmarkdown")
  stopifnot(all(file.exists(path)))
  
  for(i in path){
    if(!quiet) message("Rendering: ", basename(i))
    tryCatch({
      rmarkdown::render(input = i, 
                        output_dir = tempdir(),
                        intermediates_dir = tempdir(), quiet = quiet)
      # Note that the Debian setup on CRAN does not allow for writing files to any
      # location other than the temporary directory, which is why we must specify
      # tempdir() in the two dir arguments.
      if(!quiet) message("Successfully rendered: ", basename(i))
    }, error = function(e) {
      stop("Failed to render ", i, ": ", e$message, call. = FALSE)
    })
  }
  invisible(NULL)
}

check_tute_functions <- function(path, skip = "ergm\\(", quiet = TRUE){
  tmp <- tempfile(fileext = ".R")
  knitr::purl(
    input  = path,
    output = tmp,
    quiet  = quiet
  )
  exprs <- parse(tmp)  # your purled file
  env <- new.env(parent = globalenv())
  
  skip_rest <- FALSE
  is_skipped_call <- function(expr) {
    any(grepl(skip, deparse(expr)))
  }
  
  for (i in seq_along(exprs)) {
    if (skip_rest) {
      skip(paste("Skipping dependent expressions in", basename(path)))
      next
    }
    
    if (is_skipped_call(exprs[[i]])) {
      skip_rest <- TRUE
      skip(paste("Skipping slow functions in", basename(path)))
      next
    }
    
    w <- NULL
    e <- NULL
    m <- NULL
    
    not_out <- withCallingHandlers(
      tryCatch(
        eval(exprs[[i]], envir = env),
        error = function(err) {
          e <<- err
          NULL
        }
      ),
      warning = function(wrn) {
        w <<- wrn
        invokeRestart("muffleWarning")
      },
      message = function(msg) {
        m <<- c(m, conditionMessage(msg))
        invokeRestart("muffleMessage")
      }
    )
    
    # If there *was* a warning, check if it's a deprecated/defunct one
    if (!is.null(w)) {
      msg <- conditionMessage(w)
      
      # Only fail if it's a deprecated/defunct warning
      if (!grepl("deprecate|defunct|moved", msg, ignore.case = TRUE)) {
        w <- NULL
      }
    }
    
    # Now test what happened
    expect_null(
      e,
      info = paste0("Error in expression ", i,
                    " of ", basename(path), ": ", deparse(exprs[[i]]))
    )
    
    expect_null(
      w,
      info = paste("Warning in expression", i, ":", deparse(exprs[[i]]))
    )
  }
}
