#' Test tutorials
#'
#' @description 
#'   For our purposes, "testing" a tutorial means being able to 
#'   (successfully) run `render()` on it. 
#'   This function renders the tutorial provided in `path`. 
#'   There is no check to see if the rendered file looks OK.
#'   If a tutorial fails to render, then an error will be generated which will
#'   propagate to the caller.
#' @param path Character vector of the paths to the tutorials to be knitted.
#' @param quiet Logical, whether to suppress messages from `render()`.
#' @returns No return value, called for side effects.
#' @author David Kane, see tutorial.helpers
#' @export
test_tutorials <- function(path, quiet = TRUE){
  
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