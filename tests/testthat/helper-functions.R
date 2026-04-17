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
check_tute_rendering <- function(path, quiet = TRUE){
  
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

