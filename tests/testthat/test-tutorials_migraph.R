# test_that("migraph tutorials render", {
#   skip_on_cran()
#   for(tute.dir in list.dirs(system.file("tutorials", package = "migraph"), 
#                             recursive = F)){
#     tute.file <- list.files(tute.dir, pattern = "*.Rmd", full.names = T)
#     expect_null(check_tute_rendering(tute.file))
#   }
# })

test_that("migraph tutorial code runs without warnings or errors", {
  for(tute in find_pkg_tutorial_paths("migraph")){
    expect_null(check_tute_functions(tute, skip = "ergm\\(|play_diffusions\\("), 
                info = paste("Error in tutorial", basename(tute)))
  }
})

# test_that("migraph tutorial code runs without warnings or errors", {
#   migraph_tutes <- list.dirs(system.file("tutorials", package = "migraph"), 
#             recursive = F)
#   migraph_tutes <- migraph_tutes[!grepl("tutorial9", migraph_tutes)]
#   for(tute.dir in migraph_tutes){
#     tute.file <- list.files(tute.dir, pattern = "*.Rmd", full.names = T)
#     tmp <- tempfile(fileext = ".R")
#     knitr::purl(
#       input  = tute.file,
#       output = tmp,
#       quiet  = TRUE
#     )
#     exprs <- parse(tmp)  # your purled file
#     env <- new.env(parent = globalenv())
#     
#     skip_rest <- FALSE
#     is_ergm_call <- function(expr) {
#       any(grepl("ergm\\(", deparse(expr)))
#     }
#     
#     for (i in seq_along(exprs)) {
#       
#       if (skip_rest) {
#         skip(paste("Skipping dependent expressions in", basename(tute.file)))
#         next
#       }
#       
#       if (is_ergm_call(exprs[[i]])) {
#         skip_rest <- TRUE
#         skip(paste("Skipping ergm model in", basename(tute.file)))
#         next
#       }
#       
#       w <- NULL
#       e <- NULL
#       
#       not_out <- testthat::capture_output(withCallingHandlers(
#         tryCatch(
#           eval(exprs[[i]], envir = env),
#           error = function(err) {
#             e <<- err
#             NULL
#           }
#         ),
#         warning = function(wrn) {
#           w <<- wrn
#           invokeRestart("muffleWarning")
#         }
#       ))
#       
#       # If there *was* a warning, check if it's a deprecated/defunct one
#       if (!is.null(w)) {
#         msg <- conditionMessage(w)
#         
#         # Only fail if it's a deprecated/defunct warning
#         if (grepl("deprecated|defunct|moved", msg, ignore.case = TRUE)) {
#           fail(paste(
#             "Deprecated/defunct warning in expression", i, ":\n",
#             deparse(exprs[[i]]), "\nMessage:", msg
#           ))
#         }
#       }
#       
#       
#       # Now test what happened
#       expect_null(
#         e,
#         info = paste0("Error in expression ", i, 
#                      " of ", basename(tute.file), ": ", deparse(exprs[[i]]))
#       )
#     }
#   }
# })