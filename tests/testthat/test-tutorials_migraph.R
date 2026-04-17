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
