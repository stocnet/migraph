# test_that("netrics tutorials work", {
#   for(tute in find_pkg_tutorial_paths("netrics")){
#     expect_null(check_tute_rendering(tute))
#   }
# })

test_that("netrics tutorial code runs without warnings or errors", {
  skip_if_not_installed("netrics", minimum_version = "0.2.2")
  for(tute in find_pkg_tutorial_paths("netrics")){
    expect_null(check_tute_functions(tute), 
                info = paste("Error in tutorial", basename(tute)))
  }
})