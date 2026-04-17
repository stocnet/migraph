# test_that("autograph tutorials work", {
#     for(tute in find_pkg_tutorial_paths("autograph")){
#       expect_null(check_tute_rendering(tute))
#     }
# })

test_that("autograph tutorial code runs without warnings or errors", {
  skip_if_not_installed("autograph", minimum_version = "1.0.0")
  for(tute in find_pkg_tutorial_paths("autograph")){
    expect_null(check_tute_functions(tute), 
                info = paste("Error in tutorial", basename(tute)))
  }
})

