test_that("manynet tutorials work", {
  for(tute in find_pkg_tutorial_paths("manynet")){
    expect_null(check_tute_rendering(tute))
  }
})

test_that("manynet tutorial code runs without warnings or errors", {
  skip_if_not_installed("manynet", minimum_version = "2.0.2")
  for(tute in find_pkg_tutorial_paths("manynet")){
    expect_null(check_tute_functions(tute), 
                info = paste("Error in tutorial", basename(tute)))
  }
})
