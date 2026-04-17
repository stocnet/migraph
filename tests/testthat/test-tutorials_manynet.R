test_that("manynet tutorials work", {
  for(tute in find_pkg_tutorial_paths("manynet")){
    expect_null(check_tute_rendering(tute))
  }
})