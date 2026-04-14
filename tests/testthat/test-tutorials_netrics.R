test_that("netrics tutorials work", {
  for(tute.dir in list.dirs(system.file("tutorials", package = "netrics"), 
                            recursive = F)){
    tute.file <- list.files(tute.dir, pattern = "*.Rmd", full.names = T)
    expect_null(test_tutorials(tute.file))
  }
})