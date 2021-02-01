test_that("small-world metrics for two mode networks are calculated and displayed correctly", {
  expect_length(smallworld(southern_women), 8)
  expect_named(smallworld(southern_women), c("Num", "ObsClust", "ExpClust", "ClustRat",
                                             "ObsPath", "ExpPath", "PathRat", "SmallWorld"))
})
