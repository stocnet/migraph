test_that("Plotting function visualises historical milestones/releases of a repository", {
  # skip_on_os("mac")
  # testplot <- plot_releases("snlab-ch/migraph")
  testdf <- data.frame(tag_name = c("v0.1.0","v0.1.1"),
                       date = as.Date(c("2021-04-01","2021-05-01")),
                       milestone = c("Minor","Patch"))
  testplot <- plot_releases(testdf)
  expect_true(is.list(testplot))
  expect_length(testplot, 9)
  expect_named(testplot[1:3], c("data", "layers", "scales"))
})
