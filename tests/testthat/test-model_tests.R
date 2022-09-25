# Making sure the tests family of functions works as intended.
marvel_friends <- to_giant(to_unsigned(ison_marvel_relationships)) %>%
  to_subgraph(PowerOrigin == "Human")
cugtest <- test_random(marvel_friends,
                       network_homophily,
                       attribute = "Attractive",
                       times = 200)
cugtest2 <- test_random(marvel_friends,
                        network_betweenness,
                        times = 200)

test_that("test_random works", {
  # Set the cugtest up
  # Test stuff cug1
  expect_equal(as.numeric(cugtest$testval), -0.85714, tolerance = 0.001)
  expect_equal(length(cugtest$testdist), 200) # NB: Stochastic
  expect_false(cugtest$mode)
  expect_false(cugtest$diag)
  expect_equal(cugtest$cmode, "csize")
  expect_equal(class(cugtest$plteobs), "numeric")
  expect_equal(class(cugtest$pgteobs), "numeric")
  expect_equal(cugtest$reps, 200)
  expect_s3_class(cugtest, "network_test")
  # Test stuff cug2
  expect_equal(as.numeric(cugtest2$testval), 0.238, tolerance = 0.001)
  expect_equal(length(cugtest2$testdist), 200) # NB: Stochastic
  expect_false(cugtest2$mode)
  expect_false(cugtest2$diag)
  expect_equal(cugtest2$cmode, "csize")
  expect_equal(round(cugtest2$plteobs), 1)
  expect_equal(round(cugtest2$pgteobs), 0)
  expect_equal(cugtest2$reps, 200)
  expect_s3_class(cugtest2, "network_test")
})

# Set the qaptest up
marvel_friends <- to_unsigned(ison_marvel_relationships)
marvel_friends <- to_giant(marvel_friends)
marvel_friends <- to_subgraph(marvel_friends, PowerOrigin == "Human")
qaptest <- test_permutation(marvel_friends,
                            network_homophily,
                            attribute = "Attractive",
                            times = 200)
test_that("test_permutation works", {
  expect_equal(as.numeric(qaptest$testval), -0.85714, tolerance = 0.001)
  expect_equal(length(qaptest$testdist), 200) # NB: Stochastic
  expect_equal(class(qaptest$plteobs), "numeric") # NB: Stochastic
  expect_equal(class(qaptest$pgteobs), "numeric") # NB: Stochastic
  expect_equal(qaptest$reps, 200)
  expect_s3_class(qaptest, "network_test")
})

cugplot <- plot(cugtest)
test_that("cug plot works", {
  expect_s3_class(cugplot, "gg")
  expect_is(cugplot$layers[[1]], "ggproto")
  expect_is(cugplot$layers[[1]]$geom, "GeomDensity")
  expect_is(cugplot$layers[[1]]$stat, "StatDensity")
  expect_identical(cugplot$labels$x, "Statistic")
  expect_identical(cugplot$labels$y, "Density")
})

qapplot <- plot(qaptest)
test_that("qap plot works", {
  expect_s3_class(qapplot, "gg")
  expect_is(qapplot$layers[[1]], "ggproto")
  expect_is(qapplot$layers[[1]]$geom, "GeomDensity")
  expect_is(qapplot$layers[[1]]$stat, "StatDensity")
  expect_identical(qapplot$labels$x, "Statistic")
  expect_identical(qapplot$labels$y, "Density")
})
