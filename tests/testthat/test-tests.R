# Making sure the tests family of functions works as intended.
marvel_friends <- to_main_component(to_unsigned(ison_marvel_relationships)) %>%
  dplyr::filter(PowerOrigin == "Human")
cugtest <- test_random(marvel_friends,
                       graph_ei_index,
                       attribute = "Attractive",
                       times = 200)
cugtest2 <- test_random(marvel_friends,
                        graph_betweenness,
                        times = 200)

test_that("test_random works", {
  # Set the cugtest up
  # Test stuff cug1
  expect_equal(round(cugtest$obs.stat, 3), -0.857)
  expect_equal(length(cugtest$rep.stat), 200) # NB: Stochastic
  expect_false(cugtest$mode)
  expect_false(cugtest$diag)
  expect_equal(cugtest$cmode, "csize")
  expect_equal(class(cugtest$plteobs), "numeric")
  expect_equal(class(cugtest$pgteobs), "numeric")
  expect_equal(cugtest$reps, 200)
  expect_equal(attributes(cugtest)$class, "cug_test")
  # Test stuff cug2
  expect_equal(round(cugtest2$obs.stat, 2), 0.24)
  expect_equal(length(cugtest2$rep.stat), 200) # NB: Stochastic
  expect_false(cugtest2$mode)
  expect_false(cugtest2$diag)
  expect_equal(cugtest2$cmode, "csize")
  expect_equal(round(cugtest2$plteobs), 1)
  expect_equal(round(cugtest2$pgteobs), 0)
  expect_equal(cugtest2$reps, 200)
  expect_equal(attributes(cugtest2)$class, "cug_test")
})

# Set the qaptest up
marvel_friends <- to_unsigned(ison_marvel_relationships)
marvel_friends <- to_main_component(marvel_friends)
marvel_friends <- dplyr::filter(marvel_friends, PowerOrigin == "Human")
qaptest <- test_permutation(marvel_friends,
                            graph_ei_index,
                            attribute = "Attractive",
                            times = 200)
qaptest2 <- test_permutation(marvel_friends,
                             graph_betweenness,
                             times = 200)
test_that("test_permutation works", {
  # Test stuff qap1
  expect_equal(round(qaptest$testval, 3), -0.857)
  expect_equal(length(qaptest$dist), 200) # NB: Stochastic
  expect_equal(class(qaptest$plteobs), "numeric") # NB: Stochastic
  expect_equal(class(qaptest$pgteobs), "numeric") # NB: Stochastic
  expect_equal(qaptest$reps, 200)
  expect_equal(attributes(qaptest)$class, "qap_test")
  # Test stuff qap2
  expect_equal(round(qaptest2$testval, 2), 0.24)
  expect_equal(length(qaptest2$dist), 200) # NB: Stochastic
  expect_equal(round(qaptest2$plteobs, 2), 1)
  expect_equal(round(qaptest2$pgteobs, 2), 1)
  expect_equal(qaptest2$reps, 200)
  expect_equal(attributes(qaptest2)$class, "qap_test")
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
