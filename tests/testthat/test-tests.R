# Making sure the tests family of functions works as intended.

test_that("test_random works", {
  # Set the cugtest up
  marvel_friends <- to_unsigned(ison_marvel_relationships)
  marvel_friends <- to_main_component(marvel_friends)
  marvel_friends <- dplyr::filter(marvel_friends, PowerOrigin == "Human")
  cugtest <- test_random(marvel_friends,
                          graph_ei_index,
                          attribute = "Attractive",
                          nSim = 200)
  cugtest2 <- test_random(marvel_friends,
                         graph_betweenness,
                         nSim = 200)
  # Test stuff cug1
  expect_equal(round(cugtest$obs.stat, 3), -0.857)
  expect_equal(length(cugtest$rep.stat), 200) # NB: Stochastic
  expect_false(cugtest$mode)
  expect_false(cugtest$diag)
  expect_equal(cugtest$cmode, "csize")
  expect_equal(class(cugtest$plteobs), "numeric")
  expect_equal(class(cugtest$pgteobs), "numeric")
  expect_equal(cugtest$reps, 200)
  expect_equal(attributes(cugtest)$class, "cug.test")
  # Test stuff cug2
  expect_equal(round(cugtest2$obs.stat, 2), 0.24)
  expect_equal(length(cugtest2$rep.stat), 200) # NB: Stochastic
  expect_false(cugtest2$mode)
  expect_false(cugtest2$diag)
  expect_equal(cugtest2$cmode, "csize")
  expect_equal(cugtest2$plteobs, 1)
  expect_equal(cugtest2$pgteobs, 0)
  expect_equal(cugtest2$reps, 200)
  expect_equal(attributes(cugtest2)$class, "cug.test")
})

test_that("test_permutation works", {
  # Set the qaptest up
  marvel_friends <- to_unsigned(ison_marvel_relationships)
  marvel_friends <- to_main_component(marvel_friends)
  marvel_friends <- dplyr::filter(marvel_friends, PowerOrigin == "Human")
  qaptest <- test_permutation(marvel_friends,
                              graph_ei_index,
                              attribute = "Attractive",
                              nSim = 200)
  qaptest2 <- test_permutation(marvel_friends,
                              graph_betweenness,
                              nSim = 200)
  # Test stuff qap1
  expect_equal(round(qaptest$testval, 3), -0.857)
  expect_equal(length(qaptest$dist), 200) # NB: Stochastic
  expect_equal(class(qaptest$plteobs), "numeric") # NB: Stochastic
  expect_equal(class(qaptest$pgteobs), "numeric") # NB: Stochastic
  expect_equal(qaptest$reps, 200)
  expect_equal(attributes(qaptest)$class, "qap.test")
  # Test stuff qap2
  expect_equal(round(qaptest2$testval, 2), 0.24)
  expect_equal(length(qaptest2$dist), 200) # NB: Stochastic
  expect_equal(round(qaptest2$plteobs, 2), 1)
  expect_equal(round(qaptest2$pgteobs, 2), 1)
  expect_equal(qaptest2$reps, 200)
  expect_equal(attributes(qaptest2)$class, "qap.test")
})
