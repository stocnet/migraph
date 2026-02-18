# # Making sure the tests family of functions works as intended.
marvel_friends <- manynet::to_uniplex(manynet::fict_marvel, "relationship") %>% 
  manynet::to_giant() %>% manynet::to_unsigned() %>% 
  manynet::to_subgraph(PowerOrigin == "Human")
cugtest <- test_random(marvel_friends,
                       manynet::net_heterophily,
                       attribute = "Attractive",
                       times = 200)
cugtest2 <- test_random(marvel_friends,
                        manynet::net_betweenness,
                        times = 200)
cugtest3 <- test_random(ison_southern_women,
                        manynet::net_equivalency,
                        times = 200)

test_that("test_random works", {
  # Set the cugtest up
  # Test stuff cug1
  expect_equal(as.numeric(cugtest$testval), -0.85714, tolerance = 0.001)
  expect_equal(length(cugtest$testdist), 200) # NB: Stochastic
  expect_false(cugtest$mode)
  expect_false(cugtest$diag)
  expect_equal(cugtest$cmode, "edges")
  expect_equal(class(cugtest$plteobs), "numeric")
  expect_equal(class(cugtest$pgteobs), "numeric")
  expect_equal(cugtest$reps, 200)
  expect_s3_class(cugtest, "network_test")
  # Test stuff cug2
  expect_equal(as.numeric(cugtest2$testval), 0.2375, tolerance = 0.001)
  # expect_equal(mean(cugtest3$testdist), 0.3600, tolerance = 0.02)
  expect_equal(length(cugtest2$testdist), 200) # NB: Stochastic
  expect_false(cugtest2$mode)
  expect_false(cugtest2$diag)
  expect_equal(cugtest2$cmode, "edges")
  expect_equal(round(cugtest2$plteobs), 1)
  expect_equal(round(cugtest2$pgteobs), 0)
  expect_equal(cugtest2$reps, 200)
  expect_s3_class(cugtest2, "network_test")
})

# Set the qaptest up
qaptest <- test_permutation(marvel_friends,
                            manynet::net_heterophily,
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


test_that("test_configuration works", {
  testthat::skip_on_os("linux")
  configtest <- test_configuration(marvel_friends,
                                   manynet::net_heterophily,
                                   attribute = "Attractive",
                                   times = 200)
  expect_s3_class(configtest, "network_test")
  expect_equal(as.numeric(configtest$testval), -0.85714, tolerance = 0.001)
  expect_equal(length(configtest$testdist), 200) # NB: Stochastic
  expect_equal(class(configtest$plteobs), "numeric") # NB: Stochastic
  expect_equal(class(configtest$pgteobs), "numeric") # NB: Stochastic
})


# cugplot <- plot(cugtest)
# test_that("cug plot works", {
#   expect_s3_class(cugplot, "gg")
#   expect_s3_class(cugplot$layers[[1]], "ggproto")
#   expect_s3_class(cugplot$layers[[1]]$geom, "GeomDensity")
#   expect_s3_class(cugplot$layers[[1]]$stat, "StatDensity")
#   labels <- if ("get_labs" %in% getNamespaceExports("ggplot2")) {
#     ggplot2::get_labs(cugplot)
#   } else {
#     cugplot$labels
#   }
#   expect_identical(labels$x, "Statistic")
#   expect_identical(labels$y, "Density")
# })
# 
# qapplot <- plot(qaptest)
# test_that("qap plot works", {
#   expect_s3_class(qapplot, "gg")
#   expect_s3_class(qapplot$layers[[1]], "ggproto")
#   expect_s3_class(qapplot$layers[[1]]$geom, "GeomDensity")
#   expect_s3_class(qapplot$layers[[1]]$stat, "StatDensity")
#   labels <- if ("get_labs" %in% getNamespaceExports("ggplot2")) {
#     ggplot2::get_labs(cugplot)
#   } else {
#     cugplot$labels
#   }
#   expect_identical(labels$x, "Statistic")
#   expect_identical(labels$y, "Density")
# })
