# The tests, the regression, and the diffusion simulations moved to {infernet}.
# The functions that kept their name come from {infernet} directly. The renamed
# ones are shims, which warn once a session, and give what {infernet} gives.

reset_moved <- function(old) {
  rm(list = intersect(old, ls(migraph:::.migraph_state)),
     envir = migraph:::.migraph_state)
}

networkers <- manynet::ison_networkers |>
  manynet::to_subgraph(Discipline == "Sociology")

test_that("net_regression() calls estimate_mrqap(), and warns once", {
  reset_moved("net_regression")
  set.seed(1)
  expect_warning(
    old <- suppressMessages(net_regression(weight ~ ego(Citations) + alter(Citations),
                                           networkers, times = 5)),
    "infernet::estimate_mrqap")
  set.seed(1)
  new <- suppressMessages(infernet::estimate_mrqap(
    weight ~ ego(Citations) + alter(Citations), networkers, times = 5))
  expect_s3_class(old, "netreg")
  expect_equal(old$coefficients, new$coefficients)
  expect_equal(old$permute, "predictor")
  expect_no_warning(
    qapy <- suppressMessages(net_regression(weight ~ ego(Citations) + alter(Citations),
                                            networkers, method = "qapy", times = 5)))
  expect_equal(qapy$permute, "outcome")
})

test_that("the functions that kept their name are {infernet}'s", {
  for (fn in c("test_random", "test_configuration", "test_permutation",
               "play_diffusions", "test_gof", "tidy", "glance")) {
    expect_false(exists(fn, envir = asNamespace("migraph"), inherits = FALSE),
                 info = fn)
  }
})

test_that("test_fit() and test_distribution() call {infernet}'s", {
  reset_moved(c("test_fit", "test_distribution"))
  set.seed(1)
  sims <- infernet::play_diffusions(manynet::ison_networkers, times = 40)
  others <- infernet::play_diffusions(manynet::ison_networkers,
                                      thresholds = 75, times = 40)
  observed <- manynet::as_diffusion(manynet::play_diffusion(manynet::ison_networkers))
  expect_warning(fit <- test_fit(observed, sims), "infernet::test_gof")
  expect_s3_class(fit, "network_test")
  expect_warning(dist <- test_distribution(sims, others),
                 "infernet::test_distributions")
  expect_s3_class(dist, "network_test")
})

test_that("predict() works on what net_regression() returns", {
  model <- suppressWarnings(suppressMessages(
    net_regression(weight ~ ego(Citations), networkers, times = 5)))
  expect_length(predict(model, data.frame(ego_Citations = c(1, 10))), 2)
})
