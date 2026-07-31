set.seed(123)
networkers <- manynet::ison_networkers %>% manynet::to_subgraph(Discipline == "Sociology")
netsenders <- manynet::to_unweighted(networkers)

test <- net_regression(weight ~ ego(Citations),
                     networkers, times = 50)
test_logit <- net_regression(. ~ ego(Citations),
                          netsenders, times = 50)

test_that("network_reg estimates correctly",{
  expect_s3_class(test, "netlm")
  expect_equal(top3(test$coefficients,3),
               c(26.811, -0.379, NA))
  expect_s3_class(test_logit, "netlogit")
  expect_equal(top3(test_logit$coefficients,3),
               c(0.146, -0.024, NA))
  expect_output(print(test), "Model summary statistics")
  expect_output(print(test_logit), "Model summary statistics")
})

test_that("network_reg tests correctly",{
  expect_equal(top3(test_logit$pgreqabs,2),
               c(0.8, 0.18, NA), tolerance = 0.1)
})

test_that("tidy works correctly for network_reg",{
  tidys <- tidy(test)
  expect_s3_class(tidys, "tbl_df")
  expect_equal(top3(tidys$estimate, 2), c(26.81, -0.38, NA))
  tidys <- tidy(test_logit)
  expect_output(print(tidys), "term")
})

test_that("glance works correctly for network_reg",{
  glances <- glance(test)
  expect_s3_class(glances, "tbl_df")
  expect_equal(top3(glances$r.squared, 3), c(0.021, NA, NA))
  glances <- glance(test_logit)
  expect_output(print(glances), "AIC")
})

test_that("multivariate QAP works",{
  expect_s3_class(net_regression(weight ~ ego(Citations) + alter(Citations) + sim(Citations),
                                networkers, times = 10),
                 "netlm")
})

test_that("permutation shortcut matches manynet::to_permuted()",{
  # Tests the shortcut directly rather than via `permute_matrix()`, so that it
  # keeps guarding the equivalence once the version check defers to manynet
  onemode <- matrix(rpois(36, 3), 6, 6)
  labelled <- matrix(rbinom(36, 1, 0.4), 6, 6,
                     dimnames = list(LETTERS[1:6], LETTERS[1:6]))
  twomode <- matrix(rbinom(24, 1, 0.4), 6, 4)
  for(m in list(onemode, labelled, twomode)){
    set.seed(7)
    expected <- manynet::to_permuted(m, with_attr = FALSE)
    set.seed(7)
    expect_equal(permute_matrix_directly(m), expected, ignore_attr = FALSE)
  }
})

test_that("permutation defers to manynet once it has a matrix method",{
  expect_type(manynet_permutes_matrices(), "logical")
  expect_equal(manynet_permutes_matrices(),
               !is.null(utils::getS3method("to_permuted", "matrix",
                                           optional = TRUE)))
  # The cache is what `permute_matrix()` actually branches on, so check both
  # branches rather than only the one the installed manynet happens to select
  cached <- environment(manynet_permutes_matrices)
  on.exit(assign("delegate", NA, envir = cached), add = TRUE)
  m <- matrix(rpois(36, 3), 6, 6)

  assign("delegate", TRUE, envir = cached)
  set.seed(7); delegated <- permute_matrix(m)
  set.seed(7); expect_equal(delegated,
                            manynet::to_permuted(m, with_attr = FALSE),
                            ignore_attr = FALSE)

  assign("delegate", FALSE, envir = cached)
  set.seed(7); shortcut <- permute_matrix(m)
  set.seed(7); expect_equal(shortcut, permute_matrix_directly(m))
})

# test_that("specification advice appears",{
#   expect_message(net_regression(weight ~ same(Discipline), networkers, times = 1),
#                  "When testing for homophily")
# })

# plots <- plot(test)
# test_that("plot works correctly for network_reg",{
#   expect_s3_class(plots, "gg")
#   expect_equal(plots$labels$x, "Statistic")
# })
