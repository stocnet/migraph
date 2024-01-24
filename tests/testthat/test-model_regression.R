set.seed(123)
networkers <- manynet::ison_networkers %>% manynet::to_subgraph(Discipline == "Sociology")
netsenders <- manynet::to_unweighted(networkers)

test <- network_reg(weight ~ ego(Citations),
                     networkers, times = 50)
test_logit <- network_reg(. ~ ego(Citations),
                          netsenders, times = 50)

test_that("network_reg estimates correctly",{
  expect_s3_class(test, "netlm")
  expect_equal(top3(test$coefficients,3),
               c(26.811, -0.379, NA))
  expect_s3_class(test_logit, "netlogit")
  expect_equal(top3(test_logit$coefficients,3),
               c(0.146, -0.024, NA))
})

test_that("network_reg tests correctly",{
  expect_equal(top3(test$pgreqabs, 2),
               c(0.14, 0.32, NA), tolerance = 0.1)
  expect_equal(top3(test_logit$pgreqabs,2),
               c(0.9, 0.2, NA), tolerance = 0.1)
})

tidys <- tidy(test)
test_that("tidy works correctly for network_reg",{
  expect_s3_class(tidys, "tbl_df")
  expect_equal(top3(tidys$estimate, 2), c(26.81, -0.38, NA))
})

glances <- glance(test)
test_that("glance works correctly for network_reg",{
  expect_s3_class(glances, "tbl_df")
  expect_equal(top3(glances$r.squared, 3), c(0.021, NA, NA))
})

plots <- plot(test)
test_that("plot works correctly for network_reg",{
  expect_s3_class(plots, "gg")
  expect_equal(plots$labels$x, "Statistic")
})
