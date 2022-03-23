set.seed(123)
test <- network_reg(weight ~ ego(Discipline) + alter(Citations), 
                     ison_eies, times = 200)

friends <- ison_marvel_relationships %>% 
  activate(edges) %>% 
  tidygraph::mutate(friend = (sign > 0)*1)
test_logit <- network_reg(friend ~ ego(Attractive) + alter(Intellect),
                          friends, times = 200)

test_that("network_reg estimates correctly",{
  set.seed(123)
  expect_s3_class(test, "netlm")
  expect_s3_class(test_logit, "netlogit")
  expect_equal(round(unname(test$coefficients),3), 
               c(27.196, -18.251, -3.417, -11.831, -0.103))
})

results <- tidy(test)
results2 <- glance(test)

test_that("tidy and glance work correctly for network_reg",{
  expect_s3_class(results, "tbl_df")
  expect_s3_class(results2, "tbl_df")
  expect_equal(round(unname(results$estimate[1]), 3), 27.196)
  expect_equal(round(results2$r.squared, 4), 0.0260)
})
