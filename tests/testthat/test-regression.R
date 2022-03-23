test <- network_reg(weight ~ ego(Discipline) + alter(Discipline), 
                     ison_eies, times = 500)
test2 <- network_reg(weight ~ ego(Discipline) + alter(Discipline), 
                     ison_eies, strategy = "multicore", times = 2000)

friends <- ison_marvel_relationships %>% 
  activate(edges) %>% 
  tidygraph::mutate(friend = (sign > 0)*1)
test_logit <- network_reg(friend ~ ego(Attractive) + alter(Intellect),
                          friends, times = 500)

test_that("network_reg estimates correctly",{
  set.seed(123)
  expect_s3_class(test, "netlm")
  expect_s3_class(test_logit, "netlogit")
  expect_equal(round(unname(test$coefficients),3), 
               c(22.925, -17.280, -13.758, -6.720, NA, NA, NA))
  expect_equal(round(unname(test$coefficients),1), 
               round(unname(test2$coefficients),1))
})

results <- tidy(test)
results2 <- glance(test)

test_that("tidy and glance work correctly for network_reg",{
  set.seed(123)
  expect_s3_class(results, "tbl_df")
  expect_s3_class(results2, "tbl_df")
  expect_equal(round(unname(results$estimate[1]), 3), 22.925)
  expect_equal(round(results2$r.squared, 4), 0.0142)
})
