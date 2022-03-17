test <- network_reg(weight ~ ego(Discipline) + alter(Discipline), 
                    ison_eies, times = 500)

test_that("network_reg estimates correctly",{
  set.seed(123)
  expect_s3_class(test, "netlm")
  expect_equal(round(unname(test$coefficients),3), 
               c(22.925, -17.280, -13.758, -6.720, NA, NA, NA))
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
