set.seed(123)
networkers <- ison_networkers %>% to_subgraph(Discipline == "Sociology") %>% 
  activate(edges) %>% mutate(messaged = 1) %>% activate(nodes)

test <- network_reg(weight ~ alter(Citations) + sim(Citations), 
                     networkers, times = 60)
test_logit <- network_reg(messaged ~ alter(Citations) + sim(Citations), 
                          networkers, times = 60)

test_that("network_reg estimates correctly",{
  expect_s3_class(test, "netlm")
  expect_equal(round(unname(test$coefficients),3), 
               c(-8.470, -0.125, 44.871))
  expect_s3_class(test_logit, "netlogit")
  expect_equal(round(unname(test_logit$coefficients),3), 
               c(-2.179, 0.000, 2.632))
})

test_that("network_reg tests correctly",{
  expect_equal(test$pgreqabs, 
               c(0.65, 0.57, 0.05), tolerance = 0.1)
  expect_equal(test_logit$pgreqabs, 
               c(0.00, 0.98, 0.00), tolerance = 0.1)
})

tidys <- tidy(test)
test_that("tidy works correctly for network_reg",{
  expect_s3_class(tidys, "tbl_df")
  expect_equal(round(unname(tidys$estimate[1]), 3), -8.47)
})

glances <- glance(test)
test_that("glance works correctly for network_reg",{
  expect_s3_class(glances, "tbl_df")
  expect_equal(round(glances$r.squared, 4), 0.0575)
})

plots <- plot(test)
test_that("plot works correctly for network_reg",{
  expect_s3_class(plots, "gg")
  expect_equal(plots$labels$x, "Statistic")
})
