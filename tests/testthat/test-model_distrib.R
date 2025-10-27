test_that("test_distribution works", {
  res <- test_distribution(as_diffusion(play_diffusion(ison_networkers)),
                    as_diffusion(play_diffusion(ison_networkers, thresholds = 75)))
  expect_output(print(res), "statistic")
})
