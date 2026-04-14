test_that("test_distribution works", {
  res <- test_distribution(as_diffusion(play_diffusion(ison_networkers)),
                    as_diffusion(play_diffusion(ison_networkers, thresholds = 75)))
  expect_output(print(res), "statistic")
})

test_that("test_fit works", {
  x <- play_diffusion(generate_random(15), transmissibility = 0.7)
  y <- play_diffusions(generate_random(15), transmissibility = 0.1, times = 40)
  res <- test_fit(as_diffusion(x), y)
  expect_output(print(res), "statistic")
})