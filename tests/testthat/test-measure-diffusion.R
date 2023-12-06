
test_that("node thresholds are identified correctly", {
  dm <- play_diffusion(generate_scalefree(ison_networkers),
                       seeds = 1,
                       thresholds = 0.25,
                       steps = 10)
  expect_equal(length(node_thresholds(dm)), network_nodes(generate_scalefree(ison_networkers)))
})
