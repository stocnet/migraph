sw1 <- as.matrix(c("E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", "E11", "E12", "E13", "E14"), nrow = 14)
sw2 <- as.matrix(c("48", "48", "34", "39", "27", "24", "19", "17", "16", "33", "31", "29", "43","43"), nrow = 14) 
swc <- as.matrix(cbind(sw1, sw2))

test_that("constraint scores are reported correctly for two-mode notworks",{
expect_equal(constraint(southern_women), swc)
})


om <- igraph::graph(edges = c(1,2, 2,3), n = 4, directed = FALSE) 

test_that("constraint scores are reported correctly for one-mode notworks",{
  expect_equal(constraint(om), c(1.0, 0.5, 1.0, NaN))
})

