library(tidygraph)
data(southern_women, package = "networkdata")
southern_women <- as_tbl_graph(southern_women)

# test <- southern_women %>% as_tbl_graph %>% activate(nodes) %>% 
#   mutate(degree = roctopus::centrality_degree(normalized = T))

test_that("two mode degree centrality calculated correctly",{
  expect_equal(unname(with_graph(southern_women, 
                          roctopus::centrality_degree())[1:5]), 
               c(8,7,8,7,4))
  expect_equal(unname(with_graph(southern_women, 
                          roctopus::centrality_degree())[28:32]), 
               c(6,4,7,4,4))
  expect_equal(unname(round(with_graph(southern_women, 
                          roctopus::centrality_degree(normalized = T))[1:5],4)), 
               c(0.5714, .5, .5714, .5, .2857))
  expect_equal(unname(round(with_graph(southern_women, 
                                roctopus::centrality_degree(normalized = T))[28:32],4)), 
               c(0.3333, .2222, .3889, .2222, .2222))
})

test_that("two mode closeness centrality calculated correctly",{
  expect_equal(unname(round(with_graph(southern_women, 
                                       roctopus::centrality_closeness())[1:5], 4)), 
               c(0.0167, 0.0152, 0.0167, 0.0152, 0.0125))
  expect_equal(unname(round(with_graph(southern_women, 
                                       roctopus::centrality_closeness())[28:32], 4)), 
               c(0.0128, 0.0122, 0.0132, 0.0122, 0.0122))
  # The tests above check closeness centrality unnormalized. The results displayed from Borgatti and Everett (1997) 
  # are normalized closeness scores. These are c(51.67,46.97,51.67,46.97,38.75)) for the first five and
  # c(39.74,37.80,40.79,37.80,37.00)) for the last five. 
  expect_equal(unname(round(with_graph(southern_women, 
                                       roctopus::centrality_closeness(normalized = T))[1:5],4)), 
               c(0.8000, 0.7273, 0.8000, 0.7273, 0.6000))
  expect_equal(unname(round(with_graph(southern_women, 
                                       roctopus::centrality_closeness(normalized = T))[28:32],4)), 
               c(0.5641, 0.5366, 0.5789, 0.5366, 0.5366))
})

test_that("two mode betweenness centrality calculated correctly",{
  expect_equal(unname(with_graph(southern_women, 
                                 roctopus::centrality_betweenness())[1:5]), 
               c(42.759998, 22.856540, 38.739264, 22.011910, 4.727942))
  expect_equal(unname(round(with_graph(southern_women, 
                                 roctopus::centrality_betweenness())[28:32],2)), 
               c(6.82, 9.02, 10.24, 1.89, 1.89))
  expect_equal(unname(round(with_graph(southern_women, 
                                       roctopus::centrality_betweenness(normalized = T))[1:5],4)), 
               c(0.0967, 0.0517, 0.0876, 0.0498, 0.0107))
  expect_equal(unname(round(with_graph(southern_women, 
                                       roctopus::centrality_betweenness(normalized = T))[28:32],4)), 
               c(0.0151, 0.02, 0.0226, 0.0042, 0.0042))
})

#test_that("two mode eigenvector centrality calculated correctly",{
#  expect_equal(unname(with_graph(southern_women, 
#                                 roctopus::centrality_eigenvector())[1:5]), 
#               c(0.22, 0.2, 0.25, 0.21, 0.11))
#  expect_equal(unname(with_graph(southern_women, 
#                                 roctopus::centrality_eigenvector())[28:32]), 
#               c(0.15, 0.07, 0.17, 0.11, 0.11))
#  expect_equal(unname(round(with_graph(southern_women, 
#                                       roctopus::centrality_eigenvector(normalized = T))[1:5],4)), 
#               c(32.71, 30.14, 36.44, 30.49, 16.19))
#  expect_equal(unname(round(with_graph(southern_women, 
#                                       roctopus::centrality_eigenvector(normalized = T))[28:32],4)), 
#               c(21.73, 10.03, 24.98, 15.92, 15.92))
#})
