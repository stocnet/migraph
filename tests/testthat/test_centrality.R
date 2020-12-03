library(tidygraph)
data(southern_women, package = "networkdata")

# test <- southern_women %>% as_tbl_graph %>% activate(nodes) %>% 
#   mutate(degree = roctopus::centrality_degree(normalized = T))

test_that("two mode degree centrality calculated correctly",{
  expect_equal(unname(with_graph(as_tbl_graph(southern_women), 
                          roctopus::centrality_degree())[1:5]), 
               c(8,7,8,7,4))
  expect_equal(unname(with_graph(as_tbl_graph(southern_women), 
                          roctopus::centrality_degree())[28:32]), 
               c(6,4,7,4,4))
  expect_equal(unname(round(with_graph(as_tbl_graph(southern_women), 
                          roctopus::centrality_degree(normalized = T))[1:5],4)), 
               c(0.5714, .5, .5714, .5, .2857))
  expect_equal(unname(round(with_graph(as_tbl_graph(southern_women), 
                                roctopus::centrality_degree(normalized = T))[28:32],4)), 
               c(0.3333, .2222, .3889, .2222, .2222))
})

test_that("two mode closeness centrality calculated correctly",{
  expect_equal(unname(with_graph(as_tbl_graph(southern_women), 
                                 roctopus::centrality_closeness())[1:5]), 
               c(51.67,46.97,51.67,46.97,38.75))
  expect_equal(unname(with_graph(as_tbl_graph(southern_women), 
                                 roctopus::centrality_closeness())[28:32]), 
               c(39.74,37.80,40.79,37.80,37.00))
  expect_equal(unname(round(with_graph(as_tbl_graph(southern_women), 
                                       roctopus::centrality_closeness(normalized = T))[1:5],4)), 
               c(80.00, 72.73, 80.00, 72.73, 60.00))
  expect_equal(unname(round(with_graph(as_tbl_graph(southern_women), 
                                       roctopus::centrality_closeness(normalized = T))[28:32],4)), 
               c(56.42, 53.66, 57.89, 53.66, 53.66))
})

test_that("two mode betweenness centrality calculated correctly",{
  expect_equal(unname(with_graph(as_tbl_graph(southern_women), 
                                 roctopus::centrality_betweenness())[1:5]), 
               c(42.759998, 22.856540, 38.739264, 22.011910, 4.727942))
  expect_equal(unname(with_graph(as_tbl_graph(southern_women), 
                                 roctopus::centrality_betweenness())[28:32]), 
               c(6.818566, 9.019440, 10.235365, 1.889229, 1.889229))
  expect_equal(unname(round(with_graph(as_tbl_graph(southern_women), 
                                       roctopus::centrality_betweenness(normalized = T))[1:5],4)), 
               c(9.67, 5.17, 8.76, 4.98, 1.07))
  expect_equal(unname(round(with_graph(as_tbl_graph(southern_women), 
                                       roctopus::centrality_betweenness(normalized = T))[28:32],4)), 
               c(1.51, 2, 2.26, 0.42, 0.42))
})

#test_that("two mode eigenvector centrality calculated correctly",{
#  expect_equal(unname(with_graph(as_tbl_graph(southern_women), 
#                                 roctopus::centrality_eigenvector())[1:5]), 
#               c(0.22, 0.2, 0.25, 0.21, 0.11))
#  expect_equal(unname(with_graph(as_tbl_graph(southern_women), 
#                                 roctopus::centrality_eigenvector())[28:32]), 
#               c(0.15, 0.07, 0.17, 0.11, 0.11))
#  expect_equal(unname(round(with_graph(as_tbl_graph(southern_women), 
#                                       roctopus::centrality_eigenvector(normalized = T))[1:5],4)), 
#               c(32.71, 30.14, 36.44, 30.49, 16.19))
#  expect_equal(unname(round(with_graph(as_tbl_graph(southern_women), 
#                                       roctopus::centrality_eigenvector(normalized = T))[28:32],4)), 
#               c(21.73, 10.03, 24.98, 15.92, 15.92))
#})
