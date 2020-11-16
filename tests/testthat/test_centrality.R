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

