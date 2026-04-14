test_that("predict.netlm works", {
  networkers <- ison_networkers %>% to_subgraph(Discipline == "Sociology")
  model1 <- net_regression(weight ~ ego(Citations) + alter(Citations) + sim(Citations), 
                        networkers, times = 10)
  pred <- predict(model1, matrix(c(1,10,5,2),1,4))
  expect_length(pred, 1)
  expect_type(pred, "double")
})

test_that("predict.netlogit works", {
  networkers <- ison_networkers %>% to_subgraph(Discipline == "Sociology") %>% 
    to_unweighted()
  model1 <- net_regression(. ~ ego(Citations) + alter(Citations) + sim(Citations), 
                        networkers, times = 10)
  pred_link <- predict(model1, matrix(c(1,10,5,2),1,4), type = "link")
  pred_response <- predict(model1, matrix(c(1,10,5,2),1,4), type = "response")
  
  expect_length(pred_link, 1)
  expect_type(pred_link, "double")
  
  expect_length(pred_response, 1)
  expect_type(pred_response, "double")
})