# Test read family of functions

test_that("read_edgelist works", {
  expect_equal(read_edgelist(testthat::test_path("sheets", "test.xlsx")),
               dplyr::tibble(From = c(1, 2, 2),
                             To = c(2, 1, 3),
                             Weight = c(1, 2, 3)))
  expect_equal(read_edgelist(testthat::test_path("sheets", "testCSVComma.csv"),
                             sv = "comma"),
               data.frame(From = c(1, 2, 2),
                             To = c(2, 1, 3),
                             Weight = c(1, 2, 3)))
  expect_equal(read_edgelist(testthat::test_path("sheets", "testCSVSemiColon.csv"),
                             sv = "semi-colon"),
               data.frame(From = c(1, 2, 2),
                             To = c(2, 1, 3),
                             Weight = c(1, 2, 3)))
  expect_error(read_edgelist(testthat::test_path("sheets", "testCSVSemiColon.csv"),
                             sv = "SomethingWrong"))
})

test_that("write_edgelist works", {
  file <-  tempfile() # Create file
  file2 <- tempfile() # Create file
  edgelisttest <- as_igraph(dplyr::tibble(from = c(1, 2, 2),
                                          to = c(2, 1, 3),
                                          weight = c(1, 2, 3)))
  write_edgelist(edgelisttest,
                 filename = file)
  expect_equal(read.csv(file),
               data.frame(from = c(1, 2, 2),
                             to = c(2, 1, 3),
                             weight = c(1, 2, 3)))
  write_edgelist(filename = file2)
  expect_equal(read.csv(file2), data.frame(from = c("A", "B", "C"),
                                           to = c("B", "C", "A"),
                                           weight = c(1.1, 11, 110)))
  on.exit(unlink(file)) # Unlink file
  on.exit(unlink(file2)) #Unlink file
})

test_that("read_nodelist works", {
  expect_equal(read_nodelist(testthat::test_path("sheets", "test.xlsx")),
               dplyr::tibble(From = c(1, 2, 2),
                             To = c(2, 1, 3),
                             Weight = c(1, 2, 3)))
  expect_equal(read_nodelist(testthat::test_path("sheets", "testCSVComma.csv"),
                             sv = "comma"),
               data.frame(From = c(1, 2, 2),
                          To = c(2, 1, 3),
                          Weight = c(1, 2, 3)))
  expect_equal(read_nodelist(testthat::test_path("sheets", "testCSVSemiColon.csv"),
                             sv = "semi-colon"),
               data.frame(From = c(1, 2, 2),
                          To = c(2, 1, 3),
                          Weight = c(1, 2, 3)))
  expect_error(read_nodelist(testthat::test_path("sheets", "testCSVSemiColon.csv"),
                             sv = "SomethingWrong"))
})

test_that("write_nodelist works", {
  file <-  tempfile() # Create file
  file2 <- tempfile() # Create file
  nodelisttest <- data.frame(data.frame(from = c("A", "B", "C"),
                                        to = c("B", "A", "A")))
  nodelisttest <- add_node_attribute(nodelisttest, "type", c(FALSE, FALSE, TRUE))
  write_nodelist(nodelisttest,
                 filename = file)
  expect_equal(read.csv(file),
               data.frame(name = c("A", "B", "C"),
                          type = c(FALSE, FALSE, TRUE)))
  write_nodelist(filename = file2)
  expect_equal(read.csv(file2),
               data.frame(type = c(FALSE, FALSE, TRUE),
                          name = c("A", "B", "C")))
  on.exit(unlink(file)) # Unlink file
  on.exit(unlink(file2)) # Unlink file
})

test_that("read_pajek works", {
  testpaj <- read_pajek(testthat::test_path("sheets", "SouthernWomen.paj"))
  expect_true(is.tbl_graph(testpaj))
  edgetest <- as_edgelist(testpaj)
  expect_equal(head(toupper(edgetest$from)), head(migraph::as_edgelist(ison_southern_women)$from))
})

test_that("write_pajek works", {
  file <-  tempfile() # Create file
  write_pajek(ison_southern_women, file)
  testpaj2 <- read_pajek(file)
  expect_true(is.tbl_graph(testpaj2))
  edgetest2 <- as_edgelist(testpaj2)
  # Note, the igraph::write.graph forgets names.
  expect_equal(head(edgetest2$from), as.character(rep(1, 6)))
  on.exit(unlink(file)) # Unlink file
})

test_that("read_ucinet works", {
  testuci <- read_ucinet(testthat::test_path("sheets", "ucinettest.##h"))
  expect_true(is.tbl_graph(testuci))
  expect_equal(nrow(as_edgelist(testuci)), 78)
  expect_equal(ncol(as_edgelist(testuci)), 2)
  expect_equal(node_names(testuci), NULL)
  expect_error(read_ucinet(testthat::test_path("sheets", "ucinettest")))
  expect_error(read_ucinet(testthat::test_path("sheets", "ucinettest1.##h")))
})

test_that("write_ucinet works", {
  file <-  tempfile() # Create file
  write_ucinet(ison_adolescents, file)
  testuci2 <- read_ucinet(paste0(file, ".##h"))
  expect_true(is.tbl_graph(testuci2))
  edgetest2 <- as_edgelist(testuci2)
  # Note, the write ucinet function forgets certain attributes
  expect_equal(length(edgetest2$from), length(as_edgelist(ison_adolescents)$from))
  on.exit(unlink(file)) # Unlink file
})
