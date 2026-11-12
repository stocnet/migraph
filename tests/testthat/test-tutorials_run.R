test_that("run_tute() runs without error when missing argument", {
  skip_if_not_installed("learnr")
  expect_error(run_tute(), NA)
})

test_that("extract_tute() runs without error when missing argument", {
  skip_if_not_installed("learnr")
  expect_error(extract_tute(), NA)
})
test_that("search_tute() finds a keyword and returns a tutorial name", {
  res <- search_tute("RStudio")
  expect_s3_class(res, "migraphSearch")
  expect_length(res, 1)
  matches <- attr(res, "matches")
  expect_named(matches, c("package", "name", "title", "section", "type", "text"))
  expect_true("tutorial0" %in% matches$name)
  expect_true(as.character(res) %in% matches$name)
  expect_equal(attr(res, "package"),
               matches$package[match(as.character(res), matches$name)])
  expect_output(print(res), "tutorial0")
})

test_that("search_tute() finds function calls in code chunks", {
  res <- attr(search_tute("print()"), "matches")
  here <- res[res$name == "tutorial0", ]
  expect_gt(nrow(here), 0)
  expect_true(all(grepl("print\\(", here$text)))
  expect_true("exercise" %in% here$type)
})

test_that("search_tute() returns nothing when nothing matches", {
  res <- search_tute("zzqqxxnotaword")
  expect_s3_class(res, "migraphSearch")
  expect_length(res, 0)
  expect_equal(nrow(attr(res, "matches")), 0)
  expect_error(run_tute(res), "no tutorial")
})

test_that("search_tute() needs one search term", {
  expect_error(search_tute())
  expect_error(search_tute(c("a", "b")))
  expect_error(search_tute(""))
})
