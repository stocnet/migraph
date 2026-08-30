# The attach-time check returns early when `!interactive()`, and everything else
# in R/zzz.R reaches the network, so `snet_check_versions()` is the one part a
# test can reach. It is also where the comparison happens, so it is the part
# worth covering: version 1.7.0 shipped a call to `utils::packageVersion()` on a
# version string, which stopped `library(migraph)` with
# "there is no package called '1.6.8'".

inst <- list(migraph = "1.7.0", manynet = "2.3.1",
             autograph = "1.2.2", netrics = "0.4.0")

test_that("snet_check_versions() parses version strings rather than looking them up", {
  # A version string that is not an installed package name must not error.
  expect_silent(
    out <- snet_check_versions(cran = c(migraph = "1.6.8"), gh = NULL,
                               installed = inst))
  # 1.6.8 is older than the installed 1.7.0, so nothing is reported.
  expect_length(out, 0)
})

test_that("snet_check_versions() reports a newer version", {
  out <- snet_check_versions(cran = c(migraph = "1.8.0"), gh = NULL,
                             installed = inst)
  expect_named(out, "migraph")
  expect_equal(out$migraph$version, "1.8.0")
  expect_equal(out$migraph$source, "CRAN")
})

test_that("snet_check_versions() prefers CRAN when both are newer", {
  out <- snet_check_versions(cran = c(netrics = "1.0.1"),
                             gh = c(netrics = "1.0.1"), installed = inst)
  expect_equal(out$netrics$source, "CRAN")
})

test_that("snet_check_versions() falls back to GitHub when CRAN is not ahead", {
  out <- snet_check_versions(cran = c(netrics = "0.4.0"),
                             gh = c(netrics = "1.0.1"), installed = inst)
  expect_equal(out$netrics$source, "GitHub")
  expect_equal(out$netrics$version, "1.0.1")
})

test_that("snet_check_versions() returns NULL when neither source is available", {
  expect_null(snet_check_versions(cran = NULL, gh = NULL, installed = inst))
})

test_that("snet_check_versions() skips a package that is not installed", {
  out <- snet_check_versions(cran = c(netrics = "9.9.9"), gh = NULL,
                             installed = list(migraph = "1.7.0"))
  expect_length(out, 0)
})

test_that("snet_check_versions() survives a malformed version from a repository", {
  expect_silent(
    out <- snet_check_versions(cran = c(migraph = "not-a-version"), gh = NULL,
                               installed = inst))
  expect_length(out, 0)
})

test_that("snet_report_outdated() names the right install command", {
  expect_message(
    snet_report_outdated(list(manynet = list(version = "9.9.9", source = "CRAN"))),
    "update.packages", fixed = TRUE)
  expect_message(
    snet_report_outdated(list(manynet = list(version = "9.9.9", source = "GitHub"))),
    "stocnet/manynet", fixed = TRUE)
  expect_silent(snet_report_outdated(list()))
})
