context("defaults")

x <- snapdef()

test_that("snap defaults are as expected", {
  expect_is(x, "list")
  expect_equal(length(x$ar5dir_dist_stats), 2)
  expect_equal(x$ar5cru, x$ar5all[6])
  expect_equal(length(x), 21)
})
