context("stockmann2015")

test_that("Cli and Vi params calculated properly", {
  expect_equal(round(CLi_stockmann2015(2.9, 35, 0.2), 4), 0.2685)
  expect_equal(Vi_stockmann2015(2.9), 1.75)
})
