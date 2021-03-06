context("5bin_scr_wt")

starting_df <- data.frame(ID=1:5, WT = 2, SCR = c(0.6, 0.75, 1.1, 1.4, 1.7))


test_that("algorithm works with table test", {
  result_df <- starting_df %>%
    mutate(regimen = map2(SCR, WT, dosing_5bin_scr_wt)) %>%
    unnest()

  expect_equal(result_df$DOSE, c(30, 40, 30, 20, 30))
  expect_equal(result_df$II, c(12, 24, 24, 24, 48))
})

