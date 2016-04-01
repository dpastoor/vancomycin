context("6bin_pna_wt")



starting_df <- data.frame(ID=1:6, WT = c(1.1, 1.1, 1.9, 1.9, 2.2, 2.2), AGE=c(6, 8, 6, 8, 6, 8))


result_df <- by_row(starting_df, function(row) {
  dosing_6bin_pna_wt(row$AGE,row$WT)
}, .collate="rows")

test_that("algorithm works with table test", {
  expect_equal(result_df$DOSE, c(16.5, 16.5, 28.5, 28.5, 33, 33))
  expect_equal(result_df$II, c(24, 24, 18, 12, 12, 8))
})

