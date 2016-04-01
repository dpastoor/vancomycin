context("2bin_pna_scr_wt")


test_that("2 bin PNA SCR WT algorithm works", {
  expect_equal(dosing_2bin_pna_scr_wt(6, 79, 1.9), data.frame(DOSE = 28.5, II = 12))
  expect_equal(dosing_2bin_pna_scr_wt(8, 79, 1.9), data.frame(DOSE = 28.5, II = 8))
  expect_equal(dosing_2bin_pna_scr_wt(6, 70, 1.9), data.frame(DOSE = NA_real_, II = NA_real_))
  expect_equal(dosing_2bin_pna_scr_wt(8, 70, 1.9), data.frame(DOSE = NA_real_, II = NA_real_))
})


starting_df <- data.frame(ID=1:4, WT=1.9, SCR = c(79, 79, 70, 70), AGE=c(6, 8, 6, 8))


result_purrr <- by_row(starting_df, function(row) {
  dosing_2bin_pna_scr_wt(row$AGE, row$SCR, row$WT)
}, .collate="rows")

result_dplyr <- do(group_by(starting_df, ID, WT, SCR, AGE), dosing_2bin_pna_scr_wt(.$AGE, .$SCR, .$WT) )

test_that("2 bin PNA SCR WT algorithm in dataframe context with purrr::by_row", {
  expect_equal(result_dplyr$DOSE, c(28.5, 28.5, NA, NA))
  expect_equal(result_dplyr$II, c(12, 8, NA, NA))
  expect_equal(result_purrr$DOSE, c(28.5, 28.5, NA, NA))
  expect_equal(result_purrr$II, c(12, 8, NA, NA))
})

