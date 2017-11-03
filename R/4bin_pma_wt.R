#' dosing nomogram based on post natal age and serum creatinine
#' @param pma post menstrual age (weeks)
#' @param wt weight (kg)
#' @details
#' DR2
#' @return dataframe with DOSE and II columns
#' @importFrom dplyr between
#' @export
dosing_4bin_pma_wt <- function(pma, wt) {
  if (pma <= 26) {
     return(data.frame(DOSE = 15*wt, II = 24))
  } else if (between(pma, 26, 34)){
     return(data.frame(DOSE = 15*wt, II = 18))
  } else if (between(pma, 34, 42)){
     return(data.frame(DOSE = 15*wt, II = 12))
  } else {
    return(data.frame(DOSE = 15*wt, II = 8))
  }
  stop("didn't found a suitable combination, should never get here")
}
