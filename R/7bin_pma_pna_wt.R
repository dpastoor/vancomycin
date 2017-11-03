#' dosing nomogram based on post natal age and serum creatinine
#' @param pma post menstrual age (weeks)
#' @param pna post natal age (days)
#' @param wt weight (kg)
#' @details
#' Rajon et al 2017
#' @return dataframe with DOSE and II columns
#' @importFrom dplyr between
#' @export
dosing_7bin_pma_pna_wt <- function(pma, pna, wt) {
  if (pna < 14) {
    if(pma < 28) {
     return(data.frame(DOSE = 20*wt, II = 24))
    } else if (between(pma, 28, 35)) {
     return(data.frame(DOSE = 20*wt, II = 18))
    } else {
     return(data.frame(DOSE = 20*wt, II = 12))
    }
  } else {
    if(pma < 28) {
      return(data.frame(DOSE = 20*wt, II = 12))
    } else if (between(pma, 28, 35)) {
      return(data.frame(DOSE = 20*wt, II = 12))
    } else {
      return(data.frame(DOSE = 20*wt, II = 8))
    }
  }
  stop("didn't found a suitable combination, should never get here")
}
