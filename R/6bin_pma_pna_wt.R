#' dosing nomogram based on post natal age and serum creatinine
#' @param pma post menstrual age (weeks)
#' @param pna post natal age (days)
#' @param wt weight (kg)
#' @details
#' DR3
#' @return dataframe with DOSE and II columns
#' @importFrom dplyr between
#' @export
dosing_6bin_pma_pna <- function(pma, pna, wt) {
  if (pma < 29) {
    if(pna <= 14) {
     return(data.frame(DOSE = 10*wt, II = 18))
    } else {
     return(data.frame(DOSE = 10*wt, II = 12))
    }
  } else if (between(pma, 29, 36)){
    if(pna <= 14) {
     return(data.frame(DOSE = 10*wt, II = 12))
    } else {
     return(data.frame(DOSE = 10*wt, II = 8))
    }
  } else if (between(pma, 36, 44)){
    if(pna <= 7) {
     return(data.frame(DOSE = 10*wt, II = 12))
    } else {
     return(data.frame(DOSE = 10*wt, II = 8))
    }
  } else {
    return(data.frame(DOSE = 10*wt, II = 6))
  }
  stop("didn't found a suitable combination, should never get here")
}
