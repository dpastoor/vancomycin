#' dosing nomogram based on post natal age and serum creatinine
#' @param pna post natal age (days)
#' @param scr serum creatinine (mg/dL)
#' @param wt weight (kg)
#' @details
#' as used in publications Anderson et al, 2007; Allegaert et al., 2007;
#' kimura et al., 2004 -
#' PNA 1-7 days, serum creatinine > 71 umol/L: 15 mg/kg every 12 hrs;
#' PNA > 7 days: 15 mg/kg every 8 hrs
#' @return dataframe with DOSE and II columns
#' @export
dosing_2bin_pna_scr_wt <- function(pna, scr, wt) {
   if (pna > 1 & pna <= 7 & scr > 71/88.4) {
       return(data.frame(DOSE = 15*wt, II = 12))
   } else if (pna > 7 & scr > 71/88.4) {
     return(data.frame(DOSE = 15*wt, II = 8))
   } else {
  # covers pna < 1 or scr < 71 as this algorithm did not have a suggestion for those
     # explicit NA_real_ as for purrr rowwise need to have matching types
     return(data.frame(DOSE = NA_real_, II = NA_real_))
  }
}

