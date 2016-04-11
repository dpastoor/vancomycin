#' dosing nomogram based on serum creatinine
#' @param scr serum creatinine (mg/dL)
#' @details
#' From the Red Book 2012
#' \tabular{lll}{
#' serum creatinine \tab dose \tab interdose interval
#' mg/dL   \tab mg/kg \tab hr\cr
#' < 0.7   \tab 15    \tab 12\cr
#' 0.7-0.9 \tab 20    \tab 24\cr
#' 1-1.2   \tab 15    \tab 24\cr
#' 1.3-1.6 \tab 10    \tab 24\cr
#' > 1.6   \tab 15    \tab 48
#' }
#' @return dataframe with DOSE and II columns
#' @export
dosing_5bin_scr_wt <- function(scr, wt) {
   if (scr < 0.7) {
       return(data.frame(DOSE = 15*wt, II = 12))
   } else if (between(scr, 0.7, 1)) {
     return(data.frame(DOSE = 20*wt, II = 24))
   } else if (between(scr, 1, 1.2)) {
     return(data.frame(DOSE = 15*wt, II = 24))
   } else if (between(scr, 1.2, 1.6)) {
     return(data.frame(DOSE = 10*wt, II = 24))
   } else if (scr > 1.6) {
     return(data.frame(DOSE = 15*wt, II = 48))
   } else {
     return(data.frame(DOSE = NA_real_, II = NA_real_))
  }
}
