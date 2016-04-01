#' dosing nomogram based on post natal age and serum creatinine
#' @param pna post natal age (days)
#' @param wt weight (kg)
#' @details 
#' as used in publication Dersch-Mills,et al.,
#'  Assessment of initial vancomycin dosing in neonates. 
#'  Paediatr Child Health 19, e30–e34 (2014).
#'
#' recommended algorithm from Nelson's textbook of Pediatrics 2007
#' @return dataframe with DOSE and II columns 
#' @export
dosing_6bin_pna_wt <- function(pna, wt) {
  if (pna < 7) {
    if(wt < 1.2) {
     return(data.frame(DOSE = 15*wt, II = 24))
    } else if (wt >= 1.2 & wt <= 2) {
     return(data.frame(DOSE = 15*wt, II = 18))
    } else {
     return(data.frame(DOSE = 15*wt, II = 12))
    }
  } else {
    if(wt < 1.2) {
     return(data.frame(DOSE = 15*wt, II = 24))
    } else if (wt >= 1.2 & wt <= 2) {
     return(data.frame(DOSE = 15*wt, II = 12))
    } else {
     return(data.frame(DOSE = 15*wt, II = 8))
    }
  }
}
