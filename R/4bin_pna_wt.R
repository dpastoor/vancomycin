#' dosing nomogram based on post natal age and serum creatinine
#' @param pna post natal age (days)
#' @param wt weight (kg)
#' @details 
#' as used in publication Madigan, Theresa et al. "Optimization of Vancomycin 
#' Dosing in Very Low-Birth-Weight Preterm Neonates." 
#' American journal of perinatology 32.1 (2015): 83–86. PMC. Web. 27 Mar. 2016.
#' @return dataframe with DOSE and II columns 
#' @export
dosing_4bin_pna_wt <- function(pna, wt) {
  if (pna < 7) {
    if(wt < 1.3) {
     return(data.frame(DOSE = 15*wt, II = 24))
    } else {
     return(data.frame(DOSE = 15*wt, II = 18))
    }
  } else {
    if(wt < 1.3) {
     return(data.frame(DOSE = 15*wt, II = 12))
    } else {
     return(data.frame(DOSE = 15*wt, II = 8))
    }
  }
}
