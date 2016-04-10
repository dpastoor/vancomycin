#' calculates clearance in L/hr for an individual
#' @param wt weight in kg
#' @param pma postmentstral age, weeks
#' @param scr serum creatinine mg/dl
#' @details
#' Stockmann, C. et al. Predictive Performance of a Vancomycin
#' Population Pharmacokinetic Model in Neonates.
#' Infect Dis Ther 4, 187–198 (2015).
#' @export
CLi_stockmann2015 <- function(wt, pma, scr) {
  TVCL <- 0.345 # L/hr
  MEDIAN_WT <- 2.9 #kg
  MEDIAN_PMA <- 34.8 # weeks
  return(TVCL*
           (wt/MEDIAN_WT)^0.75*
           (1/(1 + (pma/MEDIAN_PMA)^(-4.53)))*
           (1/scr)^0.267
         )
}


#' calculates Vi L for an individual
#' @param wt weight in kg
#' @details
#' Stockmann, C. et al. Predictive Performance of a Vancomycin
#' Population Pharmacokinetic Model in Neonates.
#' Infect Dis Ther 4, 187–198 (2015).
#' @export
Vi_stockmann2015 <- function(wt) {
  TVV <- 1.75 # L/hr
  MEDIAN_WT <- 2.9 #kg
  return(TVV*(wt/MEDIAN_WT))
}

