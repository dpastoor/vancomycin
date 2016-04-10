#' calculates clearance in L/hr for an individual
#' @param wt weight in kg
#' @param pma postmentstral age, weeks
#' @param scr serum creatinine mg/dl
#' @details
#' Mehrotra, N., Tang, L., Phelps, S. J. & Meibohm, B. Evaluation of vancomycin
#' dosing regimens in preterm and term neonates using Monte Carlo simulations.
#' Pharmacotherapy 32, 408–419 (2012)
#' @export
CLi_mehrotra <- function(wt, pma, scr) {
  TVCL <- 0.18 # L/hr
  NORM_WT <- 2.5 #kg
  NORM_PMA <- 37 # weeks
  return(TVCL*
           (wt/NORM_WT)^0.75*
           # publication did not mention how the scr value of 0.42 was obtained
           (0.42/scr)^0.7*
           (pma/NORM_PMA)^1.4
         )
}


#' calculates Vi L for an individual
#' @param wt weight in kg
#' @details
#' Mehrotra, N., Tang, L., Phelps, S. J. & Meibohm, B. Evaluation of vancomycin
#' dosing regimens in preterm and term neonates using Monte Carlo simulations.
#' Pharmacotherapy 32, 408–419 (2012)
#' @export
Vi_mehrotra <- function(wt) {
  TVV <- 1.75 # L/hr
  NORM_WT <- 2.5 #kg
  return(TVV*(wt/NORM_WT))
}

