#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

is_numeric <- function(x) {
  typeof(x) %in% c("double", "integer") && !is.factor(x)
}

between <- function(value, .min, .max, include_min = TRUE, include_max = TRUE) {
  if (include_min) {
    if (value < .min) {
      return(FALSE)
    }
  } else {
    if (value <= .min) {
      return(FALSE)
    }
  }

  if (include_max) {
    if (value > .max) {
      return(FALSE)
    }
  } else {
    if (value >= .max) {
      return(FALSE)
    }
  }

  return(TRUE)
}
