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
