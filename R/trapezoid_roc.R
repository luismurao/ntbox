#' Trapezoidal integration for Partial ROC curve
#' @param fract_area fractional area predicted presence
#' @param sensibility sensibility at threshold (fract_area value).
#' @description trapozoid_roc applies the trapezoidal integration for Partial ROC curve.
#' @return Returns the area under the Partial ROC curve.
#' @import Rcpp
#' @useDynLib ntbox
#' @export

trapozoid_roc <- function(fract_area,sensibility){
  return(trap_roc(fract_area,sensibility))
}
