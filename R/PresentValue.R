#' Present Value
#'
#' This function calculates a single asset's value
#' at a specific time at a single discount rate.
#'
#' Run function at multiple discount rates for sensitivity analysis.
#'
#'@param Flow cash flow/asset/expense of interest.
#'@param Discount Discount rate. Enter percent as .05 or .07, etc. Do not enter vector only single number.
#'@param Year age at which you want to know flow's value.
#'
#'@return Present Value
#'
#'@family Value Functions
#'
#'@examples
#'
#'PresentValue(100000, .06, 30)
#'PresentValue(100000, .06, 20)
#'PresentValue(100000, .07, 30)
#'PresentValue(100000, .07, 20)
#'
#'@export




PresentValue <- function(Asset, Discount, Year){
  PV <- Asset/(1+Discount)^Year
  return(PV)
}
