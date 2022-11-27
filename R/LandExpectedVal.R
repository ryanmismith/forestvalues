#' Land Expected Value (Multiple Rotations)
#'
#' Use this function to estimate the simple LEV
#'
#'@param Age The age where LEV is of interest
#'@param FutureValue Total Future Value (Use SinglePayment and SeriesPayment Functions)
#'@param Discount The discount rate (ex: .05)
#'
#'@return Land Expected Value for a single year when the stand has multiple rotations.
#'
#'@family Rotation Age
#'
#'@examples
#'
#' Age <- 40
#' FutureValue <- 1960
#' Discount <- .06
#'
#' LandExpectVal(Age, FutureValue, Discount)
#'@export

LandExpectVal <- function(FutureValue, Age, Discount){

  val <- FutureValue/((1+Discount)^Age - 1)
  return(val)

}



