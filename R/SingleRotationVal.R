#' Single Rotation Value (Multiple Rotations)
#'
#' Use this function to estimate the simple single rotation value.
#'
#'@param Age The age where NPV is of interest
#'@param Stumpage the stumpage value in a given year
#'@param Volume The yield in a given year
#'@param Cost The total cost expanded to a given year cost (ie: regeneration costs or property taxes)
#'@param Discount The discount rate (ex: .05)
#'
#'@return The NPV at a single age for a stand when deciding optimal rotation.
#'
#'@family Rotation Age
#'
#'@examples
#' Age <- 40
#' Stumpage <- 300
#' Volume <- 20
#' Cost <- 100
#' Discount <- .06
#'
#' SingleRotationVal(75, 150, 30, 100, .06)
#'
#'
#'@export


SingleRotationVal <- function(Age, Stumpage, Volume, Cost, Discount){

  val <- ((Volume*Stumpage) - Cost*((1+Discount)^Age))/((1+Discount)^Age)
  return(val)

}
