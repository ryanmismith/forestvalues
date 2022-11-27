#' The present or future value of a series of payments.
#'
#'@description
#' The following six possible series payments can be calculated:
#'
#'@description
#'\itemize{
#'\item Series Annual Forever Present Value:
#'\item Frequency = "Annual", Termination = NA, Present = TRUE }
#'
#'@description
#'\itemize{
#' \item Series Annual Terminating Present Value:
#' \item Frequency = "Annual", Termination = Numeric(years), Present = TRUE }
#'
#' @description
#' \itemize{
#' \item Series Annual Terminating Future Value:
#' \item Frequency = "Annual", Termination = numeric(years), Present = FALSE }
#'
#'@description
#'  \itemize{
#' \item Series Periodic Forever Present Value:
#' \item Frequency = "Periodic", PeriodLength = numeric(years.between.periods),
#' Termination = NA, Present = TRUE }
#'
#'@description
#' \itemize{
#' \item Series Periodic Terminating Present Value:
#' \item Frequency = "Periodic", PeriodLength = numeric(years.between.periods),
#' Termination = numeric(years), Present = TRUE }
#'
#'@description
#' \itemize{
#' \item Series Periodic Terminating Future Value:
#' \item  Frequency = "Periodic", PeriodLength = numeric(years.between.periods),
#' Termination = numeric(years), Present = FALSE }
#'
#'@family Payment Functions
#'
#'@param Flow Value of asset or revenue/expense.
#'@param Rate either the discount rate (if calculating present value) or interest rate (
#'for value using compounding interest)
#'@param Frequency "Annual" or "Periodic" payments. Defaults to "Annual"
#'@param PeriodLength NA or Numeric. Numeric years between periods for "Periodic" payments. NA for annual payments.
#'@param Termination Time horizon in years. NA for 'forever' or non-terminating payments.
#'@param Present TRUE or FALSE. TRUE will calculate the present value of payment made at year Time
#'and FALSE the future value of a payment with compounding interest in Time years.
#'
#'
#'
#'@examples
#'
#'SeriesPayment(100, .05, "Annual", NA, NA, TRUE)
#'SeriesPayment(100, .05, "Annual", NA, Termination = 30, TRUE)
#'SeriesPayment(100, .05, "Annual", NA, Termination = 30, FALSE)
#'SeriesPayment(100, .05, "Periodic", PeriodLength = 5, Termination = NA, TRUE)
#'SeriesPayment(100, .05, "Periodic", PeriodLength = 5, Termination = 30, TRUE)
#'SeriesPayment(100, .05, "Periodic", PeriodLength = 5, Termination = 30, FALSE)
#'
#'@export


SeriesPayment <- function(Flow,
                          Rate = .06,
                          Frequency = "Annual",
                          PeriodLength = NA,
                          Termination = NA,
                          Present = TRUE){

  data <- data.frame(Flow, Rate, Frequency, Termination, Present, PeriodLength)

  for(i in length(data$Flow)){
    if(data$Frequency[i] == "Annual"){
      if(is.na(data$Termination[i]) == TRUE){
        data$val[i] <- data$Flow[i]/data$Rate[i]
       } else {
        if(data$Present[i] == FALSE){
          data$val[i] <- (data$Flow[i]*((((1+data$Rate[i])^data$Termination[i]))-1))/data$Rate[i]
        } else {
          data$val[i] <- (data$Flow[i]*((((1+data$Rate[i])^data$Termination[i]))-1))/
            (data$Rate[i]*((1+data$Rate[i])^data$Termination[i]))
        }
      }
    } else {
   if(data$Frequency[i] == "Periodic"){
     if(is.na(data$Termination[i]) == TRUE){
       data$val[i] <- data$Flow[i]/((1+data$Rate[i])^data$PeriodLength[i]-1)
     } else {
       if(data$Present[i] == FALSE){
         data$val[i] <- (data$Flow[i]*((1+data$Rate[i])^data$Termination[i]-1))/
           ((1+data$Rate[i])^data$PeriodLength[i]-1)
       } else {
         data$val[i] <- (data$Flow[i]*((1+data$Rate[i])^data$Termination[i]-1))/
           (((1+data$Rate[i])^data$PeriodLength[i]-1)*((1+data$Rate[i])^data$Termination[i]))
       }
      }
     }
    }
  }
  return(round(data$val,2))
}



