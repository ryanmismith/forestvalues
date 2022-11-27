#' Single Payment Future of Present Value
#'
#' This function calculates either the present value of a future cash flow or the
#' compounded future value of a cash flow after n years.
#'
#' Must be fed a matrix containing Rate, Time, and Present values for all payments.
#'
#'@param Flow Value of asset or revenue/expense.
#'@param Rate either the discount rate (if calculating present value) or interest rate (
#'for value using compounding interest)
#'@param Time Time horizon in years.
#'@param Present TRUE or FALSE. TRUE will calculate the present value of payment made at year Time
#'and FALSE the future value of a payment with compounding interest in Time years.
#'
#'
#'
#'@return Vector of length Flow
#'
#'@family Payment Functions
#'
#'@examples
#'
#'Flow <- c(rep(c(500,600,700,800,900),2))
#'Rate <- rep(.06, 10)
#'Time <- rep(30, 10)
#'Present <- c(rep(TRUE,5), rep(FALSE,5))
#'SinglePayment(Flow, Rate, Time, Present)
#'SinglePayment(6000, .05, 40, TRUE)
#'SinglePayment(6000, .05, 40, FALSE)
#'SinglePayment(6000, .06, 40, FALSE)
#'
#'@export

  SinglePayment <- function(Flow, Rate, Time, Present = TRUE){



      # Simple Discounting
      discount <- function(Flow, Rate, Time){
           Val <- Flow/((1+Rate)^Time)
           return(Val)
       }

      # Simple Compounding
      compounding <- function(Flow, Rate, Time){
        Flow*((1+Rate)^Time)
      }

      Val <- ifelse(Present == TRUE,
             discount(Flow, Rate, Time),
             compounding(Flow, Rate, Time))

      Val <- round(Val, 2)
      return(Val)
    }
