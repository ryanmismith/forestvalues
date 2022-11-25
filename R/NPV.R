#' Net Present Value
#'
#' This function takes in any number of positive or negative values or negative
#' flows to calculate NPV as a specific discount rate.
#'
#' Run function at multiple discount rates for sensitivity analysis.
#'
#'@param Flow Value of asset or revenue/expense
#'@param Class asset or flow class use either "Expense"/"Revenue" or "Positive"/"Negative"
#'@param Occurance Years from today the revenue occurs. Today is year 0.
#'@param Discount Discount rate. Enter as .05 or .07, etc. Do not enter vector only single number.
#'@param NegativeValues if set to FALSE will change flow values to negative numbers for
#'expenses and negative assets that have not been
#'entered as negative numbers in the Flow vector. Expenses or negative values must
#'be given an Expense or Negative Class value to work. Defaults to TRUE.
#'
#'@return Net Present Value
#'
#'@family Value Functions
#'
#'@examples
#'
#' Flow <- c(-400, -100, 0, 200, 6600)
#' Class <- c("expense", "Expense", "Revenue", "Revenue", "Revenue")
#' Occurance <- c(0,5,8,15,30)
#' Discount <- .06
#' NPV(Flow, Class, Occurance, Discount)
#'
#' \dontrun{
#' Example if flows are not given negative values when they are expenses
#' Flow <- c(400, 100, 0, 200, 6600)
#' Class <- c("negative", "negative", "positive", "positive", "positive")
#' Occurance <- c(0,5,8,15,30)
#' Discount <- .06
#' NegativeValues == FALSE
#' }
#'@export

NPV <- function(Flow, Class, Occurance, Discount, NegativeValues = TRUE){


# If flow values are not indicated as positive or negative ----------------
# Change to correct flow (negative or positive) ---------------------------
# Allow inputs of Expense/Revenue or Positive/Negative --------------------

  if(NegativeValues == TRUE){
    Flow <- Flow
  } else {
    Flow <- ifelse(startsWith(Class, "E") == TRUE | startsWith(Class, "e") == TRUE, Flow*-1, Flow)
  }

  if(NegativeValues == TRUE){
    Flow <- Flow
  } else {
    Flow <- ifelse(startsWith(Class, "N") == TRUE | startsWith(Class, "n") == TRUE, Flow*-1, Flow)
  }


# Create vector for discount of length Flows ------------------------------

  Discount <- rep(Discount, length(Flow))


# Create DF for mapply ----------------------------------------------------


  ValueTable <- data.frame(Flow, Class, Occurance, Discount)


# Internal Function Call --------------------------------------------------

  ValueTable$PV <- mapply(PresentValue, ValueTable$Flow,
                          ValueTable$Discount, ValueTable$Occurance)

# Calculate, round, and return total NPV ----------------------------------
  NPV <- sum(ValueTable$PV)

  NPV <- round(NPV, 2)
  return(NPV)

}
