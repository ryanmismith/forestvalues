#' Complex Net Present Value
#'
#' Calculates NPV for complex management scenarios with mixed payment types
#' (single, annual, periodic) and frequencies. Compounds all cash flows to the
#' time horizon, then discounts the net flow to present value.
#'
#' This function handles scenarios where a forest management plan includes
#' one-time costs (planting, PCT), recurring annual costs (property tax),
#' and periodic costs/revenues (validation inventories, thinning cycles).
#'
#' @param Flow Numeric vector. Cash flow amounts (all positive; sign determined by Class).
#' @param Class Character vector. "Revenue"/"Expense" or "Positive"/"Negative" for each flow.
#' @param Frequency Character vector. "Single", "Annual", or "Periodic" for each flow.
#' @param FirstOccurrence Numeric vector. Year each flow first occurs (0 = today).
#' @param TimeHorizon Numeric scalar. Total analysis period in years.
#' @param Rate Numeric vector. Interest/discount rate for compounding each flow.
#' @param PeriodLength Numeric vector. Years between periodic payments (NA for non-periodic).
#' @param Discount Numeric scalar. Discount rate for final NPV calculation.
#'
#' @return Numeric scalar. The net present value of the management scenario.
#'
#' @family Value Functions
#'
#' @references
#' Klemperer, W.D. (1996). *Forest Resource Economics and Finance*. McGraw-Hill.
#'
#' Bettinger, P., Boston, K., Siry, J.P., & Grebner, D.L. (2017).
#' *Forest Management and Planning*. 2nd ed. Academic Press.
#'
#' @examples
#' # Carbon credit scenario: planting, carbon payment, taxes, validation, harvest
#' ComplexNPV(
#'   Flow = c(750, 4000, 50, 500, 750, 2000),
#'   Class = c("Expense", "Revenue", "Expense", "Expense", "Expense", "Revenue"),
#'   Frequency = c("Single", "Single", "Annual", "Periodic", "Single", "Single"),
#'   FirstOccurrence = c(0, 0, 0, 10, 15, 40),
#'   TimeHorizon = 40,
#'   Rate = c(0.06, 0.06, 0.06, 0.06, 0.06, 0.06),
#'   PeriodLength = c(NA, NA, NA, 10, NA, NA),
#'   Discount = 0.06
#' )
#'
#' @export


ComplexNPV <- function(Flow, Class,
                       Frequency, FirstOccurrence,
                       TimeHorizon, Rate,
                       PeriodLength = NA,
                       Discount){

  Occurrence <- FirstOccurrence
# Add checks to catch data entry errors -----------------------------------

  # Flow Variable
  ifelse(is.numeric(Flow) == TRUE,
         print("No Flow Entries Missed"),
         stop("All Cash Flows and Assets must have a numeric value."))

  # Make Sure Flow Variables are not Negative (Costs subtracted later)
  ifelse(Flow > 0,
         print("All Flows Properly Formatted"),
         stop("Please enter all flows as positive values,
         costs will be deducted when labeled as class 'Negative' or 'Cost'"))

  # Class Variables Valid for Filtering Later
  ifelse(Class %in% c("Revenue", "Expense", "Positive", "Negative"),
         print("Class Okay"),
         stop("Classes must be either
              'Revenue/Expense' or 'Positive/Negative'"))

  # Frequencies Valid For Filtering Later / SeriesPayment Functions
  ifelse(Frequency %in% c("Single", "Periodic", "Annual"),
         print("Frequency Okay"),
         stop("Unknown Types or Errors in Frequency Input.
               Must be 'Single', 'Periodic', or 'Annual'"))

  # Make Sure first occurance values are entered
  ifelse(is.numeric(Occurrence) == TRUE,
      print("No Occurrence Entries Missed"),
      stop("Occurrence must have a numeric value equal to the year of event."))

  # Only One time horizon can be run at a time (mapply over list for multiple)
  ifelse(length(TimeHorizon) == 1,
         print(paste("Single Time Horizon set at length",
                     TimeHorizon, sep = " ")),
         stop("Please enter a single time horizon and not a list or
              vector of values. ex: TimeHorizon = 30"))

   # Make sure rate is formatted correctly
   ifelse(Rate <= .25,
         print(paste("Rate is set at", Rate, sep = " ")),
         stop("Rate is set above 25%"))

    # Ensure there are no weird values for Period Length
    PeriodLength <- ifelse(Frequency %in% c("Single", "Annual"),
                           NA, PeriodLength)

    # Apply Time Horizon to each flow for functions
    TimeHorizon <- rep(TimeHorizon, length(Flow))
    TimeHorizon <- TimeHorizon[1:length(Flow)]
# Create Dataframe --------------------------------------------------------
     Data <- data.frame(Flow, Class, Frequency, Occurrence, TimeHorizon, Rate,
                     PeriodLength)


# Create Different Compounding Lengths based on event occurance -----------
  Data <- Data %>% mutate(CompoundingLength = TimeHorizon - Occurrence)


# Filter All Events Into Like Classes ----------------------------------
  Costs <- Data %>% filter(Class %in% c("Negative", "Expense"))
  Revenues <- Data %>% filter(Class %in% c("Positive", "Revenue"))


# Process Costs -----------------------------------------------------------
  SingleCosts <- Costs %>% filter(Frequency == "Single")
  PeriodicCosts <- Costs %>% filter(Frequency == "Periodic")
  AnnualCosts <- Costs %>% filter(Frequency == "Annual")

  # One Time Costs
  TotalSingleCosts <- mapply(SinglePayment, SingleCosts$Flow, SingleCosts$Rate,
                        SingleCosts$CompoundingLength, Present = FALSE)
  # Sum and Allow mathematical operations when length(list) = 0
  TotalSingleCosts <- ifelse(length(TotalSingleCosts) == 0, 0,
                             TotalSingleCosts)
  TotalSingleCosts <- sum(TotalSingleCosts)

  # Periodic Costs
  TotalPeriodicCosts <- mapply(SeriesPayment, PeriodicCosts$Flow,
                               PeriodicCosts$Rate, PeriodicCosts$Frequency,
                               PeriodLength = PeriodicCosts$PeriodLength,
                               Termination = PeriodicCosts$CompoundingLength,
                               Present = FALSE)
  # Sum and Allow mathematical operations when length(list) = 0
  TotalPeriodicCosts <- ifelse(length(TotalPeriodicCosts) == 0, 0,
                               TotalPeriodicCosts)
  TotalPeriodicCosts <- sum(TotalPeriodicCosts)

  #Annual Costs
  TotalAnnualCosts <- mapply(SeriesPayment, AnnualCosts$Flow,
                         AnnualCosts$Rate, AnnualCosts$Frequency,
                         PeriodLength = AnnualCosts$PeriodLength,
                         Termination = AnnualCosts$CompoundingLength,
                         Present = FALSE)
  # Sum and Allow mathematical operations when length(list) = 0
  TotalAnnualCosts <- ifelse(length(TotalAnnualCosts) == 0, 0,
                             TotalAnnualCosts)
  TotalAnnualCosts <- sum(TotalAnnualCosts)

# Sum all Costs --------------------------------------------------------

  TotalCosts <- TotalSingleCosts + TotalPeriodicCosts + TotalAnnualCosts

# Clean Environment -------------------------------------------------------
  rm(SingleCosts, TotalSingleCosts, PeriodicCosts, TotalPeriodicCosts,
     AnnualCosts, TotalAnnualCosts)

# Process Revenues -----------------------------------------------------------
  SingleRevenues <- Revenues %>% filter(Frequency == "Single")
  PeriodicRevenues <- Revenues %>% filter(Frequency == "Periodic")
  AnnualRevenues <- Revenues %>% filter(Frequency == "Annual")


# Single Payment Revenues -------------------------------------------------
  TotalSingleRevenues <- mapply(SinglePayment, SingleRevenues$Flow,
                                SingleRevenues$Rate,
                             SingleRevenues$CompoundingLength,
                             Present = FALSE)
  # Sum and Allow mathematical operations when length(list) = 0
  TotalSingleRevenues <- ifelse(length(TotalSingleRevenues) == 0, 0,
                                TotalSingleRevenues)
  TotalSingleRevenues <- sum(TotalSingleRevenues)

# Periodic Revenues -------------------------------------------------------

  TotalPeriodicRevenues <- mapply(SeriesPayment, PeriodicRevenues$Flow,
                               PeriodicRevenues$Rate,
                               PeriodicRevenues$Frequency,
                               PeriodLength = PeriodicRevenues$PeriodLength,
                               Termination = PeriodicRevenues$CompoundingLength,
                               Present = FALSE)
  # Sum and Allow mathematical operations when length(list) = 0
  TotalPeriodicRevenues <- ifelse(length(TotalPeriodicRevenues) == 0, 0,
                                  TotalPeriodicRevenues)


# Annual Revenues ---------------------------------------------------------
  TotalAnnualRevenues <- mapply(SeriesPayment, AnnualRevenues$Flow,
                             AnnualRevenues$Rate, AnnualRevenues$Frequency,
                             PeriodLength = AnnualRevenues$PeriodLength,
                             Termination = AnnualRevenues$CompoundingLength,
                             Present = FALSE)
  # Sum and Allow mathematical operations when length(list) = 0
  TotalAnnualRevenues <- ifelse(length(TotalAnnualRevenues) == 0, 0,
                                TotalAnnualRevenues)
  TotalAnnualRevenues <- sum(TotalAnnualRevenues)


# Sum all Revenues --------------------------------------------------------
  TotalRevenues <- TotalSingleRevenues + TotalPeriodicRevenues + TotalAnnualRevenues


# Clean Enviornment -------------------------------------------------------
  rm(SingleRevenues, TotalSingleRevenues, PeriodicRevenues, TotalPeriodicRevenues,
     AnnualRevenues, TotalAnnualRevenues)

# Prepare for NPV Calculation ---------------------------------------------

  Horizon <- max(Data$TimeHorizon) # Single Horizon Value

  NetFlow <- TotalRevenues-TotalCosts # Single Flow Value

  DiscountVal <- max(Discount) # Apply Universal Discount to Total Flow

  NPV <- SimpleNPV(NetFlow, Class = "Revenue", Horizon, DiscountVal)
  return(sum(NPV))
}
