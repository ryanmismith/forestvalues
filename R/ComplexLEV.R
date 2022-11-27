


ComplexLEV <- function(Flow, Class,
                       Frequency, FirstOccurance,
                       TimeHorizon, Rate,
                       PeriodLength = NA,
                       Discount){

  Occurance <- FirstOccurance
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
  ifelse(is.numeric(Occurance) == TRUE,
         print("No Occurance Entries Missed"),
         stop("Occurance must have a numeric value equal to the year of event."))

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
  Data <- data.frame(Flow, Class, Frequency, Occurance, TimeHorizon, Rate,
                     PeriodLength)


  # Create Different Compounding Lengths based on event occurance -----------
  Data <- Data %>% mutate(CompoundingLength = TimeHorizon - Occurance)


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
  TotalAnnualCosts <- mapply(SeriesPayment, PeriodicCosts$Flow,
                             PeriodicCosts$Rate, PeriodicCosts$Frequency,
                             PeriodLength = PeriodicCosts$PeriodLength,
                             Termination = PeriodicCosts$CompoundingLength,
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
  TotalAnnualRevenues <- mapply(SeriesPayment, PeriodicRevenues$Flow,
                                PeriodicRevenues$Rate, PeriodicRevenues$Frequency,
                                PeriodLength = PeriodicRevenues$PeriodLength,
                                Termination = PeriodicRevenues$CompoundingLength,
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

  LEV <- LandExpectVal(NetFlow, Horizon, DiscountVal)
  LEV <- round(LEV,2)
  return(LEV)
}
